use pest::{error::Error as PestError, iterators::Pair, Parser};
use pest_derive::Parser;
use photon_rs::{
    multiple::blend,
    native::{open_image, save_image},
    transform, PhotonImage,
};
use std::{collections::HashMap, fmt};

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct IMGParser;

#[derive(Debug, Clone)]
pub enum FlipType {
    H,
    V,
}

#[derive(Debug, Clone)]
pub enum AstNode {
    Print(Box<AstNode>),
    Open(Box<(AstNode, AstNode)>),
    Resize(Box<(AstNode, AstNode, AstNode, AstNode, AstNode)>),
    Output(Box<AstNode>),
    Save(Box<(AstNode, AstNode)>),
    SetVar(Box<(AstNode, AstNode)>),
    GetAttr(Box<(AstNode, AstNode)>),
    Flip(FlipType, Box<AstNode>),
    Blend(Box<(AstNode, AstNode, AstNode)>),
    Variable(String),

    Value(VariableValue),
}

fn build_ast_from_expr(pair: Pair<Rule>) -> AstNode {
    match pair.as_rule() {
        Rule::expr => build_ast_from_expr(pair.into_inner().next().unwrap()),
        Rule::OpenExpr => {
            let mut pair = pair.into_inner();
            AstNode::Open(Box::new((
                build_ast_from_expr(pair.next().unwrap()),
                build_ast_from_expr(pair.next().unwrap()),
            )))
        }
        Rule::OutputExpr => AstNode::Output(Box::new(build_ast_from_expr(
            pair.into_inner().next().unwrap(),
        ))),
        Rule::PrintExpr => {
            let mut pair = pair.into_inner();
            AstNode::Print(Box::new(build_ast_from_expr(pair.next().unwrap())))
        }
        Rule::ResizeExpr => {
            let mut pair = pair.into_inner();
            AstNode::Resize(Box::new((
                build_ast_from_expr(pair.next().unwrap()),
                build_ast_from_expr(pair.next().unwrap().into_inner().next().unwrap()),
                build_ast_from_expr(pair.next().unwrap().into_inner().next().unwrap()),
                build_ast_from_expr(pair.next().unwrap()),
                build_ast_from_expr(pair.next().unwrap()),
            )))
        }
        Rule::SaveExpr => {
            let mut pair = pair.into_inner();
            AstNode::Save(Box::new((
                build_ast_from_expr(pair.next().unwrap()),
                build_ast_from_expr(pair.next().unwrap()),
            )))
        }
        Rule::SetVarExpr => {
            let mut pair = pair.into_inner();
            AstNode::SetVar(Box::new((
                build_ast_from_expr(pair.next().unwrap()),
                build_ast_from_expr(pair.next().unwrap()),
            )))
        }
        Rule::FlipVExpr => AstNode::Flip(
            FlipType::V,
            Box::new(build_ast_from_expr(pair.into_inner().next().unwrap())),
        ),
        Rule::FlipHExpr => AstNode::Flip(
            FlipType::H,
            Box::new(build_ast_from_expr(pair.into_inner().next().unwrap())),
        ),
        Rule::BlendExpr => {
            let mut pair = pair.into_inner();
            AstNode::Blend(Box::new((
                build_ast_from_expr(pair.next().unwrap()),
                build_ast_from_expr(pair.next().unwrap()),
                build_ast_from_expr(pair.next().unwrap()),
            )))
        }
        Rule::getattr => {
            let mut pair = pair.into_inner();
            let var = build_ast_from_expr(pair.next().unwrap());
            let attr = pair.next().unwrap().as_span().as_str().to_string();
            AstNode::GetAttr(Box::new((var, AstNode::Value(VariableValue::String(attr)))))
        }
        Rule::string => {
            let mut chars = pair.as_span().as_str().chars();
            chars.next();
            chars.next_back();

            AstNode::Value(VariableValue::String(chars.as_str().to_string()))
        }
        Rule::variable => AstNode::Variable(pair.as_span().as_str().to_string()),
        Rule::hex => AstNode::Value(VariableValue::Integer(
            i64::from_str_radix(pair.as_span().as_str().trim_start_matches("#"), 16).unwrap(),
        )),
        Rule::int => AstNode::Value(VariableValue::Integer(
            pair.as_span().as_str().parse::<i64>().unwrap(),
        )),
        Rule::value | Rule::_value => build_ast_from_expr(pair.into_inner().next().unwrap()),
        unknown => {
            panic!("{:?}", unknown)
        }
    }
}

pub fn parse(code: &str) -> Result<Vec<AstNode>, PestError<Rule>> {
    let pairs = IMGParser::parse(Rule::program, code)?;
    let mut ast = vec![];

    for pair in pairs {
        match pair.as_rule() {
            Rule::expr => {
                ast.push(build_ast_from_expr(pair));
            }
            _ => {}
        }
    }
    Ok(ast)
}

#[derive(Debug, Clone)]
pub struct Image {
    image: PhotonImage,
    name: String,
}

impl Image {
    fn new(name: String) -> RuntimeResult<Self> {
        let image = open_image(name.as_str()).map_err(|_| Error::NoFileFound(name.clone()))?;
        Ok(Image { image, name })
    }
}

impl fmt::Display for Image {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<image: {}>", self.name)
    }
}

#[derive(Debug, Clone)]
pub enum VariableValue {
    String(String),
    Integer(i64),
    Image(Image),
}

impl VariableValue {
    fn get_attr(&self, name: &String) -> RuntimeResult<VariableValue> {
        match self {
            VariableValue::String(_) => Err(Error::NoAttribute("string".into(), name.clone())),
            VariableValue::Integer(_) => Err(Error::NoAttribute("integer".into(), name.clone())),
            VariableValue::Image(image) => match name.as_str() {
                "width" => Ok(VariableValue::Integer(image.image.get_width() as i64)),
                "height" => Ok(VariableValue::Integer(image.image.get_height() as i64)),
                _ => Err(Error::NoAttribute("image".into(), name.clone())),
            },
        }
    }
}

impl fmt::Display for VariableValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VariableValue::String(v) => write!(f, "\"{}\"", v),
            VariableValue::Integer(v) => write!(f, "{}", v),
            VariableValue::Image(v) => write!(f, "{}", v),
        }
    }
}

fn expect_string(value: &VariableValue) -> RuntimeResult<&str> {
    match value {
        VariableValue::String(v) => Ok(v),
        _ => Err(Error::InvalidArgType(ArgType::String)),
    }
}

fn expect_integer(value: &VariableValue) -> RuntimeResult<i64> {
    match value {
        VariableValue::Integer(v) => Ok(*v),
        _ => Err(Error::InvalidArgType(ArgType::Integer)),
    }
}

fn expect_image(value: VariableValue) -> RuntimeResult<Image> {
    match value {
        VariableValue::Image(v) => Ok(v),
        _ => Err(Error::InvalidArgType(ArgType::Image)),
    }
}

fn get_variable_name(node: &AstNode) -> RuntimeResult<&str> {
    match node {
        AstNode::Variable(name) => Ok(name),
        _ => Err(Error::ExpectedVariable),
    }
}

pub struct Code {
    pub ast: Vec<AstNode>,
    pub variables: HashMap<String, VariableValue>,
}

pub type CodeResult = Result<Code, PestError<Rule>>;
pub type RuntimeResult<T> = Result<T, Error>;

#[derive(Clone, Debug)]
pub enum ArgType {
    Image,
    Integer,
    String,
}

#[derive(Clone, Debug)]
pub enum Error {
    MissingVariable(String),
    InvalidArgType(ArgType),
    InvalidFilter(String),
    NoAttribute(String, String),
    NoFileFound(String),
    ExpectedVariable,
}

impl Code {
    pub fn compile(code: &str) -> CodeResult {
        let nodes = parse(code)?;
        Ok(Code {
            ast: nodes,
            variables: HashMap::new(),
        })
    }

    fn get_content(&self, node: &AstNode) -> RuntimeResult<VariableValue> {
        match node {
            AstNode::Value(value) => Ok(value.clone()),
            AstNode::Variable(name) => Ok(self
                .variables
                .get(name)
                .ok_or_else(|| Error::MissingVariable(name.clone()))?
                .clone()),
            AstNode::GetAttr(args) => {
                let (varnode, attrnode) = &**args;
                let attrname = &match self.get_content(attrnode)? {
                    VariableValue::String(s) => s,
                    _ => unreachable!(),
                };
                let varname = get_variable_name(varnode)?;
                let variable = self
                    .variables
                    .get(varname)
                    .ok_or_else(|| Error::MissingVariable(varname.into()))?;
                let attr = variable.get_attr(attrname)?;

                Ok(attr)
            }
            _ => unreachable!(),
        }
    }

    fn pop_variable(&mut self, node: AstNode) -> RuntimeResult<(String, VariableValue)> {
        match node {
            AstNode::Variable(name) => {
                let var: VariableValue = self
                    .variables
                    .remove(&name)
                    .ok_or_else(|| Error::MissingVariable(name.clone()))?;
                Ok((name, var))
            }
            _ => panic!("Expected variable"),
        }
    }

    pub fn run(mut self) -> RuntimeResult<Option<Image>> {
        // Could represent this as a `Vec<Statement>` if only the top layer can contain commands
        for node in self.ast.clone() {
            match node {
                AstNode::Open(args) => {
                    let (file, var) = &*args;
                    let temp = self.get_content(file)?;
                    let filename = expect_string(&temp)?;
                    let variable = get_variable_name(var)?;
                    let image = Image::new(filename.to_string())?;
                    let value = VariableValue::Image(image);

                    self.variables.insert(variable.to_owned(), value);
                }
                AstNode::Print(valuebox) => {
                    let value = self.get_content(&*valuebox)?;
                    println!("{}", value);
                }
                AstNode::Output(outputbox) => {
                    let file = expect_image(self.get_content(&*outputbox)?)?;
                    return Ok(Some(file));
                }
                AstNode::Resize(args) => {
                    let (input, w, h, filternode, outputnode) = &*args;
                    let image = expect_image(self.get_content(input)?)?;
                    let filter = match expect_string(&self.get_content(filternode)?)? {
                        "nearest" => Ok(transform::SamplingFilter::Nearest),
                        "triangle" => Ok(transform::SamplingFilter::Triangle),
                        "catmullrom" => Ok(transform::SamplingFilter::CatmullRom),
                        "gaussian" => Ok(transform::SamplingFilter::Gaussian),
                        "lanczons" => Ok(transform::SamplingFilter::Lanczos3),
                        s => Err(Error::InvalidFilter(s.into())),
                    }?;

                    let width = expect_integer(&self.get_content(w)?)? as u32;
                    let height = expect_integer(&self.get_content(h)?)? as u32;
                    let outputname = get_variable_name(outputnode)?;

                    let newimage = Image {
                        name: image.name,
                        image: transform::resize(&image.image, width, height, filter),
                    };
                    self.variables
                        .insert(outputname.to_owned(), VariableValue::Image(newimage));
                }
                AstNode::SetVar(args) => {
                    let (varnode, valuenode) = &*args;
                    let name = get_variable_name(varnode)?;
                    let value = self.get_content(valuenode)?;

                    self.variables.insert(name.to_owned(), value);
                }
                AstNode::Save(args) => {
                    let (imagenode, filenamenode) = &*args;
                    let image = expect_image(self.get_content(imagenode)?)?;
                    let temp = self.get_content(filenamenode)?;
                    let outputname = expect_string(&temp)?;

                    save_image(image.image, &outputname);
                }
                AstNode::Flip(fliptype, imagebox) => {
                    let (varname, var) = self.pop_variable((*imagebox).clone())?;

                    let mut image = expect_image(var)?;

                    match fliptype {
                        FlipType::H => transform::fliph(&mut image.image),
                        FlipType::V => transform::flipv(&mut image.image),
                    }

                    self.variables.insert(varname, VariableValue::Image(image));
                }
                AstNode::Blend(args) => {
                    let (base, overlay, modenode) = &*args;
                    let (name, base_image_var) = self.pop_variable((*base).clone())?;
                    let mut base_image = expect_image(base_image_var)?;
                    let overlay_image = expect_image(self.get_content(overlay)?)?;
                    let temp = self.get_content(modenode)?;
                    let mode = expect_string(&temp)?;

                    blend(&mut base_image.image, &overlay_image.image, mode);
                    self.variables
                        .insert(name, VariableValue::Image(base_image));
                }
                _ => unreachable!(),
            }
        }
        Ok(None)
    }
}

pub fn compile(code: &str) -> CodeResult {
    Code::compile(code)
}
