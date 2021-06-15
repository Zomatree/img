#[macro_use]
extern crate pest_derive;

extern crate pest;

use std::{borrow::Cow, collections::HashMap, fmt};
use pest::{Parser, error::Error, iterators::Pair};
use photon_rs::{PhotonImage, native::{open_image, save_image},  transform};

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct IMGParser;

#[derive(Debug, Clone)]
pub enum FlipType {H, V}

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
    String(String),
    Variable(String),
    Int(i64),
}

fn build_ast_from_expr(pair: Pair<Rule>) -> AstNode {
    match pair.as_rule() {
        Rule::expr => build_ast_from_expr(pair.into_inner().next().unwrap()),
        Rule::OpenExpr => {
            let mut pair = pair.into_inner();
            AstNode::Open(Box::new((build_ast_from_expr(pair.next().unwrap()), build_ast_from_expr(pair.next().unwrap()))))
        },
        Rule::OutputExpr => {
            AstNode::Output(Box::new(build_ast_from_expr(pair.into_inner().next().unwrap())))
        },
        Rule::PrintExpr => {
            let mut pair = pair.into_inner();
            AstNode::Print(Box::new(build_ast_from_expr(pair.next().unwrap())))
        },
        Rule::ResizeExpr => {
            let mut pair = pair.into_inner();
            AstNode::Resize(Box::new((
                build_ast_from_expr(pair.next().unwrap()),
                build_ast_from_expr(pair.next().unwrap().into_inner().next().unwrap()), build_ast_from_expr(pair.next().unwrap().into_inner().next().unwrap()),
                build_ast_from_expr(pair.next().unwrap()),
                build_ast_from_expr(pair.next().unwrap())))
            )
        },
        Rule::SaveExpr => {
            let mut pair = pair.into_inner();
            AstNode::Save(Box::new((build_ast_from_expr(pair.next().unwrap()), build_ast_from_expr(pair.next().unwrap()))))
        },
        Rule::SetVarExpr => {
            let mut pair = pair.into_inner();
            AstNode::SetVar(Box::new((build_ast_from_expr(pair.next().unwrap()), build_ast_from_expr(pair.next().unwrap()))))
        },
        Rule::FlipVExpr => {
            AstNode::Flip(FlipType::V, Box::new(build_ast_from_expr(pair.into_inner().next().unwrap())))
        },
        Rule::FlipHExpr => {
            AstNode::Flip(FlipType::H, Box::new(build_ast_from_expr(pair.into_inner().next().unwrap())))
        },
        Rule::getattr => {
            let mut pair = pair.into_inner();
            let var = build_ast_from_expr(pair.next().unwrap());
            let attr = pair.next().unwrap().as_span().as_str().to_string();
            AstNode::GetAttr(Box::new((var, AstNode::String(attr))))
        },
        Rule::string => {
            let mut chars = pair.as_span().as_str().chars();
            chars.next();
            chars.next_back();

            AstNode::String(chars.as_str().to_string())
        },
        Rule::variable => {
            AstNode::Variable(pair.as_span().as_str().to_string())
        },
        Rule::hex => {
            AstNode::Int(i64::from_str_radix(pair.as_span().as_str().trim_start_matches("#"), 16).unwrap())
        },
        Rule::int => {
            AstNode::Int(pair.as_span().as_str().parse::<i64>().unwrap())
        }
        Rule::value | Rule::_value => {
            build_ast_from_expr(pair.into_inner().next().unwrap())
        },
        unknown => {panic!("{:?}", unknown)}
    }
}

pub fn parse(code: &str) -> Result<Vec<AstNode>, Error<Rule>> {
    let pairs =  IMGParser::parse(Rule::program, code)?;
    let mut ast = vec![];

    for pair in pairs {
        match pair.as_rule() {
            Rule::expr => {
                ast.push(build_ast_from_expr(pair));
            }
            _ => {}
        }
    };
    Ok(ast)
}

#[derive(Debug, Clone)]
pub struct Image {
    image: PhotonImage,
    name: String,
}

impl Image {
    fn new(name: String) -> Self {
        let image = open_image(name.as_str()).expect(format!("No image found called {}", name).as_str());
        Image {image, name}
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
    Image(Image)
}

impl VariableValue {
    fn get_attr(&self, name: String) -> VariableValue {
        match self {
            VariableValue::String(_) => panic!("Cant get attribute on string"),
            VariableValue::Integer(_) => panic!("Cant get attribute on integer"),
            VariableValue::Image(image) => {
                match name.as_str() {
                    "width" => VariableValue::Integer(image.image.get_width() as i64),
                    "height" => VariableValue::Integer(image.image.get_height() as i64),
                    _ => panic!("No attribute on image called '{}'", name)
                }
            }
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

fn expect_string(value: VariableValue) -> String {
    match value {
        VariableValue::String(v) => v.to_string(),
        _ => panic!("Expected string")
    }
}

fn expect_integer(value: VariableValue) -> i64 {
    match value {
        VariableValue::Integer(v) => v,
        _ => panic!("Expected integer")
    }
}

fn expect_image(value: VariableValue) -> Image {
    match value {
        VariableValue::Image(v) => v,
        _ => panic!("Expected image")
    }
}

fn get_variable_name(node: AstNode) -> String {
    match node {
        AstNode::Variable(name) => name,
        _ => panic!("Expected variable")
    }
}

pub struct Code {
    pub ast: Vec<AstNode>,
    pub variables: HashMap<String, VariableValue>
}

pub type CodeResult = Result<Code, Error<Rule>>;

impl Code {
    pub fn compile(code: &str) -> CodeResult {
        let nodes = parse(code)?;
        Ok(Code {
            ast: nodes,
            variables: HashMap::new()
        })
    }

    fn get_content(&self, node: AstNode) -> Cow<VariableValue> {
        match node {
            AstNode::Int(v) => Cow::Owned(VariableValue::Integer(v)),
            AstNode::String(v) => Cow::Owned(VariableValue::String(v)),
            AstNode::Variable(name) => Cow::Borrowed(self.variables.get(&name).expect(format!("No variable found called {}", name).as_str())),
            AstNode::GetAttr(args) => {
                let (varnode, attrnode) = *args;
                let attrname = expect_string(self.get_content(attrnode).into_owned());
                let varname = get_variable_name(varnode);
                let variable = self.variables.get(&varname).expect(format!("No variable found called {}", varname).as_str());
                let attr = variable.get_attr(attrname);

                Cow::Owned(attr)
            }
            _ => unreachable!()
        }
    }

    fn pop_variable(&mut self, node: AstNode) -> (Cow<'static, String>, Cow<'static, VariableValue>) {
        match node {
            AstNode::Variable(name) => {
                let var: VariableValue = self.variables.remove(&name).expect(format!("No variable found called {}", name).as_str());
                (Cow::Owned(name), Cow::Owned(var))
            },
            _ => panic!("Expected variable")
        }
    }

    pub fn run(mut self) -> Option<Image> {
        for node in self.ast.clone().into_iter() {
            match node {
                AstNode::Open(args) => {
                    let (file, var) = *args;
                    let filename = expect_string(self.get_content(file).into_owned());
                    let variable = get_variable_name(var);
                    let image = Image::new(filename);
                    let value = VariableValue::Image(image);

                    self.variables.insert(variable, value);
                },
                AstNode::Print(valuebox) => {
                    let value = self.get_content(*valuebox).into_owned();
                    println!("{}", value);
                },
                AstNode::Output(outputbox) => {
                    let file = expect_image(self.get_content(*outputbox).into_owned());
                    return Some(file);
                },
                AstNode::Resize(args) => {
                    let (input, w, h, filternode, outputnode) = *args;
                    let image = expect_image(self.get_content(input).into_owned());
                    let filter = match expect_string(self.get_content(filternode).into_owned()).as_str() {
                        "nearest" => transform::SamplingFilter::Nearest,
                        "triangle" => transform::SamplingFilter::Triangle,
                        "catmullrom" => transform::SamplingFilter::CatmullRom,
                        "gaussian" => transform::SamplingFilter::Gaussian,
                        "lanczons" => transform::SamplingFilter::Lanczos3,
                        _ => panic!("Invalid filter")
                    };

                    let width = expect_integer(self.get_content(w).into_owned()) as u32;
                    let height = expect_integer(self.get_content(h).into_owned()) as u32;
                    let outputname = get_variable_name(outputnode);

                    let newimage = Image {name: image.name, image: transform::resize(&image.image, width, height, filter)};
                    self.variables.insert(outputname, VariableValue::Image(newimage));
                },
                AstNode::SetVar(args) => {
                    let (varnode, valuenode) = *args;
                    let name = get_variable_name(varnode);
                    let value = self.get_content(valuenode).into_owned();
                    
                    self.variables.insert(name, value);
                }
                AstNode::Save(args) => {
                    let (imagenode, filenamenode) = *args;
                    let image = expect_image(self.get_content(imagenode).into_owned());
                    let outputname = expect_string(self.get_content(filenamenode).into_owned());

                    save_image(image.image, &outputname);
                },
                AstNode::Flip(fliptype, imagebox) => {
                    let (varname, var) = self.pop_variable(*imagebox);

                    let mut image = expect_image(var.into_owned());

                    match fliptype {
                        FlipType::H => transform::fliph(&mut image.image),
                        FlipType::V => transform::flipv(&mut image.image),
                    }

                    self.variables.insert(varname.into_owned(), VariableValue::Image(image));
                },
                _ => unreachable!()
            }
        };
        None
    }
}

pub fn compile(code: &str) -> CodeResult {
	Code::compile(code)
}
