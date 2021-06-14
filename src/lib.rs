#[macro_use]
extern crate pest_derive;

extern crate pest;

use std::{collections::HashMap, borrow::Cow, fmt};
use pest::{Parser, error::Error, iterators::Pair};
use photon_rs::{PhotonImage, native::{open_image, save_image},  transform};

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct IMGParser;

#[derive(Debug, Clone)]
pub enum AstNode {
    Print(Box<AstNode>),
    Open(Box<(AstNode, AstNode)>),
    Resize(Box<(AstNode, AstNode, AstNode, AstNode, AstNode)>),
    Output(Box<AstNode>),
    Save(Box<(AstNode, AstNode)>),
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
        }
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
        Rule::value => {
            build_ast_from_expr(pair.into_inner().next().unwrap())
        },
        unknown => {panic!("{:?}", unknown)}
    }
}

pub fn parse(code: &str) -> Result<Vec<AstNode>, Error<Rule>> {
    match IMGParser::parse(Rule::program, code) {
        Err(err) => Err(err),
        Ok(pairs) => {
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
    }
}

#[derive(Debug, Clone)]
pub struct Image {
    image: PhotonImage,
    name: String,
}

impl Image {
    fn new(name: String) -> Self {
        let image = open_image(&*name).expect(&*format!("No image found called {}", name));
        Image {image, name }
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
        let result = parse(code);
        match result {
            Ok(nodes) => {
                Ok(Code {
                    ast: nodes,
                    variables: HashMap::new()
                })
            },
            Err(err) => Err(err)
        }
    }

    pub fn get_content(&self, node: AstNode) -> Option<Cow<VariableValue>> {
        match node {
            AstNode::Int(v) => Some(Cow::Owned(VariableValue::Integer(v))),
            AstNode::String(v) => Some(Cow::Owned(VariableValue::String(v))),
            AstNode::Variable(name) => self.variables.get(&name).map(Cow::Borrowed),
            _ => panic!("??")
        }
    }

    pub fn run(mut self) -> Option<Image> {
        for node in self.ast.clone().into_iter() {
            match node {
                AstNode::Open(args) => {
                    let (file, var) = *args;
                    let filename = expect_string(self.get_content(file).unwrap().into_owned());
                    let variable = get_variable_name(var);
                    let image = Image::new(filename);
                    let value = VariableValue::Image(image);

                    self.variables.insert(variable, value);
                },
                AstNode::Print(valuebox) => {
                    let value = self.get_content(*valuebox).unwrap().into_owned();
                    println!("{}", value);
                },
                AstNode::Output(outputbox) => {
                    let file = expect_image(self.get_content(*outputbox).unwrap().into_owned());
                    return Some(file);
                },
                AstNode::Resize(args) => {
                    let (input, w, h, filternode, outputnode) = *args;
                    let image = expect_image(self.get_content(input).unwrap().into_owned());
                    let filter = match &*expect_string(self.get_content(filternode).unwrap().into_owned()) {
                        "nearest" => transform::SamplingFilter::Nearest,
                        "triangle" => transform::SamplingFilter::Triangle,
                        "catmullrom" => transform::SamplingFilter::CatmullRom,
                        "gaussian" => transform::SamplingFilter::Gaussian,
                        "lanczons" => transform::SamplingFilter::Lanczos3,
                        _ => panic!("Invalid filter")
                    };
                    
                    let width = expect_integer(self.get_content(w).unwrap().into_owned()) as u32;
                    let height = expect_integer(self.get_content(h).unwrap().into_owned()) as u32;
                    let outputname = get_variable_name(outputnode);

                    let newimage = Image {
                        image: transform::resize(&image.image, width, height, filter),
                        name: image.name
                    };
                    self.variables.insert(outputname, VariableValue::Image(newimage));
                },
                AstNode::Save(args) => {
                    let (imagenode, filenamenode) = *args;
                    let image = expect_image(self.get_content(imagenode).unwrap().into_owned());
                    let outputname = expect_string(self.get_content(filenamenode).unwrap().into_owned());

                    save_image(image.image, &outputname);
                },
                _ => {}
            }
        };
        None
    }
}

pub fn compile(code: &str) -> CodeResult {
	Code::compile(code)
}
