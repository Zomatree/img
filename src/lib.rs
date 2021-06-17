use pest::error::Error as PestError;
use photon_rs::{multiple::blend, native::save_image, transform};

use std::{collections::HashMap, fmt};

mod ast;
mod structs;
use crate::ast::{parse, AstNode, Rule};
use crate::structs::{ArgType, Error, FlipType, Image, RuntimeResult, VariableValue};

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

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::MissingVariable(var) => write!(f, "Missing Variable \"{}\"", var),
            Error::InvalidArgType(_type) => write!(f, "Invalid Argument Type \"{:?}\"", _type),
            Error::InvalidFilter(filter) => write!(f, "Invalid Filter \"{}\"", filter),
            Error::NoAttribute(_type, attr) => {
                write!(f, "No Attribute On \"{}\" Called \"{}\"", _type, attr)
            }
            Error::NoFileFound(filename) => write!(f, "No file Called \"{}\"", filename),
            Error::ExpectedVariable => write!(f, "Expected Variable"),
        }
    }
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
            _ => Err(Error::ExpectedVariable),
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
