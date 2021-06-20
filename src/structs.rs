use image::{open, DynamicImage, GenericImageView};
use std::fmt;

#[derive(Debug, Clone)]
pub struct Image {
    pub image: DynamicImage,
    pub name: String,
}

impl Image {
    pub fn new(name: String) -> RuntimeResult<Self> {
        let image = open(name.as_str()).map_err(|_| Error::NoFileFound(name.clone()))?;
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
    pub fn get_attr(&self, name: &String) -> RuntimeResult<VariableValue> {
        match self {
            VariableValue::String(_) => Err(Error::NoAttribute("string".into(), name.clone())),
            VariableValue::Integer(_) => Err(Error::NoAttribute("integer".into(), name.clone())),
            VariableValue::Image(image) => match name.as_str() {
                "width" => Ok(VariableValue::Integer(image.image.width() as i64)),
                "height" => Ok(VariableValue::Integer(image.image.height() as i64)),
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

#[derive(Clone, Debug)]
pub enum Error {
    MissingVariable(String),
    InvalidArgType(ArgType),
    InvalidFilter(String),
    NoAttribute(String, String),
    NoFileFound(String),
    ErrorReadingFile(String),
    ErrorSavingFile(String),
    ExpectedVariable,
}

#[derive(Clone, Debug)]
pub enum ArgType {
    Image,
    Integer,
    String,
}

pub type RuntimeResult<T> = Result<T, Error>;

#[derive(Debug, Clone)]
pub enum FlipType {
    H,
    V,
}
