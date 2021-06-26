use pest::Parser;
use pest::{error::Error as PestError, iterators::Pair};

use crate::structs::{FlipType, VariableValue};

#[derive(pest_derive::Parser)]
#[grammar = "grammar.pest"]
pub struct IMGParser;

#[derive(Debug, Clone)]
pub enum AstNode {
    Print(Box<AstNode>),
    Open(Box<(AstNode, AstNode)>),
    Resize(Box<(AstNode, AstNode, AstNode, AstNode, AstNode)>),
    Output(Box<AstNode>),
    Save(Box<(AstNode, AstNode)>),
    SetVar(Box<(AstNode, AstNode)>),
    GetAttr(Box<(AstNode, AstNode)>),
    Flip(FlipType, Box<(AstNode, AstNode)>),
    Blur(Box<(AstNode, AstNode, AstNode)>),
    Overlay(Box<(AstNode, AstNode, AstNode, AstNode, AstNode)>),
    
    Variable(String),
    Value(VariableValue),
}

pub fn build_ast_from_expr(pair: Pair<Rule>) -> AstNode {
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
        Rule::FlipVExpr => {
            let mut pair = pair.into_inner();
            AstNode::Flip(
                FlipType::V,
                Box::new((
                    build_ast_from_expr(pair.next().unwrap()),
                    build_ast_from_expr(pair.next().unwrap()),
                )),
            )
        }
        Rule::FlipHExpr => {
            let mut pair = pair.into_inner();
            AstNode::Flip(
                FlipType::H,
                Box::new((
                    build_ast_from_expr(pair.next().unwrap()),
                    build_ast_from_expr(pair.next().unwrap()),
                )),
            )
        }
        Rule::BlurExpr => {
            let mut pair = pair.into_inner();
            AstNode::Blur(Box::new((
                build_ast_from_expr(pair.next().unwrap()),
                build_ast_from_expr(pair.next().unwrap()),
                build_ast_from_expr(pair.next().unwrap()),
            )))
        }
        Rule::OverlayExpr => {
            let mut pair = pair.into_inner();
            AstNode::Overlay(Box::new((
                build_ast_from_expr(pair.next().unwrap()),
                build_ast_from_expr(pair.next().unwrap()),
                build_ast_from_expr(pair.next().unwrap().into_inner().next().unwrap()),
                build_ast_from_expr(pair.next().unwrap().into_inner().next().unwrap()),
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
            panic!(" unhandled op: {:?}", unknown)
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
