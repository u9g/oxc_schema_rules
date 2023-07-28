use std::collections::BTreeMap;

use apollo_parser::{ast::Definition, Parser};
use miette::{miette, Diagnostic, LabeledSpan, Result};
use thiserror::Error;
use trustfall::{execute_query, FieldValue, Schema, SchemaAdapter};

#[derive(Debug, Error, Diagnostic)]
#[error("Invalid Schema!")]
struct ManyErrors {
    #[related]
    others: Vec<miette::Report>,
}

fn anything_that_ends_with_ast_must_impl_astnode(schema_text: &str) -> Vec<miette::Report> {
    let top_level_definitions = Parser::new(schema_text).parse().document();

    let schema = Schema::parse(schema_text).unwrap();

    let adapter = SchemaAdapter::new(&schema);

    let mut vars: BTreeMap<_, FieldValue> = BTreeMap::new();
    vars.insert("ast", "AST".into());
    vars.insert("zero", 0.into());
    vars.insert("ast_node", "ASTNode".into());

    execute_query(
        &Schema::parse(SchemaAdapter::schema_text()).unwrap(),
        adapter.into(),
        r#"
        query {
            VertexType {
                name @filter(op: "has_suffix", value: ["$ast"]) @output(name: "type_name_and_get_the_span_automatically")

                implements @fold
                    @transform(op: "count")
                    @filter(op: "=", value: ["$zero"])
                {
                    name @filter(op: "=", value: ["$ast_node"])
                }
            }
        }
        "#,
        vars,
    )
    .unwrap()
    .map(|row| {
        let Some(v) = row.get("type_name_and_get_the_span_automatically") else {unreachable!()};
        let object_type_name_token = top_level_definitions.definitions()
            .filter(|x| matches!(x, Definition::ObjectTypeDefinition(_)))
            .filter(|x| x.name().unwrap().text() == v.as_str().unwrap())
            .map(|x| x.name().unwrap().ident_token().unwrap().text_range())
            .next().unwrap();

        let st: usize = object_type_name_token.start().into();
        let end: usize = object_type_name_token.end().into();

        miette!(
            code = "oxc::implement_astnode_for_ast_type",
            labels = vec![LabeledSpan::at(st..end, format!("implement ASTNode for '{}'", v.as_str().unwrap()))],
            "implements ast_node_but"
        )
    })
    .collect::<Vec<_>>()
}

fn anything_that_impls_astnode_must_ends_with_ast(schema_text: &str) -> Vec<miette::Report> {
    let top_level_definitions = Parser::new(schema_text).parse().document();

    let schema = Schema::parse(schema_text).unwrap();

    let adapter = SchemaAdapter::new(&schema);

    let mut vars: BTreeMap<_, FieldValue> = BTreeMap::new();
    vars.insert("ast", "AST".into());
    vars.insert("zero", 0.into());
    vars.insert("ast_node", "ASTNode".into());

    execute_query(
        &Schema::parse(SchemaAdapter::schema_text()).unwrap(),
        adapter.into(),
        r#"
        query {
            VertexType {
                name @filter(op: "not_has_suffix", value: ["$ast"]) @output(name: "type_name_and_get_the_span_automatically")

                implements @fold
                    @transform(op: "count")
                    @filter(op: ">", value: ["$zero"])
                {
                    name @filter(op: "=", value: ["$ast_node"])
                }
            }
        }
        "#,
        vars,
    )
    .unwrap()
    .map(|row| {
        let Some(v)  = row.get("type_name_and_get_the_span_automatically") else {unreachable!()};
        let object_type_name_token = top_level_definitions.definitions()
            .filter(|x| matches!(x, Definition::ObjectTypeDefinition(_)))
            .filter(|x| x.name().unwrap().text() == v.as_str().unwrap())
            .map(|x| x.name().unwrap().ident_token().unwrap().text_range())
            .next().unwrap();

        let st: usize = object_type_name_token.start().into();
        let end: usize = object_type_name_token.end().into();

        miette!(
            code = "oxc::ast_type_doesnt_impl_astnode",
            labels = vec![LabeledSpan::at(st..end, "suffix with 'AST'")],
            "types that implement ASTNode must have a suffix of 'AST'"
        )
    })
    .collect::<Vec<_>>()
}

fn main() -> Result<()> {
    miette::set_hook(Box::new(|_| {
        Box::new(miette::MietteHandlerOpts::new().build())
    }))?;

    let schema_text = std::include_str!("./schema.graphql");

    let errs = anything_that_ends_with_ast_must_impl_astnode(schema_text)
        .into_iter()
        .chain(anything_that_impls_astnode_must_ends_with_ast(schema_text))
        .map(|err| err.with_source_code(schema_text))
        .collect::<Vec<_>>();

    if !errs.is_empty() {
        Err(ManyErrors { others: errs }.into())
    } else {
        Ok(())
    }
}
