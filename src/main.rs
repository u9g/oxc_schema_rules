#![feature(is_sorted)]

use std::{cmp::Ordering, collections::BTreeMap, sync::Arc};

use apollo_parser::{
    ast::{Definition, FieldDefinition, Type},
    Parser,
};
use miette::{miette, Diagnostic, LabeledSpan, Result};
use thiserror::Error;
use trustfall::{execute_query, FieldValue, Schema, SchemaAdapter};

#[derive(Debug, Error, Diagnostic)]
#[error("Invalid Schema!")]
struct ManyErrors {
    #[related]
    others: Vec<miette::Report>,
}

fn field_naming(schema_text: &str) -> Vec<miette::Report> {
    let top_level_definitions = Parser::new(schema_text).parse().document();

    let schema = Schema::parse(schema_text).unwrap();

    let adapter = SchemaAdapter::new(&schema);

    let mut vars: BTreeMap<_, FieldValue> = BTreeMap::new();
    vars.insert("caps_camel_case", "[A-Z][a-zA-Z0-9]+".into());

    execute_query(
        &Schema::parse(SchemaAdapter::schema_text()).unwrap(),
        adapter.into(),
        r#"
        query {
            Entrypoint {
                name @output(name: "edge_and_get_the_span_automatically") @filter(op: "not_regex", value: ["$caps_camel_case"])
            }
        }
        "#,
        vars,
    )
    .unwrap()
    .map(|row| {
        let type_wanted = top_level_definitions
            .definitions()
            .filter_map(|x| {
                let Definition::ObjectTypeDefinition(otd) = x else {
                    return None;
                };
                Some(otd)
            })
            .find(|x| x.name().unwrap().text() == "RootSchemaQuery");

        let fdefs = type_wanted.unwrap().fields_definition().unwrap();

        let edge_name = &row["edge_and_get_the_span_automatically"];
        let fdef = fdefs.field_definitions().find(|x| x.name().unwrap().text() == edge_name.as_str().unwrap()).unwrap();
        let range = fdef.name().unwrap().ident_token().unwrap().text_range();
        let st: usize = range.start().into();
        let end: usize = range.end().into();
        miette!(
            code = "trustfall::caps_camel_for_root_query_name_edges",
            labels = vec![LabeledSpan::at(st..end, "rewrite in caps camel case")],
            "Edges in RootSchemaQuery must be written in capitalized camel case"
        )
    })
    .collect::<Vec<_>>()
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
            .filter_map(|x| {
                let Definition::ObjectTypeDefinition(otd) = x else {return None};
                Some(otd)
            })
            .filter(|x| x.name().unwrap().text() == v.as_str().unwrap())
            // .map(|x| x.name().unwrap().ident_token().unwrap().text_range()) // for name span
            .map(|x| {
                x.implements_interfaces().unwrap().implements_token().unwrap().text_range()
            })
            .next().unwrap();

        let st: usize = object_type_name_token.start().into();
        let end: usize = object_type_name_token.end().into();

        miette!(
            code = "oxc::implement_astnode_for_ast_type",
            labels = vec![LabeledSpan::at(st..end, "implement ASTNode")],
            "types that end with 'AST' must implement ASTNode"
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

fn array_type_must_have_bang(schema_text: &str) -> Vec<miette::Report> {
    let top_level_definitions = Parser::new(schema_text).parse().document();

    top_level_definitions
        .definitions()
        .filter_map(|ref x| {
            let field_definitions = match x {
                Definition::ObjectTypeDefinition(otd) => otd.fields_definition(),
                Definition::InterfaceTypeDefinition(itd) => itd.fields_definition(),
                _ => return None,
            }
            .unwrap();

            Some(field_definitions.field_definitions().filter_map(|field| {
                let field_type = field.ty().unwrap();
                if is_list_type(&field_type) {
                    let ty = match field_type {
                        Type::ListType(lt) => lt.ty().unwrap(),
                        Type::NonNullType(nnt) => nnt.list_type().unwrap().ty().unwrap(),
                        _ => unreachable!(),
                    };
                    let Type::NamedType(nullish_type) = ty else {
                        return None;
                    };
                    let range = nullish_type
                        .name()
                        .unwrap()
                        .ident_token()
                        .unwrap()
                        .text_range();
                    let st: usize = range.start().into();
                    let end: usize = range.end().into();
                    Some(miette!(
                        code = "trustfall::no_list_inner_type_nullability",
                        labels = vec![LabeledSpan::at(st..end, "add ! after type in list")],
                        "types in a list must have a ! suffix"
                    ))
                } else {
                    None
                }
            }))
        })
        .flatten()
        .collect::<Vec<_>>()
}

fn is_primitive_type_name(type_name: &str) -> bool {
    type_name == "String" || type_name == "Int" || type_name == "Float" || type_name == "Boolean"
}

fn is_primitive(_type: &Type) -> bool {
    match _type {
        Type::NamedType(ty) => is_primitive_type_name(ty.name().unwrap().text().as_str()),
        Type::NonNullType(ty) => ty.named_type().map_or(false, |list_ty| {
            is_primitive_type_name(list_ty.name().unwrap().text().as_str())
        }),
        Type::ListType(_) => false,
    }
}

fn is_list_type(_type: &Type) -> bool {
    matches!(_type, Type::ListType(_))
        || matches!(_type, Type::NonNullType(nnt) if nnt.list_type().is_some())
}

fn field_definition_sorter_maybe(a: &FieldDefinition, b: &FieldDefinition) -> Option<Ordering> {
    Some(field_definition_sorter(a, b))
}

fn field_definition_sorter(a: &FieldDefinition, b: &FieldDefinition) -> Ordering {
    let a_type = a.ty().unwrap();
    let b_type = b.ty().unwrap();
    let a_is_primitive = is_primitive(&a_type);
    let b_is_primitive = is_primitive(&b_type);

    if a_is_primitive && !b_is_primitive {
        Ordering::Less
    } else if !a_is_primitive && b_is_primitive {
        Ordering::Greater
    } else if !is_list_type(&a_type) && is_list_type(&b_type) {
        Ordering::Less
    } else if is_list_type(&a_type) && !is_list_type(&b_type) {
        Ordering::Greater
    } else {
        a.name().unwrap().text().cmp(&b.name().unwrap().text())
    }
}

fn bottom_heavy_fields(schema_text: &str) -> Vec<miette::Report> {
    let top_level_definitions = Parser::new(schema_text).parse().document();

    let schema = Schema::parse(schema_text).unwrap();

    let adapter = SchemaAdapter::new(&schema);

    let vars: BTreeMap<Arc<str>, FieldValue> = BTreeMap::new();

    execute_query(
        &Schema::parse(SchemaAdapter::schema_text()).unwrap(),
        adapter.into(),
        r#"
        query {
            VertexType {
                name @output
            }
        }
        "#,
        vars,
    )
    .unwrap()
    .filter_map(|row| {
        let maybe_rule_violation = top_level_definitions
            .definitions()
            .filter(|x| {
                x.name()
                    .map_or(false, |name| name.text() == row["name"].as_str().unwrap())
            })
            .filter_map(|ref x| {
                let field_definitions = match x {
                    Definition::ObjectTypeDefinition(otd) => otd.fields_definition(),
                    Definition::InterfaceTypeDefinition(itd) => itd.fields_definition(),
                    _ => unreachable!(),
                }
                .unwrap();

                let mut fds = field_definitions.field_definitions().collect::<Vec<_>>();

                if !fds.is_sorted_by(field_definition_sorter_maybe) {
                    let tr = x.name().unwrap().ident_token().unwrap().text_range();
                    let st: usize = tr.start().into();
                    let end: usize = tr.end().into();

                    fds.sort_by(field_definition_sorter);

                    Some(miette!(
                        code = "trustfall::sort_fields",
                        labels = vec![LabeledSpan::at(st..end, "Sort fields")],
                        help = format!(
                            "Correct order: {}",
                            fds.into_iter()
                                .map(|x| x.name().unwrap().text().as_str().to_string())
                                .collect::<Vec<_>>()
                                .join(", ")
                        ),
                        "Fields should be sorted bottom-heavy then by field name alphabetically"
                    ))
                } else {
                    None
                }
            })
            .next();

        maybe_rule_violation
    })
    .collect::<Vec<_>>()
}

fn main() -> Result<()> {
    miette::set_hook(Box::new(|_| {
        Box::new(miette::MietteHandlerOpts::new().build())
    }))?;

    let schema_text = std::include_str!("./schema.graphql");

    let errs = array_type_must_have_bang(schema_text)
        .into_iter()
        .chain(field_naming(schema_text))
        .chain(anything_that_ends_with_ast_must_impl_astnode(schema_text))
        .chain(anything_that_impls_astnode_must_ends_with_ast(schema_text))
        // .chain(bottom_heavy_fields(schema_text))
        .map(|err| err.with_source_code(schema_text))
        .collect::<Vec<_>>();

    if !errs.is_empty() {
        Err(ManyErrors { others: errs }.into())
    } else {
        Ok(())
    }
}
