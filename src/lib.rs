use std::{
    collections::{BTreeMap, HashSet},
    sync::Arc,
};

use serde::Deserialize;
use trustfall::{execute_query, FieldValue, Schema, SchemaAdapter, TryIntoStruct};
use wasm_bindgen::prelude::wasm_bindgen;

const INTROSPECTION_QUERY: &str = r#"
        query {
            VertexType {
                name @output
                implements @fold {
                    implements: name @output
                }
                property @fold {
                    property: name @output
                }
                edge @fold {
                    edge: name @output
                }
            }
        }
        "#;

#[derive(Deserialize, Debug, Default, Clone)]
struct IntrospectionOutput {
    name: String,
    implements: HashSet<String>,
    property: HashSet<String>,
    edge: HashSet<String>,
}

#[wasm_bindgen]
pub fn diff_schemas(schema_one_str: &str, schema_two_str: &str) -> String {
    let schema_adapter_schema = &Schema::parse(SchemaAdapter::schema_text()).unwrap();

    let schema_one = Schema::parse(schema_one_str).unwrap();
    let adapter_one = SchemaAdapter::new(&schema_one);
    let arc_one: Arc<_> = adapter_one.into();
    let data_one = run_introspection_query(schema_adapter_schema, arc_one.clone());

    let schema_two = Schema::parse(schema_two_str).unwrap();
    let adapter_two = SchemaAdapter::new(&schema_two);
    let arc_two: Arc<_> = adapter_two.into();
    let data_two = run_introspection_query(schema_adapter_schema, arc_two.clone());

    let mut output: Vec<String> = vec![];

    for (name, new) in &data_two {
        let mut pushed = false;
        let old = if !data_one.contains_key(name) {
            output.push(format!("+ {name}"));
            pushed = true;
            Default::default()
        } else {
            data_one[name].clone()
        };

        let removed_implements = old.implements.difference(&new.implements);
        for implem in removed_implements {
            pushed = true;
            output.push(format!("- {} implements {}", name, implem));
        }
        let added_implements = new.implements.difference(&old.implements);
        for implem in added_implements {
            pushed = true;
            output.push(format!("+ {} implements {}", name, implem));
        }

        // properties
        let old_combined_implemented_properties = old
            .implements
            .iter()
            .flat_map(|x| data_one[x].property.clone())
            .collect::<HashSet<_>>();
        let new_combined_implemented_properties = new
            .implements
            .iter()
            .flat_map(|x| data_two[x].property.clone())
            .collect::<HashSet<_>>();

        let removed_properties = old.property.difference(&new.property);
        for property in removed_properties {
            if old_combined_implemented_properties.contains(property) {
                continue;
            }
            pushed = true;
            output.push(format!("- {}.{}", name, property));
        }
        let added_properties = new.property.difference(&old.property);
        for property in added_properties {
            if new_combined_implemented_properties.contains(property) {
                continue;
            }
            pushed = true;
            output.push(format!("+ {}.{}", name, property));
        }
        // edges
        let old_combined_implemented_edges = old
            .implements
            .iter()
            .flat_map(|x| data_one[x].edge.clone())
            .collect::<HashSet<_>>();
        let new_combined_implemented_edges = new
            .implements
            .iter()
            .flat_map(|x| data_two[x].edge.clone())
            .collect::<HashSet<_>>();

        let removed_edges = old.edge.difference(&new.edge);
        for edge in removed_edges {
            if old_combined_implemented_edges.contains(edge) {
                continue;
            }
            pushed = true;
            output.push(format!("- {}.{}", name, edge));
        }
        let added_edges = new.edge.difference(&old.edge);
        for edge in added_edges {
            if new_combined_implemented_edges.contains(edge) {
                continue;
            }
            pushed = true;
            output.push(format!("+ {}.{}", name, edge));
        }

        if pushed {
            output.push("".to_owned());
        }
    }

    format!("```diff\n{}\n```", output.join("\n"))
}

fn run_introspection_query(
    schema_adapter_schema: &Schema,
    schema_adapter: Arc<SchemaAdapter>,
) -> BTreeMap<String, IntrospectionOutput> {
    let args: BTreeMap<Arc<str>, FieldValue> = Default::default();

    execute_query(
        schema_adapter_schema,
        Arc::clone(&schema_adapter),
        INTROSPECTION_QUERY,
        args,
    )
    .expect("to succeed")
    .map(|x| {
        x.try_into_struct::<IntrospectionOutput>()
            .expect("to fit into IntrospectionOutput format")
    })
    .fold(BTreeMap::new(), |mut map, output| {
        map.insert(output.name.clone(), output);
        map
    })
}
