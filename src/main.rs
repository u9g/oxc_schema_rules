use oxc_schema_rules::diff_schemas;

fn main() {
    let schema_one_str = include_str!("./schema1.graphql");
    let schema_two_str = include_str!("./schema2.graphql");
    println!("{}", diff_schemas(schema_one_str, schema_two_str));
}
