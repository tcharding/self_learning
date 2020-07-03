use into_map_derive::IntoMap;

#[derive(IntoMap)]
struct User {
    name: String,
    id: usize,
    active: bool,
}

fn main() {
    let bar = User {
        name: "Alice".to_string(),
        id: 35,
        active: false,
    };
    let map = bar.into_map();
    println!("{:?}", map);
}
