fn main() {
    println!("Structs!");
    demo_structs();
}

struct User {
    name: String,
    id: u64,
}

fn demo_structs() {
    let user = User {
        name: String::from("Tobin"),
        id: 1,
    };
    
}
