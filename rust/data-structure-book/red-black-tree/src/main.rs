use red_black_tree::Rbt;
use std::process;

fn main() {
    let tree = Rbt::new();
    if tree.is_valid() {
        println!("You created a valid null RBT!");
    } else {
        eprintln!("Poo, freshly sprouted RBT is invalid.");
        process::exit(-1);
    }
}
