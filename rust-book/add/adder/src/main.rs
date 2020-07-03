use add_one;

fn main() {
    let num = 10;
    println!("Hello, world! {} plus 1 is {}", num, add_one::add_one(num));
    println!("OMG! {} plus 2 is {}", num, add_two::add_two(num));
}
