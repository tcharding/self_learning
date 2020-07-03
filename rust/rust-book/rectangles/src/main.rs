#[derive(Debug)]
struct Rectangle {
    width: u32,
    height: u32,
}

impl Rectangle {
    fn area(&self) -> u32 {
        self.width * self.height
    }

    fn can_hold(&self, other: &Rectangle) -> bool {
        other.width < self.width &&
            other.height < self.height
    }
}

fn main() {
    let rect = Rectangle { width: 30, height: 50};

    println!("The area of the rectangle is {} square pixels.",
             rect.area());

    println!("rect: {:?}", rect);
}
