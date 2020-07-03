
fn main() {
    let number_list = vec![34, 50, 25, 100, 65];

    let largest = largest(&number_list);

    println!("The largest number is {}", largest);

    let s = String::from("first word from string");
    let first = first_word(s.as_str());
    println!("calling first_word with arg: '{}' \n got: {}", s.as_str(), first);
}

fn largest<T>(list: &[T]) -> T
    where T: PartialOrd + Copy
{
    let mut largest = list[0];

    for &item in list.iter() {
        if item > largest {
            largest = item;
        }
    }
    largest
}

pub trait Summary {
    fn summarize(&self) -> String;
}

fn first_word(s: &str) -> &str {
    let bytes = s.as_bytes();

    for (i, &item) in bytes.iter().enumerate() {
        if item == b' ' {
            return &s[0..i];
        }
    }

    &s[..]
}

    
