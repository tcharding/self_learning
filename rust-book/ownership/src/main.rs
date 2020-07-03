fn main() {
    println!("Rust ownership functions.");

    let mut s = String::from("hello");

    let len = calculate_length(&s);
    println!("s : '{}' len: {}", s, len);

    change(&mut s);
    println!("{}", s);

    let len = calculate_length(&s);
    println!("s : '{}' len: {}", s, len);

    let line = String::from("this is a line\n");
    let first = first_word(&line);
    println!("first word: {}", first);
}

fn change(s: &mut String) {
    s.push_str(", world!");
}

fn calculate_length(s: &String) -> usize {
    s.len()
}

fn first_word(s: &String) -> &str {
    let bytes = s.as_bytes();
    
    for (i, &item) in bytes.iter().enumerate() {
        if item == b' ' {
            return &s[..i];
        }
    }
    &s[..]
}
