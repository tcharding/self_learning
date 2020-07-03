fn main() {
    let number = 3;

    if number < 5 {
        println!("condition was true");
    } else {
        println!("condition was false");
    }

    ifs();
    break_out();
    iterate_array();
    println!("---------");
    print_range();
}

fn print_range() {
    for x in (1..4).rev() {
        println!("got: {}", x);
    }
}

fn iterate_array() {
    let xs = [1, 2, 3, 4, 5, 6];

    for x in xs.iter() {
        println!("got: {}", x);
    }
}

fn break_out() {
    let mut counter = 0;

    let result = loop {
        counter += 1;

        if counter == 10 {
            break counter * 2;
        }
    };

    println!("The result is: {}", result)
}

fn ifs() {
    let condition = true;

    let x = if condition { 4 } else { 5 };
    println!("x: {}", x);
}
