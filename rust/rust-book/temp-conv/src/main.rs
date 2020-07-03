use std::error::Error;
use std::io::{self, BufRead};

fn main() {
    println!("Temperature converter");
    println!("1) Fahrenheit to Celcius");
    println!("2) Celcius to Fahrenheit");

    let mut line = String::new();
    let stdin = io::stdin();
    stdin
        .lock()
        .read_line(&mut line)
        .expect("Could not read line");

    let input: u32 = match line.trim().parse() {
        Ok(num) => num,
        Err(_) => {
            println!("Unknown input format, please enter an integer.");
            0
        }
    };

    match input {
        1 => f_to_c(),
        2 => c_to_f(),
        _ => {
            println!("Unknown option: {}", input);
            return;
        }
    }
}

// Celcius to Fahrenheit
fn c_to_f() {
    let temp = read_temp().expect("failed to read input");
    let conv = temp * 9 / 5 + 32;
    println!("Celcius: {}", temp);
    println!("Fahrenheit: {}", conv);
}

// Fahrenheit to Celcius{}
fn f_to_c() {
    let temp = read_temp().expect("failed to read input");
    let conv = (temp - 32) * 5 / 9;
    println!("Fahrenheit: {}", temp);
    println!("Celcius: {}", conv);
}

fn read_temp() -> Result<i32, Box<dyn Error>> {
    println!("Enter temperature:");
    let mut line = String::new();
    let stdin = io::stdin();
    stdin.lock().read_line(&mut line)?;

    let temp: i32 = line.trim().parse()?;
    Ok(temp)
}
