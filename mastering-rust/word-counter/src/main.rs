use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::prelude::BufRead;
use std::io::BufReader;

#[derive(Debug)]
struct WordCounter(HashMap<String, u64>);

impl WordCounter {
    fn new() -> WordCounter {
        WordCounter(HashMap::new())
    }

    fn increment(&mut self, word: &str) {
        let key = word.to_string();
        let count = self.0.entry(key).or_insert(0);
        *count += 1;
    }

    /// Show words with count greater than or equal to 'filter'.
    fn display(&self, filter: u64) {
        for (key, value) in self.0.iter() {
            if value < &filter {
                continue;
            }
            println!("{} : {}", key, value);
        }
    }
}

fn main() {
    let arguments: Vec<String> = env::args().collect();
    let filename = &arguments[1];
    println!("Processing file: {}", filename);
    let file = File::open(filename).expect("Could not open file");
    let reader = BufReader::new(file);
    let mut word_counter = WordCounter::new();

    for line in reader.lines() {
        let line = line.expect("Could not read line");
        let words = line.split(" ");
        for word in words {
            if word == "" {
                continue;
            } else {
                word_counter.increment(word);
            }
        }
    }
    word_counter.display(2);

    let mut vec = Vec::new();
    for (k, v) in word_counter.0 {
        vec.push((k, v));
    }

    vec.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap());
    println!("sorted: ");
    for (k, v) in vec.iter() {
        println!("{}: {}", k, v);
    }
    println!("");
    println!("{:?}", vec);
}
