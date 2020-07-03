// Print song lyrics: The 12 days of Christmas

static TITLE: &'static str = "The 12 days of Christmas";

fn main() {
    print!("\n{}\n\n", TITLE);
    print_lyrics();
}

fn print_lyrics() {
    println!("On the First day of Christmas,");
    pr_my_true_love();
    print!("a partridge in a pear tree\n\n");

    for n in 2..13 {
        pr_day(n);
        pr_my_true_love();
        pr_gifts(n);
        println!("");
    }
}

fn pr_day(n: usize) {
    let numeric: [&str; 13] = [
        "0th-unused",
        "1st-unused",
        "Second",
        "Third",
        "Fourth",
        "Fifth",
        "Sixth",
        "Seventh",
        "Eighth",
        "Ninth",
        "Tenth",
        "Eleventh",
        "Twelfth",
    ];

    println!("On the {} day of Christmas,", numeric[n]);
}

fn pr_my_true_love() {
    println!("My true love gave to me");
}

fn pr_gifts(n: usize) {
    let lines: [&str; 12] = [
        "Twelve drummers drumming",
        "Eleven pipers piping",
        "Ten lords a-leaping",
        "Nine ladies dancing",
        "Eight maids a-milking",
        "Seven swans a-swimming",
        "Six geese a-laying",
        "Five golden rings",
        "Four calling birds",
        "Three french hens",
        "Two turtle doves",
        "and a partridge in a pear tree",
    ];

    let start = 12 - n as usize;
    let end: usize = 12 as usize;
    let print = &lines[start..end];

    for line in print {
        println!("{}", line);
    }
}
