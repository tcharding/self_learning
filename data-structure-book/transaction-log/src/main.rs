use transaction_log::{ListIterator, TransactionLog};

fn main() {
    let mut log = TransactionLog::new();
    log.append(String::from("first"));
    log.append(String::from("second"));

    for msg in ListIterator::new(&log) {
        println!("{}", msg);
    }

    for msg in log.iter() {
        println!("{}", msg);
    }
}
