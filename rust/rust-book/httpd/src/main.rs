extern crate thread_pool;

use std::cell::RefCell;
use std::fs;
use std::io::prelude::*;
use std::net::TcpListener;
use std::net::TcpStream;
use std::thread;
use std::time::Duration;
use thread_pool::ThreadPool;

fn main() {
    let listener = TcpListener::bind("127.0.0.1:7878").unwrap();
    let (sender, _pool) = ThreadPool::fixed_size(10);

    for stream in listener.incoming() {
        let stream = stream.unwrap();
        let task = ThreadTask::new(RefCell::new(stream));

        sender.send(task).expect("failed to send task to thread.");
    }
}

// From thread_pool
// pub trait Task: Send + 'static {
//     /// Run the task
//     fn run(self);
// }

struct ThreadTask {
    stream: RefCell<TcpStream>,
}

impl ThreadTask {
    pub fn new(stream: RefCell<TcpStream>) -> ThreadTask {
        ThreadTask { stream }
    }
}

impl thread_pool::Task for ThreadTask {
    fn run(self) {
        let mut buffer = [0; 512];
        let mut stream = self.stream.borrow_mut();

        stream.read(&mut buffer).unwrap();

        let get = b"GET / HTTP/1.1\r\n";
        let sleep = b"GET /sleep HTTP/1.1\r\n";

        let (status_line, filename) = if buffer.starts_with(get) {
            ("HTTP/1.1 200 OK\r\n\r\n", "hello.html")
        } else if buffer.starts_with(sleep) {
            thread::sleep(Duration::from_secs(5));
            ("HTTP/1.1 200 OK\r\n\r\n", "hello.html")
        } else {
            ("HTTP/1.1 404 NOT FOUND\r\n\r\n", "404.html")
        };

        let contents = fs::read_to_string(filename).unwrap();
        let response = format!("{}{}", status_line, contents);

        stream.write(response.as_bytes()).unwrap();
        stream.flush().unwrap();
    }
}
