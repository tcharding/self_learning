fn main() {
    println!("opp: Skeleton implementation.");
}

pub struct Post {
    state: Option<Box<dyn State>>,
    content: String,
}

impl Post {
    pub fn new() -> Post {
        Post {
            state: Some(Box::new(Draft {})),
            content: String::new(),
        }
    }

    pub fn add_text(&mut self, text: &str) {
        // FIXME: Is use of 'take()' correct here?
        if let Some(s) = self.state.take() {
            if s.can_add_text() {
                self.content.push_str(text);
            }
        }
    }

    pub fn content(&self) -> &str {
        self.state.as_ref().unwrap().content(&self)
    }

    pub fn request_review(&mut self) {
        if let Some(s) = self.state.take() {
            self.state = Some(s.request_review())
        }
    }

    pub fn approve(&mut self) {
        if let Some(s) = self.state.take() {
            self.state = Some(s.approve())
        }
    }

    pub fn reject(&mut self) {
        if let Some(s) = self.state.take() {
            self.state = Some(s.reject())
        }
    }
}

trait State {
    fn request_review(self: Box<Self>) -> Box<dyn State>;

    fn approve(self: Box<Self>) -> Box<dyn State>;

    fn content<'a>(&self, _post: &'a Post) -> &'a str {
        ""
    }

    fn reject(self: Box<Self>) -> Box<dyn State> {
        Box::new(Draft {})
    }

    fn can_add_text(self: Box<Self>) -> bool {
        false
    }
}

struct Draft {}

impl State for Draft {
    fn request_review(self: Box<Self>) -> Box<dyn State> {
        Box::new(PendingReview { reviewed: false })
    }

    fn approve(self: Box<Self>) -> Box<dyn State> {
        self
    }

    fn reject(self: Box<Self>) -> Box<dyn State> {
        self
    }

    fn can_add_text(self: Box<Self>) -> bool {
        true
    }
}

struct PendingReview {
    reviewed: bool,
}

impl PendingReview {}

impl State for PendingReview {
    fn request_review(self: Box<Self>) -> Box<dyn State> {
        self
    }

    // state change requires two approvals.
    fn approve(self: Box<Self>) -> Box<dyn State> {
        if self.reviewed {
            return Box::new(Published {});
        }
        Box::new(PendingReview { reviewed: true })
    }
}

struct Published {}

impl State for Published {
    fn request_review(self: Box<Self>) -> Box<dyn State> {
        self
    }

    fn approve(self: Box<Self>) -> Box<dyn State> {
        self
    }

    fn content<'a>(&self, post: &'a Post) -> &'a str {
        &post.content
    }
}
