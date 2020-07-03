pub struct DraftPost {
    content: String,
}

impl DraftPost {
    pub fn add_text(&mut self, text: &str) {
        self.content.push_str(text);
    }

    pub fn replace_text(&mut self, text: &str) {
        self.content = text.to_string();
    }

    pub fn request_review(self) -> PendingFirstReviewPost {
        PendingFirstReviewPost {
            content: self.content,
        }
    }
}

pub struct PendingFirstReviewPost {
    content: String,
}

pub struct PendingSecondReviewPost {
    content: String,
}

impl PendingFirstReviewPost {
    pub fn approve(self) -> PendingSecondReviewPost {
        PendingSecondReviewPost {
            content: self.content,
        }
    }

    pub fn reject(self) -> DraftPost {
        DraftPost {
            content: self.content,
        }
    }
}

impl PendingSecondReviewPost {
    pub fn approve(self) -> Post {
        Post {
            content: self.content,
        }
    }

    pub fn reject(self) -> PendingSecondReviewPost {
        // Rejection of already reviewed post maintains the original review.
        self
    }
}

pub struct Post {
    content: String,
}

impl Post {
    pub fn new() -> DraftPost {
        DraftPost {
            content: String::new(),
        }
    }

    pub fn content(&self) -> &str {
        &self.content
    }
}
