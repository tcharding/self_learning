use blog::Post;

fn main() {
    let mut post = Post::new();

    post.add_text("I ate a salad for lunch today");

    let post = post.request_review();
    let post = post.reject();

    // Simulate fix content here
    let mut post = post;
    post.replace_text("I ate a beef for lunch today");

    let post = post.request_review();
    let post = post.approve();

    let post = post.reject();
    let post = post.approve();

    assert_eq!("I ate a beef for lunch today", post.content());
}
