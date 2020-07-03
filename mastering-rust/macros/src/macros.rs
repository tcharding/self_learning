macro_rules! greet {
    (HELLO $recipient:expr) => {
        println!("Hello {}", $recipient)
    };
}

// $item must be an array.
macro_rules! html_list {
    ( $( $items:tt ),* ) => {
        let mut out = String::from("<ul>");
        for item in $item.iter() {
            out.push(format!("<li>{}</li>", item));
        }
        out.push("</ul>");
        &out
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hmtl_list_works() {
        let want = "<ul><li>1</li><li>2</li></ul>";
        let got = html_list!([1, 2]);
        assert_eq!(want, got);
    }
}
