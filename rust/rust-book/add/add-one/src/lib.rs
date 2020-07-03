use rand;

/// Adds one to the given number.
///
/// # Example
///
/// ```
/// let x = 5;
/// let result = add_one::add_one(x);
///
/// assert_eq!(result, 6);
/// ```
pub fn add_one(x: i32) -> i32 {
    x + 1
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }

    #[test]
    fn it_adds_one() {
        assert_eq!(add_one(-3), -2);
    }
}
