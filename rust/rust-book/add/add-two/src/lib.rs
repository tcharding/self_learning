/// Adds two to the number provided.
///
/// # Example
///
/// ```
/// let x = 5;
/// let result = add_two::add_two(x);
///
/// assert_eq!(result, 7);
/// assert_eq!(add_two::add_two(-1), 1);
/// ```
pub fn add_two(x: i32) -> i32 {
    x + 2
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }

    #[test]
    fn it_adds_two() {
        assert_eq!(add_two(-2), 0);
    }

}
