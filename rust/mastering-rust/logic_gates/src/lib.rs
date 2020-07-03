/// Implements a boolean AND gate, taking as input two bits and returning a bit as output.
pub fn and(x: u8, y: u8) -> u8 {
    match (x, y) {
        (1, 1) => 1,
        _ => 0,
    }
}

/// Implements a boolean XOR gate, taking as input two bits and returning a bit as output.
pub fn xor(x: u8, y: u8) -> u8 {
    match (x, y) {
        (1, 0) | (0, 1) => 1,
        _ => 0,
    }
}

#[cfg(test)]
mod tests {
    use crate::{and, xor};

    #[test]
    fn and_works() {
        assert_eq!(1, and(1, 1));
        assert_eq!(0, and(1, 0));
        assert_eq!(0, and(0, 1));
        assert_eq!(0, and(0, 0));
    }

    #[test]
    fn xor_works() {
        assert_eq!(0, xor(0, 0));
        assert_eq!(1, xor(1, 0));
        assert_eq!(1, xor(0, 1));
        assert_eq!(0, xor(1, 1));
    }
}
