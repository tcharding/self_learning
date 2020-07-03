use logic_gates::{and, xor};

type Sum = u8;
type Carry = u8;

fn half_adder_input_output() -> Vec<((u8, u8), (Sum, Carry))> {
    vec![
        ((0, 0), (0, 0)),
        ((0, 1), (1, 0)),
        ((1, 0), (1, 0)),
        ((1, 1), (0, 1)),
    ]
}

/// Implements a half adder using primitive logic gates.
fn half_adder(x: u8, y: u8) -> (Sum, Carry) {
    (xor(x, y), and(x, y))
}

#[test]
fn one_bit_half_adder() {
    for (inn, out) in half_adder_input_output() {
        let (x, y) = inn;
        println!("Testing {}, {} -> {:?}", x, y, out);
        assert_eq!(out, half_adder(x, y));
    }
}
