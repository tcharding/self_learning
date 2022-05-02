use anyhow::{bail, Result};
use std::io;

fn main() -> Result<()> {
    quick_find()?;
    Ok(())
}

/// Arbitrary maximum value for p and q.
const N: usize = 10;

/// Program 1.1: Quick-find solution to connectivity problem.
fn quick_find() -> Result<()> {
    let mut id: Vec<usize> = Vec::with_capacity(N);

    for i in 0..N {
        id.push(i);
    }

    loop {
        match read_input_values()? {
            None => return Ok(()),
            Some((p, q)) => {
                if id[p] == id[q] {
                    continue;
                }
                let t = id[p];
                for i in 0..N {
                    if id[i] == t {
                        id[i] = id[q];
                    }
                }
                println!("{} {}", p, q);
            }
        }
    }
}

/// Reads two integers from stdin, enter an empty line to end input.
///
/// This function is the equivalent of the C code: `scanf("%d %d\n", &p, &q)`
/// with added Rust-isms and error handling.
///
/// # Returns
///
/// - Ok(None): if input is finished.
/// - Ok(Some(p, q)): if input was read successfully.
/// - Err(anyhow::Error): if there was an IO error.
fn read_input_values() -> Result<Option<(usize, usize)>> {
    let mut line = String::new();
    let stdin = io::stdin();
    stdin.read_line(&mut line)?;

    let inputs: Vec<u32> = line
        .split_whitespace()
        .map(|x| x.parse().expect("not an integer"))
        .collect();

    if inputs.is_empty() {
        return Ok(None);
    }

    if inputs.len() != 2 {
        bail!("invalid input");
    }

    let p = inputs[0] as usize;
    let q = inputs[1] as usize;

    if p >= N {
        bail!("p is too big {}, max: {}", p, N);
    }
    if q >= N {
        bail!("q is too big {}, max: {}", q, N);
    }

    Ok(Some((p, q)))
}
