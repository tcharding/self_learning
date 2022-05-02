/// Program 2.1: Sequential search.
///
/// This function checks whether the number `v` is in `a[l]..a[r]` by comparing
/// against each number sequentially.
///
/// # Returns
///
/// The index of `v` if found, `None` otherwise.
fn sequential_search(a: Vec<i32>, v: i32, l: usize, r: usize) -> Option<usize> {
    for i in l..r {
        if v == a[i] {
            return Some(i);
        }
    }
    None
}

/// Program 2.1: Binary search.
///
/// This program has the same functionality as `sequential_search` but much is
/// more efficient.
fn binary_search(a: Vec<i32>, v: i32, mut l: usize, mut r: usize) -> Option<usize> {
    while r >= l {
        let m = (l + r) / 2;
        if v == a[m] {
            return Some(m);
        }
        if v < a[m] {
            r = m - 1;
        } else {
            l = m + 1;
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sequential_search_works() {
        let v = 3;
        let a = vec![1, 2, 3, 4];

        let want = 2;
        let got = sequential_search(a, v, 0, 4).expect("failed to find value");
        assert_eq!(got, want);
    }

    #[test]
    fn sequential_search_not_found() {
        let v = 10;
        let a = vec![1, 2, 3, 4];

        let res = sequential_search(a, v, 0, 4);
        assert_eq!(res, None);
    }

    #[test]
    fn binary_search_works() {
        let v = 3;
        let a = vec![1, 2, 3, 4];

        let want = 2;
        let got = binary_search(a, v, 0, 4).expect("failed to find value");
        assert_eq!(got, want);
    }

    #[test]
    fn binary_search_not_found() {
        let v = 10;
        let a = vec![1, 2, 3, 4];

        let res = sequential_search(a, v, 0, 4);
        assert_eq!(res, None);
    }
}
