package mergeSort

// Sort xs in place
func Sort(xs []int) {
	mergeSort(xs, 0, len(xs))
}

// mergeSort: sort xs[p:r] in place
func mergeSort(xs []int, p, r int) {
	// if p >= r-1 then sub array has at most one element
	if p < r-1 {
		q := (p + r) / 2
		mergeSort(xs, p, q)
		mergeSort(xs, q, r)
		merge(xs, p, q, r)
	}
}

// merges xs[p:q] ad xs[q:r] in place
// pre: sub arrays must be already sorted
func merge(xs []int, p, q, r int) {
	// copy values to auxiliary arrays
	left := copy(xs[p:q])
	right := copy(xs[q:r])
	var x int
	for i := 0; i < len(xs[p:r]); i++ {
		x, left, right = smallest(left, right)
		xs[p+i] = x
	}
}

// return copy of xs
func copy(xs []int) []int {
	ys := make([]int, len(xs))
	for i := range xs {
		ys[i] = xs[i]
	}
	return ys
}

// smallest removes smallest integer and shrinks slice containing it
// returns the smallest integer and the resulting slices
func smallest(xs, ys []int) (int, []int, []int) {
	var shrink []int
	var leave []int
	switch {
	case len(xs) == 0:
		shrink = ys
		leave = xs
	case len(ys) == 0:
		shrink = xs
		leave = ys
	case xs[0] < ys[0]:
		shrink = xs
		leave = ys
	default:
		shrink = ys
		leave = xs
	}
	return shrink[0], leave, shrink[1:]
}
