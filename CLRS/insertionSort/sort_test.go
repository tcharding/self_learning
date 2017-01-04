package insertionSort

import "testing"

func Test(t *testing.T) {
	var tests = []struct {
		input []int
		want  []int
	}{
		{[]int{8, 4, 6, 3, 2, 0, 9, 1, 5, 7}, []int{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}},
		{[]int{8, -4, 6, 3, -2, 0, 9, 1, 5, 7}, []int{-4, -2, 0, 1, 3, 5, 6, 7, 8, 9}},
	}

	for _, test := range tests {
		xs := copy(test.input)
		Sort(xs)
		if !equal(xs, test.want) {
			t.Errorf("sort failed\ninput: %v\n got:  %v\nwant:  %v\n", test.input, xs, test.want)
		}
	}
}

func equal(xs, ys []int) bool {
	for i := range xs {
		if ys[i] != xs[i] {
			return false
		}
	}
	return true
}

func copy(xs []int) []int {
	ys := make([]int, len(xs))
	for i := range xs {
		ys[i] = xs[i]
	}
	return ys
}
