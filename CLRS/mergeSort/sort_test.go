package mergeSort

import (
	"fmt"
	"testing"
)

var output bool = false

func TestSmallest(t *testing.T) {
	var tests = []struct {
		xs   []int
		ys   []int
		want []int
	}{
		{[]int{}, []int{}, []int{}},
		{[]int{}, []int{1}, []int{1}},
		{[]int{1}, []int{}, []int{1}},
		{[]int{1}, []int{4}, []int{1, 4}},
		{[]int{1, 2, 3}, []int{4, 5, 6}, []int{1, 2, 3, 4, 5, 6}},
	}

	for _, test := range tests {
		var x int
		for i := 0; i < len(test.want); i++ {
			if output {
				fmt.Printf("pre: %v %v\n", test.xs, test.ys)
			}
			x, test.xs, test.ys = smallest(test.xs, test.ys)
			if x != test.want[i] {
				t.Errorf("smallest() failed")
			}
			if output {
				fmt.Printf("post: %d %v %v\n\n", x, test.xs, test.ys)
			}

		}
	}
}

func TestMerge(t *testing.T) {
	var tests = []struct {
		input []int
		want  []int
		p     int
		q     int
		r     int
	}{
		{[]int{4}, []int{4}, 0, 1, 1},
		{[]int{4}, []int{4}, 0, 0, 1},
		{[]int{4, 6}, []int{4, 6}, 0, 1, 2},
		{[]int{4, 6}, []int{4, 6}, 0, 1, 2},
		{[]int{4, 6, 3, 9}, []int{3, 4, 6, 9}, 0, 2, 4},
		{[]int{4, 6, 3, 9, 10}, []int{3, 4, 6, 9, 10}, 0, 2, 5},
		{[]int{4, 6, 8, 2, 3, 9}, []int{2, 3, 4, 6, 8, 9}, 0, 3, 6},

		{[]int{3, 2, 1}, []int{3, 1, 2}, 1, 2, 3},
	}

	for _, test := range tests {
		xs := copy(test.input)
		merge(xs, test.p, test.q, test.r)
		if !equal(xs, test.want) {
			t.Errorf("merge failed\n input: %v\n got:  %v\nwant:  %v\n", test.input, xs, test.want)
		}
	}
}

func Test(t *testing.T) {
	fmt.Printf("Test\n")
	var tests = []struct {
		input []int
		want  []int
	}{
		{[]int{1}, []int{1}},

		{[]int{2, 1}, []int{1, 2}},
		{[]int{1, 2}, []int{1, 2}},

		{[]int{1, 2, 3}, []int{1, 2, 3}},
		{[]int{3, 2, 1}, []int{1, 2, 3}},
		{[]int{1, 3, 2}, []int{1, 2, 3}},

		{[]int{4, 5, 3, 2}, []int{2, 3, 4, 5}},
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
