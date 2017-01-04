package initial

import "testing"

func TestSum(t *testing.T) {
	xs := []int{0, 1, 2, 3, 4}
	var tests = []struct {
		i    int
		j    int
		want int
	}{
		{0, 5, 10},
		{1, 2, 1},
	}

	for _, test := range tests {
		if got := sum(xs, test.i, test.j); got != test.want {
			t.Errorf("sum(%d, %d) = %d want: %d\n", test.i, test.j, got, test.want)
		}
	}
}

func TestBruteForce(t *testing.T) {
	// answer xs[7:11], sum = 43
	xs := []int{13, -3, -25, 20, -3, -16, -23, 18, 20, -7, 12, -5, -22, 15, -4, 7}

	sum, slice := BruteForce(xs)
	if sum != 43 {
		t.Errorf("incorrect sum: %d\n", sum)
	}
	if ok := check(slice); !ok {
		t.Errorf("incorrect slice: %v\n", slice)
	}
}

func check(xs []int) bool {
	correct := []int{18, 20, -7, 12}
	if len(xs) != len(correct) {
		return false
	}

	for i, x := range xs {
		if x != correct[i] {
			return false
		}

	}
	return true
}
