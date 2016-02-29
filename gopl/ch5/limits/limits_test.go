package limits

import "testing"

func TestMax(t *testing.T) {
	check := func(vals []int, exp int) {
		got, err := max(vals...)
		if err != nil {
			t.Errorf("Fail: max() error: %v\n", err)
		}
		if got != exp {
			t.Errorf("Fail: max() for vals: %v exp: %d got: %d\n",
				vals, exp, got)
		}
	}
	var tc []int

	tc = []int{1, 3, 2}
	check(tc, 3)
	tc = []int{1, 3, 2, 1}
	check(tc, 3)
	tc = []int{1, -3, 2}
	check(tc, 2)
}

func TestMin(t *testing.T) {
	check := func(vals []int, exp int) {
		got, err := min(vals...)
		if err != nil {
			t.Errorf("min() error: %v\n", err)
		}
		if got != exp {
			t.Errorf("min(): vals: %v exp: %d got: %d\n",
				vals, exp, got)
		}
	}
	var tc []int

	tc = []int{1, 3, 2}
	check(tc, 1)
	tc = []int{1, 3, 2, 1}
	check(tc, 1)
	tc = []int{1, -3, 2}
	check(tc, -3)

}

func TestMaxAtLeastOne(t *testing.T) {
	check := func(vals []int, exp int) {
		got := maxAtLeastOne(vals[0], vals...)
		if got != exp {
			t.Errorf("Fail: max() for vals: %v exp: %d got: %d\n",
				vals, exp, got)
		}
	}
	var tc []int

	tc = []int{1, 3, 2}
	check(tc, 3)
	tc = []int{1, 3, 2, 1}
	check(tc, 3)
	tc = []int{1, -3, 2}
	check(tc, 2)
}

func TestMinAtLeastOne(t *testing.T) {
	check := func(vals []int, exp int) {
		got := minAtLeastOne(vals[0], vals...)
		if got != exp {
			t.Errorf("min(): vals: %v exp: %d got: %d\n",
				vals, exp, got)
		}
	}
	var tc []int

	tc = []int{1, 3, 2}
	check(tc, 1)
	tc = []int{1, 3, 2, 1}
	check(tc, 1)
	tc = []int{1, -3, 2}
	check(tc, -3)

}
