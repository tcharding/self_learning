package variadic

import "testing"

// func TestMax(t *testing.T) {
// 	var tests = []struct {
// 		input []int
// 		want  int
// 	}{
// 		{[]int{-3, 0, 3}, 3},
// 		{[]int{0, -1, -2}, 0},
// 		{[]int{0, 1, 2, 3, 4}, 4},
// 	}
// 	for _, test := range tests {
// 		if got := max(test.input...); got != test.want {
// 			t.Errorf("function(%q) = %s", test.input)
// 		}
// 	}

// }

// func TestMin(t *testing.T) {
// 	var tests = []struct {
// 		input []int
// 		want  int
// 	}{
// 		{[]int{-3, 0, 3}, -3},
// 		{[]int{0, -1, -2}, -2},
// 		{[]int{0, 1, 2, 3, 4}, 0},
// 	}
// 	for _, test := range tests {
// 		if got := min(test.input...); got != test.want {
// 			t.Errorf("function(%q) = %s", test.input)
// 		}
// 	}

// }

func TestMax(t *testing.T) {
	var tests = []struct {
		inFirst int
		inRest  []int
		want    int
	}{
		{0, []int{-3, 0, 3}, 3},
		{0, []int{0, -1, -2}, 0},
		{0, []int{0, 1, 2, 3, 4}, 4},
	}
	for _, test := range tests {
		if got := max(test.inFirst, test.inRest...); got != test.want {
			t.Errorf("max(%q) = %d %v", test.inFirst, test.inRest)
		}
	}

}

func TestMin(t *testing.T) {
	var tests = []struct {
		inFirst int
		inRest  []int
		want    int
	}{
		{0, []int{-3, 0, 3}, -3},
		{0, []int{0, -1, -2}, -2},
		{0, []int{0, 1, 2, 3, 4}, 0},
	}
	for _, test := range tests {
		if got := min(test.inFirst, test.inRest...); got != test.want {
			t.Errorf("min(%q) = %d %v", test.inFirst, test.inRest)
		}
	}

}

func TestJoin(t *testing.T) {
	var tests = []struct {
		input []string
		sep   string
		want  string
	}{
		{[]string{"this", "is", "a", "vector"}, " ", "this is a vector"},
		{[]string{"this", "is", "a", "vector"}, ",", "this,is,a,vector"},
		{[]string{""}, " ", ""},
	}
	for _, test := range tests {
		if got := join(test.input, test.sep); got != test.want {
			t.Errorf("join(%v, %s) = %v", test.input, test.sep, got)
		}
	}
}

func TestContains(t *testing.T) {
	var tests = []struct {
		v    []string
		s    string
		want bool
	}{
		{[]string{"this", "is", "a", "vector"}, "vector", true},
		{[]string{"this", "is", "a", "vector"}, "that", false},
		{[]string{""}, "vector", false},
	}
	for _, test := range tests {
		if got := vector(test.v).Contains(test.s); got != test.want {
			t.Errorf("%v.Contains(%s) = %v\n", test.v, test.s, got)
		}
	}
}
