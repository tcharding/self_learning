package popcount

import "testing"

func Test(t *testing.T) {
	var tests = []struct {
		input int
		want  int
	}{
		{1, 1},
		{3, 2},
	}
	for _, test := range tests {
		if got := PopCount(1); got != 1 {
			t.Errorf("PopCount(%d) = %d want: %d\n", test.input, got, test.want)
		}
	}

}
