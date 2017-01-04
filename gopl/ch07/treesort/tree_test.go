package treesort

import "testing"

func TestString(tst *testing.T) {
	want := "[0 [] []]"
	var t tree
	if got := t.String(); got != want {
		tst.Errorf("nil tree, got: %s want: %s\n", got, want)
	}

	add(&t, 5)
	want = "[0 [] [5 [] []]]"
	if got := t.String(); got != want {
		tst.Errorf("nil tree, got: %s want: %s\n", got, want)
	}

	add(&t, -2)
	want = "[0 [-2 [] []] [5 [] []]]"
	if got := t.String(); got != want {
		tst.Errorf("nil tree, got: %s want: %s\n", got, want)
	}

}
