package unionFind

import (
	"bytes"
	"testing"
)

type function func()

func test(t *testing.T, input, want string, fn function) {
	out = new(bytes.Buffer)
	in = bytes.NewBufferString(input)
	fn()

	if got := out.(*bytes.Buffer).String(); got != want {
		t.Errorf("\n\ninput: %s\n  got: \n%s\n want: \n%s\n\n",
			input, got, want)
	}
}

func Test(t *testing.T) {
	input := `3 4
4 9
8 0
2 3
5 6
2 9
5 9
7 3
4 8
5 6
0 2
6 1`
	want := `3 4
4 9
8 0
2 3
5 6
5 9
7 3
4 8
6 1
`
	functions := []function{
		quickFind, quickUnion, quickUnionWeighted, quickUnionPathCompressionByHalving,
	}
	for _, fn := range functions {
		test(t, input, want, fn)
	}
}
