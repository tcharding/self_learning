package treesort

import (
	"fmt"
	"testing"
)

func TestString(t *testing.T) {
	var tp *tree

	tp = add(tp, 5)
	tp = add(tp, 1)
	tp = add(tp, 7)
	tp = add(tp, 3)

	fmt.Println(tp)
}
