package reverse

import (
	"fmt"
	"testing"
)

const verbose = true

func TestReverseInt(t *testing.T) {
	array := [...]int{0, 1, 2, 3, 4, 5}
	tc := array[:]
	fmt.Printf("Initial: \t\t %v\n", tc)

	var out string
	ReverseInt(tc)
	out = fmt.Sprintf("Reversed: \t\t %v \t %v\n", tc, array)
	testOut(out)

	RotateIntLeft(tc, 2)
	out = fmt.Sprintf("Rotated Left 2: \t %v\n", tc)
	testOut(out)

	RotateIntRight(tc, 2)
	out = fmt.Sprintf("Rotated Right 2: \t %v\n", tc)
	testOut(out)

	RotateIntRight(tc, 2)
	out = fmt.Sprintf("Rotated Right 2: \t %v\n", tc)
	testOut(out)

	rotated := RotateRight(tc, 3)
	out = fmt.Sprintf("Rotated Right 3: \t %v\n", rotated)
	testOut(out)

	testOut("\n")
}

func testOut(s string) {
	if verbose {
		fmt.Print(s)
	}
}

func TestRemoveAdjacentDuplicates(t *testing.T) {
	tc := []string{"a", "b", "c", "a", "a", "c"}
	testOut("Should print \n[a b c a c]\n")
	tc = RemoveAdjacentDuplicates(tc)
	out := fmt.Sprintf("%v\n", tc)
	testOut(out)
	testOut("\n")
}

func TestSquashWhitespace(t *testing.T) {
	tc := fmt.Sprintf("this  is\ta  test\n")
	fmt.Printf("Should print:\nthis is a test\n")
	squashed := SquashWhitespace(tc)
	fmt.Println(squashed)
	testOut("\n")
}

func TestReverseString(t *testing.T) {
	s := "this is the string"
	slice := []byte(s)
	fmt.Printf("%s\n", slice)

	ReverseByte(slice)
	fmt.Printf("%s\n", slice)
	testOut("\n")
}
