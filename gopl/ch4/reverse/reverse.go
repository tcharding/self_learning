// Reverse slices
package reverse

import "unicode"

// Reverse int slice in place
func ReverseInt(s []int) {
	for i, j := 0, len(s)-1; i < j; i, j = i+1, j-1 {
		s[i], s[j] = s[j], s[i]
	}
}

// rotate in a single pass
func RotateRight(s []int, n int) []int {
	var rotated []int
	for i, _ := range s {
		src := (i + len(s) - n) % len(s)
		rotated = append(rotated, s[src])
	}
	return rotated
}

// Rotate int slice in place to the left
func RotateIntLeft(s []int, n int) {
	ReverseInt(s[:n])
	ReverseInt(s[n:])
	ReverseInt(s)
}

// Rotate int slice in place to the right
func RotateIntRight(s []int, n int) {
	ReverseInt(s)
	ReverseInt(s[:n])
	ReverseInt(s[n:])
}

func remove(slice []string, i int) []string {
	copy(slice[i:], slice[i+1:])
	return slice[:len(slice)-1]
}

func RemoveAdjacentDuplicates(s []string) []string {
	var i int
	for i = 1; i < len(s); i++ {
		if s[i] == s[i-1] {
			s = remove(s, i)
		}
	}
	return s[:i]
}

// squash all whitespace runs into single ASCII space character
func SquashWhitespace(s string) string {
	const space = " "
	ingap := false
	output := ""

	for _, char := range s {
		if !unicode.IsSpace(char) {
			ingap = false
			output += string(char)
		} else if !ingap {
			output += space
			ingap = true
		}
	}
	return output
}

func ReverseByte(b []byte) {
	for i, j := 0, len(b)-1; i < j; i, j = i+1, j-1 {
		b[i], b[j] = b[j], b[i]
	}
}
