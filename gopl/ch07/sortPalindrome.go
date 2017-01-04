package main

import (
	"fmt"
	"os"
	"sort"
)

func main() {
	var tests = []struct {
		input string
		want  bool
	}{
		{"isi", true},
		{"issi", true},
		{"isnt", false},
	}
	for _, test := range tests {
		word := sorter([]byte(test.input))
		if got := isPalindrome(word); got != test.want {
			fmt.Fprintf(os.Stderr, "isPalindrome(%v) returned %v\n", word.String(), got)
		}
	}
}

type sorter []byte

func (s sorter) Len() int { return len(s) }
func (s sorter) Less(i, j int) bool {
	return s[i] < s[j]
}
func (s sorter) Swap(i, j int) {
	s[i], s[j] = s[j], s[i]
}

func (s sorter) String() string {
	return string(s)
}

func isPalindrome(s sort.Interface) bool {
	for i, j := 0, s.Len()-1; i < s.Len(); i, j = i+1, j-1 {
		equal := !s.Less(i, j) && !s.Less(j, i)
		if !equal {
			return false
		}
	}
	return true
}

// func Test(t *testing.T) {
// 	var tests = []struct {
// 		input string
// 		want  string
// 	}{
// 		{"input", "output"},
// 	}
// 	for _, test := range tests {
// 		if got := function(test.input); got != test.want {
// 			t.Errorf("function(%q) = %v, want: %v",
// 				test.input, got, test.want)
// 		}
// 	}

// }
