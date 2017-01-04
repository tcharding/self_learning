package anagrams

import "testing"

func TestAnagrams(t *testing.T) {
	var tests = []struct {
		inputA string
		inputB string
		want   bool
	}{
		{"abc", "def", false},
		{"abc", "cbad", false},
		{"abc", "ab", false},
		{"abc", "", false},

		{"abc", "cba", true},
		{"abc", "abc", true},
		{"abc", "bca", true},
		{"", "", true},
	}
	for _, test := range tests {
		if got := Anagrams(test.inputA, test.inputB); got != test.want {
			t.Errorf("anagrams(%q %q) = %v", test.inputA, test.inputB, got)
		}
	}
}
