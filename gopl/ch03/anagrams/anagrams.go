package anagrams

import "strings"

// Anagrams: true if s and t contain same letters in different order
func Anagrams(s, t string) bool {
	if len(s) != len(t) {
		return false
	}
	for _, c := range s {
		if strings.IndexRune(t, c) == -1 {
			return false
		}
	}
	return true
}
