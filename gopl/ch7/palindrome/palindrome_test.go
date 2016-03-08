package palindrome

import "testing"

// The standard library already has one of these see sort.Strings
type sortableString []string

func (s sortableString) Len() int           { return len(s) }
func (s sortableString) Less(i, j int) bool { return s[i] < s[j] }
func (s sortableString) Swap(i, j int)      { s[i], s[j] = s[j], s[i] }

func TestIsPalindrome(t *testing.T) {
	var is sortableString = []string{"is", "pal", "is"}
	var isnot sortableString = []string{"is", "not", "pal"}
	if !IsPalindrome(is) {
		t.Errorf("failed: %s\n", is)
	}
	if IsPalindrome(isnot) {
		t.Errorf("failed: %s\n", isnot)
	}

}
