// Anagram checks if strings are anagrams
package anagram

func IsAnagram(s, t string) bool {
	if len(s) != len(t) {
		return false
	}

	schars := mapFromString(s)
	tchars := mapFromString(t)

	for k, sv := range schars {
		if tv, ok := tchars[k]; !ok || sv != tv {
			return false
		}
	}
	return true
}

func mapFromString(s string) map[string]int {
	chars := make(map[string]int)
	for i := 0; i < len(s); i++ {
		chars[s[i:i+1]]++
	}
	return chars
}
