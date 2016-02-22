package anagram

import "testing"

func TestIsAnagramPositive(t *testing.T) {
	s1, s2 := "abcd", "dcba"
	if !IsAnagram(s1, s2) {
		t.Error("test failed")
	}
	s1, s2 = "dogcatherd", "catdogherd"
	if !IsAnagram(s1, s2) {
		t.Error("test failed")
	}
	s1, s2 = "d", "d"
	if !IsAnagram(s1, s2) {
		t.Error("test failed")
	}
	s1, s2 = "abcd", "abcd"
	if !IsAnagram(s1, s2) {
		t.Error("test failed")
	}
}

func TestIsAnagramNegative(t *testing.T) {
	s1, s2 := "abcd", "abc"
	if IsAnagram(s1, s2) {
		t.Error("test failed")
	}
	s1, s2 = "abcda", "abcd"
	if IsAnagram(s1, s2) {
		t.Error("test failed")
	}
	s1, s2 = "a", "ab"
	if IsAnagram(s1, s2) {
		t.Error("test failed")
	}
	s1, s2 = "", "a"
	if IsAnagram(s1, s2) {
		t.Error("test failed")
	}
}
