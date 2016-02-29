// prereqs maps computer science courses to their prerequisites.
package main

import (
	"fmt"
	"os"
	"sort"
)

var prerequisiteMap = map[string][]string{
	"algorithms": {"data structures"},
	"calculus":   {"linear algebra"},
	"compilers": {
		"data structures",
		"formal languages",
		"computer organization",
	},
	"data structures":       {"discrete math"},
	"databases":             {"data structures"},
	"discrete math":         {"intro to programming"},
	"formal languages":      {"discrete math"},
	"networks":              {"operating systems"},
	"programming languages": {"data structures", "computer organization"},
	"linear algebra":        {"calculus"},
}

func main() {
	if hasCycles(prerequisiteMap) {
		fmt.Println("Map has cycles")
		os.Exit(1)
	}
	testSliceContains()
	result := topoSort(prerequisiteMap)
	if !isCorrect(result) {
		fmt.Println("topoSort failed")
	}

	for i, course := range result {
		fmt.Printf("%d:\t%s\n", i+1, course)
	}
}

func topoSort(m map[string][]string) []string {
	var order []string
	seen := make(map[string]bool)
	var visitAll func(items []string)
	visitAll = func(items []string) {
		for _, item := range items {
			if !seen[item] {
				seen[item] = true
				visitAll(m[item])
				order = append(order, item)
			}
		}
	}
	var keys []string
	for key := range m {
		keys = append(keys, key)
	}
	sort.Strings(keys)
	visitAll(keys)
	return order
}

// topSortWithMap Exercise 5.10
func topoSortWithMap(m map[string][]string) []string {
	var order []string
	seen := make(map[string]bool)
	var visitAll func(items []string)
	visitAll = func(items []string) {
		for _, item := range items {
			if !seen[item] {
				seen[item] = true
				visitAll(m[item])
				order = append(order, item)
			}
		}
	}
	var keys []string
	for key := range m {
		keys = append(keys, key)
	}
	//	sort.Strings(keys)
	visitAll(keys)
	return order
}

// isCorrect verifies result of topoSort is correct
func isCorrect(courses []string) bool {
	for i, course := range courses {
		previous := courses[:i]
		requires := prerequisiteMap[course]
		for _, r := range requires {
			if !sliceContains(previous, r) {
				return false
			}
		}
	}
	return true
}

// sliceContains returns true if this slice contains that string
func sliceContains(this []string, that string) bool {
	for _, s := range this {
		if s == that {
			return true
		}
	}
	return false
}

func testSliceContains() {
	var testStrings []string = []string{"this", "is", "the", "list", "of", "strings"}
	var presentStrings []string = []string{"this", "the", "strings"}
	var notPresentStrings []string = []string{"oh", "not", "present", "This", "stringS"}

	for _, present := range presentStrings {
		if !sliceContains(testStrings, present) {
			fmt.Printf("Error: present string failed: %s\n", present)
		}
	}
	for _, notPresent := range notPresentStrings {
		if sliceContains(testStrings, notPresent) {
			fmt.Printf("Error: not present string failed: %s\n", notPresent)
		}
	}
}

func hasCycles(m map[string][]string) bool {
	for course, prereqs := range m {
		for _, prereq := range prereqs {
			otherPrereqs := m[prereq]
			if sliceContains(otherPrereqs, course) {
				return true
			}
		}
	}
	return false
}
