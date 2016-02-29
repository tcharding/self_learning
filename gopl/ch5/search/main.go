package main

import "fmt"

func main() {
	var worklist []string
	for course, _ := range prerequisiteMap {
		worklist = append(worklist, course)
	}
	courses := breadthFirst(prerequisitesForCourse, worklist)
	for _, course := range courses {
		fmt.Println(course)
	}
}

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

// breadthFirst calls f for each item in the worklist. Any items returned by f
// are added to the worklist. f is called at most once for each item.
func breadthFirst(f func(item string) []string, worklist []string) []string {
	seen := make(map[string]bool)
	for len(worklist) > 0 {
		items := worklist
		worklist = nil
		for _, item := range items {
			if !seen[item] {
				seen[item] = true
				worklist = append(worklist, f(item)...)
			}
		}
	}
	var items []string
	for item, _ := range seen {
		items = append(items, item)
	}
	return items
}

func prerequisitesForCourse(course string) []string {
	return prerequisiteMap[course]
}
