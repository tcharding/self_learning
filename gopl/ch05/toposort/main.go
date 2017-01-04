// Copyright © 2016 Alan A. A. Donovan & Brian W. Kernighan.
// License: https://creativecommons.org/licenses/by-nc-sa/4.0/

// See page 136.

// The toposort program prints the nodes of a DAG in topological order.
package main

import (
	"fmt"
	"os"
	"sort"
)

// prereqTable maps computer science courses to their prerequisites.
var prereqTable = map[string][]string{
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
	"operating systems":     {"data structures", "computer organization"},
	"programming languages": {"data structures", "computer organization"},
	//	"linear algebra":        {"calculus"}, // introduces cycle
}

func main() {
	err := detectCycles(prereqTable)
	if err != nil {
		fmt.Fprintf(os.Stderr, "%v\n", err)
		os.Exit(1)
	}
	courseList := topoSort(prereqTable)
	err = verifyOrder(courseList)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: course list is incorrectly ordered: %v\n", err)
	}
	for i, course := range courseList {
		fmt.Printf("%d:\t%s\n", i+1, course)
	}
}

type Course string

// detectCyces: detect cyclical course prerequisites
func detectCycles(m map[string][]string) error {
	for course, prereqs := range m {
		for _, prereq := range prereqs {
			if Course(course).isPrereqOf(Course(prereq)) == true {
				return fmt.Errorf("cycle detected: (%v) <-> (%v)", course, prereq)
			}
		}
	}
	return nil
}

// topoSort: non-deterministic topological sort
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

	for k, _ := range m {
		visitAll([]string{k})
	}

	return order
}

// verifyOrder: verify course order fulfills all prerequisites.
func verifyOrder(courses []string) error {
	for i, course := range courses {
		done := vector(courses[:i])
		if done.containsAll(prereqTable[course]) == false {
			return fmt.Errorf("%s prerequisites not met", course)
		}
	}
	return nil // true
}

// isPrereqOf: true if c is prerequisite of d
func (c Course) isPrereqOf(d Course) bool {
	prereqs := prereqTable[string(d)]
	for _, prereq := range prereqs {
		if Course(prereq) == c {
			return true
		}
	}
	return false
}

type vector []string

// vector a contains all elements of vector b
func (a vector) containsAll(b vector) bool {
	for _, s := range b {
		if b.contains(s) == false {
			return false
		}
	}
	return true
}

// contains: true if v contains s
func (v vector) contains(s string) bool {
	var strings []string = v
	for _, sub := range strings {
		if sub == s {
			return true
		}
	}
	return false
}

// topoSortD: deterministic topological sort (from text)
func topoSortD(m map[string][]string) []string {
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
