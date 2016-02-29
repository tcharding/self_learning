package main

import (
	"fmt"
	"log"
	"os"
	"strings"

	"github.com/tcharding/gopl/ch5/links"
)

func main() {
	breadthFirst(localLinks, os.Args[1:])
}

// breadthFirst calls f for each item in the worklist. Any items returned by f
// are added to the worklist. f is called at most once for each item.
func breadthFirst(f func(item string) []string, worklist []string) {
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
}

func localLinks(url string) []string {
	basename := links.baseURL(url)
	var local []string
	list, err := links.Extract(url)
	if err != nil {
		log.Println(err)
	}
	for _, checkURL := range list {
		index := strings.Index(checkURL, basename)
		if index != -1 {
			local = append(local, checkURL)
			fmt.Println(checkURL)
		}
	}
	return local
}
