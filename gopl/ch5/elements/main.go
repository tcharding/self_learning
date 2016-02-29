// Elements counts the elements in an HTML document
package main

import (
	"fmt"
	"os"

	"golang.org/x/net/html"
)

func main() {
	doc, err := html.Parse(os.Stdin)
	if err != nil {
		fmt.Fprintf(os.Stderr, "elements: %v\n", err)
		os.Exit(1)
	}
	elements := make(map[string]int)
	visit(elements, doc)

	for k, v := range elements {
		fmt.Printf("%s: %d\n", k, v)
	}
}

type elements map[string]int

// visit adds elements to map
func visit(e elements, n *html.Node) {
	if n.Type == html.ElementNode {
		e[n.Data]++
	}
	for c := n.FirstChild; c != nil; c = c.NextSibling {
		visit(e, c)
	}
}
