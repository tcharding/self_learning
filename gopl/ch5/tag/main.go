// Exercise 5.17 get elements by tag name
package main

import (
	"fmt"
	"os"

	"github.com/tcharding/utils"

	"golang.org/x/net/html"
)

func main() {
	doc, err := html.Parse(os.Stdin)
	if err != nil {
		fmt.Fprintf(os.Stderr, "outline: %v\n", err)
		os.Exit(1)
	}

	for _, n := range ElementsByTagName(doc, "img") {
		for _, a := range n.Attr {
			if a.Key == "src" {
				fmt.Printf("image source: %s\n", a.Val)
			}
		}

	}

	//	headings := ElementsByTagName(doc, "h1", "h2", "h3", "h4")
}

// ElementsByTagName gets elements from html node tree with tag name
func ElementsByTagName(n *html.Node, tags ...string) []html.Node {
	var elements []html.Node
	if n.Type == html.ElementNode {
		//		fmt.Println(n.Data)
		if len(tags) == 0 || utils.SliceContainsString(tags, n.Data) {
			elements = append(elements, *n)
		}
	}

	for c := n.FirstChild; c != nil; c = c.NextSibling {
		elements = append(elements, ElementsByTagName(c, tags...)...)
	}

	return elements
}
