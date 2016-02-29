// Text prints the contents of all text elements of an HTML document.
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
	visit(doc)
}

// visit recursively visits elements of an HTML document
func visit(n *html.Node) {
	if n.Type == html.TextNode {
		fmt.Printf("%s", n.Data)
	}
	if n.Type == html.ElementNode && n.Data == "img" {
		for _, a := range n.Attr {
			if a.Key == "src" {
				fmt.Printf("Image link: %s\n", a.Val)
			}
		}
	}
	for c := n.FirstChild; c != nil; c = c.NextSibling {
		if n.Type == html.ElementNode {
			if c.Data == "script" || c.Data == "style" {
				continue
			}
		}
		visit(c)
	}

}
