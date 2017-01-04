// outline: print structure of HTML Node
package main

import (
	"fmt"
	"os"
	"strings"

	"golang.org/x/net/html"
)

func main() {
	doc, err := html.Parse(os.Stdin)
	if err != nil {
		fmt.Fprintf(os.Stdout, "outline: %v\n", err)
		os.Exit(1)
	}

	var depth int

	startElement := func(n *html.Node) bool {
		if n.Type == html.ElementNode {
			fmt.Printf("%*s<%s>\n", depth*2, "", n.Data)
			depth++
		}
		if n.Type == html.TextNode {
			fmt.Printf("%s\n", strings.TrimSpace(n.Data))
		}
		return true
	}

	endElement := func(n *html.Node) bool {
		if n.Type == html.ElementNode {
			depth--
			if n.Data == "br" {
				return true
			}

			fmt.Printf("%*s</%s>\n", depth*2, "", n.Data)
		}
		return true
	}

	forEachNode(doc, startElement, endElement)
}

// forEachNode: calls pre(x) and post(x) if non nil,
// functions enable preorder and postorder tree traversal
func forEachNode(n *html.Node, pre, post func(n *html.Node) bool) {
	if pre != nil {
		if pre(n) == false {
			return
		}
	}

	for c := n.FirstChild; c != nil; c = c.NextSibling {
		forEachNode(c, pre, post)
	}

	if post != nil {
		if post(n) == false {
			return
		}

	}
}

func requiresNewline(tag string) bool {
	no := []string{"br", "b", "li", "i"}

	for _, t := range no {
		if t == tag {
			return false
		}
	}
	return true
}
