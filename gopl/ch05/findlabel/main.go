// findlabel: find an HTML tag
package main

import (
	"fmt"
	"os"

	"golang.org/x/net/html"
)

func main() {
	doc, err := html.Parse(os.Stdin)
	if err != nil {
		fmt.Fprintf(os.Stdout, "outline: %v\n", err)
		os.Exit(1)
	}
	// test ElementByID
	n := ElementByID(doc, "type")
	fmt.Println(n.Data)
}

// ElementByID: return first node with attribute a
func ElementByID(n *html.Node, req string) *html.Node {
	hasAttribute := func(n *html.Node) bool {
		if n.Type == html.ElementNode {
			for _, a := range n.Attr {
				if a.Key == req {
					return true
				}
			}
		}
		return false
	}
	return forEachNode(n, hasAttribute, nil)
}

// forEachNode: calls pre(x) and post(x) if non nil,
// functions enable preorder and postorder tree traversal
// if either pre(x) or post(x) return true forEachNode returns current node
func forEachNode(n *html.Node, pre, post func(n *html.Node) bool) *html.Node {
	if pre != nil {
		if pre(n) == true {
			return n
		}
	}
	for c := n.FirstChild; c != nil; c = c.NextSibling {
		found := forEachNode(c, pre, post)
		if found != nil {
			return found
		}
	}
	if post != nil {
		if post(n) == true {
			return n
		}
	}
	return nil
}
