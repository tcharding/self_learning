package main

import (
	"fmt"
	"os"

	"golang.org/x/net/html"
)

func main() {
	doc, err := html.Parse(os.Stdin)
	if err != nil {
		fmt.Fprintf(os.Stderr, "outline: %v\n", err)
		os.Exit(1)
	}
	//	node := ElementByID(doc, "img")
	//	printNode(node)
	outlineWithAnonymous(doc)
}

func outline(stack []string, n *html.Node) {
	if n.Type == html.ElementNode {
		stack = append(stack, n.Data) // push tag
		fmt.Println(stack)
	}
	for c := n.FirstChild; c != nil; c = c.NextSibling {
		outline(stack, c)
	}
}

// outlineWithAnonymous Exercise 5.12
func outlineWithAnonymous(n *html.Node) {

	var depth int

	start := func(n *html.Node) {
		if n.Type == html.ElementNode {
			fmt.Printf("%*s<%s>\n", depth*2, "", n.Data)
			depth++
		}
		if n.Type == html.TextNode {
			fmt.Printf("%*s%s", depth*2, "", n.Data)
		}
	}

	end := func(n *html.Node) {
		if n.Type == html.ElementNode {
			depth--
			fmt.Printf("%*s</%s>\n", depth*2, "", n.Data)
		}
	}
	forEachNode(n, start, end)
}

// forEachNode calls the function pre(x) and post(x) for each node x in the tree
// rooted at n. Both functions are optional.  pre is called before the children
// are visited (perorder) and post is called after (postorder).
func forEachNode(n *html.Node, pre, post func(n *html.Node)) {
	if pre != nil {
		pre(n)
	}
	for c := n.FirstChild; c != nil; c = c.NextSibling {
		forEachNode(c, pre, post)
	}
	if post != nil {
		post(n)
	}
}

var depth int

func startElement(n *html.Node) bool {
	if n.Type == html.ElementNode {
		fmt.Printf("%*s<%s>\n", depth*2, "", n.Data)
		depth++
	}
	if n.Type == html.TextNode {
		fmt.Printf("%*s%s", depth*2, "", n.Data)
	}
	return true
}

func endElement(n *html.Node) bool {
	if n.Type == html.ElementNode {
		depth--
		fmt.Printf("%*s</%s>\n", depth*2, "", n.Data)
	}
	return true
}

func printNode(n *html.Node) {
	fmt.Printf("Node: %s\n", n.Data)
}
