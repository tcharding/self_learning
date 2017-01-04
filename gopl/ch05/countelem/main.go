//
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
		utils.Fatalf("countelem: %v\n", err)
	}

	m := make(map[string]int)
	countElem(m, doc)
	for k, v := range m {
		fmt.Printf("<%s>: %d\n", k, v)
	}
}

func countElem(m map[string]int, n *html.Node) {
	if n == nil {
		return // base case
	}
	if n.Type == html.ElementNode {
		m[n.Data]++
	}

	countElem(m, n.FirstChild)
	countElem(m, n.NextSibling)
}
