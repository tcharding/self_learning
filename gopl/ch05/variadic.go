// variadic: play with variadic functions
package main

import (
	"fmt"
	"os"

	"github.com/tcharding/gopl/ch5/variadic"

	"golang.org/x/net/html"
)

func main() {
	doc, err := html.Parse(os.Stdin)
	if err != nil {
		fmt.Fprintf(os.Stderr, "variadic: %v\n", err)
		os.Exit(1)
	}
	nodes := variadic.ElementsByTagName(doc, "html", "h2", "b")
	for i, n := range nodes {
		fmt.Printf("%d: %s\n", i+1, n.Data)
	}
}
