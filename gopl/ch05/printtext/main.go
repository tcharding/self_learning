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
		utils.Fatalf("printtext: %v\n", err)
	}
	printText(doc)
}

func printText(n *html.Node) {
	if n == nil {
		return
	}
	if n.Type == html.ElementNode && n.Data == "li" {
		fmt.Printf("* ")
	}
	if n.Type == html.TextNode {
		fmt.Printf("%s", n.Data)
	}
	printText(n.FirstChild)
	printText(n.NextSibling)
}
