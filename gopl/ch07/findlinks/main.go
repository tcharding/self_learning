// Findlinks1 prints the links in an HTML document read from standard input.
package main

import (
	"fmt"
	"os"

	"github.com/tcharding/gopl/ch7/reader"

	"golang.org/x/net/html"
)

const page = `
<html>
  <body>
      <ul>
        <li><a href="resume.html">Resume</a></li>
        <li><a href="http://github.com/tcharding">Github</a></li>
      </ul>
      <br>
  </body>
</html>
`

func main() {
	reader := reader.NewReader(page)
	doc, err := html.Parse(reader)
	if err != nil {
		fatalf("findlinks1: %v\n", err)
	}
	for _, link := range visit(nil, doc) {
		fmt.Println(link)
	}
}

// fatalf: print format string to standard out and exit
func fatalf(format string, v ...interface{}) {
	fmt.Fprintf(os.Stderr, format, v...)
	os.Exit(1)
}

// visit appends to links each link found in n and returns result.
func visit(links []string, n *html.Node) []string {
	if n.Type == html.ElementNode {
		if n.Data == "a" {
			for _, a := range n.Attr {
				if a.Key == "href" {
					links = append(links, a.Val)
				}
			}
		}
		if n.Data == "img" {
			for _, a := range n.Attr {
				if a.Key == "src" {
					links = append(links, a.Val)
				}
			}
		}
	}
	for c := n.FirstChild; c != nil; c = c.NextSibling {
		links = visit(links, c)
	}
	return links
}
