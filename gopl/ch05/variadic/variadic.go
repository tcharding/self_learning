// variadic: example variadic functions
package variadic

import (
	"bytes"
	"fmt"

	"golang.org/x/net/html"
)

// func max(vals ...int) int {
// 	if len(vals) == 0 {
// 		panic("cannot take max of nothing")
// 	}
// 	max := vals[0]
// 	for _, v := range vals {
// 		if v > max {
// 			max = v
// 		}
// 	}
// 	return max
// }

// func min(vals ...int) int {
// 	if len(vals) == 0 {
// 		panic("cannot take min of nothing")
// 	}
// 	min := vals[0]
// 	for _, v := range vals {
// 		if v < min {
// 			min = v
// 		}
// 	}
// 	return min
// }

func max(first int, rest ...int) int {
	max := first
	for _, v := range rest {
		if v > max {
			max = v
		}
	}
	return max
}

func min(first int, rest ...int) int {
	min := first
	for _, v := range rest {
		if v < min {
			min = v
		}
	}
	return min
}

// // join (string version)
// func join(v []string, sep string) string {
// 	var joined string
// 	for i, s := range v {
// 		if i != 0 {
// 			joined += sep
// 		}
// 		joined += s
// 	}
// 	return joined
// }

// join (byte version)
func join(v []string, sep string) string {
	var buf bytes.Buffer
	for i, s := range v {
		if i != 0 {
			buf.WriteString(sep)
		}
		buf.WriteString(s)
	}
	return buf.String()
}

func ElementsByTagName(doc *html.Node, names ...string) []*html.Node {
	fmt.Println(names)
	var elems []*html.Node
	visit := func(n *html.Node) {
		if n.Type == html.ElementNode {
			if vector(names).Contains(n.Data) {
				elems = append(elems, n)
			}
		}
	}
	forEachNode(doc, visit, nil)
	return elems
}

type vector []string

func (v vector) Contains(s string) bool {
	var strings []string = v
	for _, sub := range strings {
		if sub == s {
			return true
		}
	}
	return false
}

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
