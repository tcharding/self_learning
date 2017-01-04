// xmltree: read XML and create tree of nodes
package xmltree

import (
	"bytes"
	"encoding/xml"
	"fmt"
	"io"
)

//
// Come back to this after learning about reflection

type Node interface{}

type CharData string

type Element struct {
	Type xml.Name
	Attr []xml.Attr
	Kids []Node
}

func buildTree(r io.Reader) (Element, error) {
	var sp Stack
	var cur Element
	dec := xml.NewDecoder(r)

	for {
		tok, err := dec.Token()
		if err == io.EOF {
			return cur, fmt.Errorf("EOF reached, is XML well formed?")
		} else if err != nil {
			return cur, err

		}
		switch tok := tok.(type) {
		case xml.StartElement:
			// make a new node
			var ns []Node
			n := Element{tok.Name, tok.Attr, ns}
			if sp.Len() != 0 {
				cur.Kids = append(cur.Kids, n)
			}
			sp.Push(n)
			cur = n
		case xml.EndElement:
			// go up one level of node
			cur, err = sp.Pop()
			if err != nil {
				return cur, fmt.Errorf("XML parse error")
			}
			if sp.Len() == 0 {
				return cur, nil
			}
		case xml.CharData:
			n := CharData(tok)
			cur.Kids = append(cur.Kids, n)
		}
	}
}

func DumpTree(e Element) {
	recursivePrint(e, 0)
}

func recursivePrint(n Node, indent int) {
	printNode(n, indent)
	// if isElement(n) {
	// 	for _, child := range n.Kids {
	// 		recursivePrint(child, indent+2)
	// 	}
	// }

}

func printNode(n Node, indent int) {
	// we need reflection for this
}

func isElement(n Node) bool {
	// we need reflection for this
	return false
}

func indentString(n int) string {
	var buf bytes.Buffer
	for i := 0; i < n; i++ {
		buf.WriteString(" ")
	}
	return buf.String()
}
