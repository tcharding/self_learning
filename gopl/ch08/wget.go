// Wget: download web site recursively
package main

import (
	"flag"
	"fmt"

	"github.com/tcharding/gopl/ch8/wget"
)

var depth = flag.Int("depth", 0, "Depth of links to crawl")

func main() {
	flag.Parse()
	err := wget.Start(*depth, flag.Args()[0])
	if err != nil {
		fmt.Printf("wget: %v\n", err)
	}
}
