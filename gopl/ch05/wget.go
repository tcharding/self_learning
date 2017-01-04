// Wget: download web site recursively
package main

import (
	"fmt"
	"os"

	"github.com/tcharding/gopl/ch5/wget"
)

func main() {
	// Crawl a site breadth-first,
	if len(os.Args) != 2 {
		fmt.Fprintf(os.Stderr, "Usage: wget <url>")
		os.Exit(1)
	}
	err := wget.Start(os.Args[1])
	if err != nil {
		fmt.Printf("wget: %v\n", err)
	}
}
