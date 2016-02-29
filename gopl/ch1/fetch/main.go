// Fetch prints the content found at a URL.
package main

import (
	"fmt"
	"io"
	"net/http"
	"os"
	"strings"
)

func main() {
	const (
		protocol = "http://"
	)
	if len(os.Args) == 1 {
		fmt.Printf("Usage: %s <url>\n", os.Args[0])
		os.Exit(1)
	}
	for _, url := range os.Args[1:] {
		if !strings.HasPrefix(url, protocol) {
			url = protocol + url
		}
		resp, err := http.Get(url)
		if err != nil {
			fmt.Fprintf(os.Stderr, "fetch: %v\n", err)
			os.Exit(1)
		}
		io.Copy(os.Stdout, resp.Body)
		resp.Body.Close()
		fmt.Printf("HTTP Status: %s\n", resp.Status)
	}
}
