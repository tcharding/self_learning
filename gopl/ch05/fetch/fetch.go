// fetch downloads the URL and returns the
// name and length of the local file
package main

import (
	"fmt"
	"io"
	"net/http"
	"os"
	"path"
)

func main() {
	if len(os.Args) == 1 {
		fmt.Fprintf(os.Stdout, "Usage: fetch URL")
	}
	url := os.Args[1]
	filename, n, err := fetch(url)
	if err != nil {
		fmt.Fprintf(os.Stdout, "fetch: %v\n", err)
	}
	fmt.Printf("fetched: %v\ncreated file: %v\nsize: %d bytes\n", url, filename, n)
}
func fetch(url string) (filename string, n int64, err error) {
	resp, err := http.Get(url)
	if err != nil {
		return "", 0, err
	}
	defer resp.Body.Close()

	local := path.Base(resp.Request.URL.Path)
	if local == "/" {
		local = "index.html"
	}
	f, err := os.Create(local)
	if err != nil {
		return "", 0, err
	}
	defer func() {
		if err == nil {
			err = f.Close()
		}
	}()
	n, err = io.Copy(f, resp.Body)
	// // Close file, but prefer error from Copy, if any.
	// if closeErr := f.Close(); err == nil {
	// 	err = closeErr
	// }
	return local, n, err
}
