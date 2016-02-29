package main

import (
	"fmt"
	"io"
	"net/http"
	"os"
	"path"
)

func main() {
	filename, n, err := Fetch("http://tobin.cc")
	if err != nil {
		fmt.Fprintf(os.Stderr, "fetch: %v", err)
		os.Exit(1)
	}
	fmt.Printf("Created file: %s size: %d", filename, n)
}

// Fetch downloads the URL and returns the name and length of the local file
func Fetch(url string) (filename string, n int64, err error) {
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
	defer func() {
		closeErr := f.Close()
		if closeErr != nil {
			if err == nil {
				err = closeErr
			}
		}
	}()
	if err != nil {
		return "", 0, err
	}
	n, err = io.Copy(f, resp.Body)
	// Close file but prefer error from Copy, if any.
	// if closeErr := f.Close; err == nil {
	// 	err = closeErr
	// }
	return local, n, err
}
