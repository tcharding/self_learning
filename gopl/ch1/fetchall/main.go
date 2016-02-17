// Fetchall fetches URL's in parallel and reports their times and sizes.
package main

import (
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"os"
	"strings"
	"time"
)

func main() {
	const (
		file = "out.fetchall"
	)
	start := time.Now()
	ch := make(chan string)

	if len(os.Args) == 1 {
		fmt.Printf("Usage: %s <url> [<url> ...]\n", os.Args[0])
		os.Exit(1)
	}

	for _, url := range os.Args[1:] {
		url = addPrefixIfRequired(url)
		go fetch(url, ch) // start a goroutine
	}
	f, err := os.Create(file)
	if err != nil {
		fmt.Fprintf(os.Stderr, "fetchall: %v\n", err)
		os.Exit(1)
	}
	for range os.Args[1:] {
		fmt.Fprintln(f, <-ch) // receive from channel ch
	}
	fmt.Fprintf(f, "%.2fs elapsed\n", time.Since(start).Seconds())
	f.Close()
}

func addPrefixIfRequired(url string) string {
	const (
		http  = "http://"
		https = "https://"
	)
	if !strings.HasPrefix(url, http) &&
		!strings.HasPrefix(url, https) {
		url = http + url
	}
	return url
}

func fetch(url string, ch chan<- string) {
	start := time.Now()
	resp, err := http.Get(url)
	if err != nil {
		ch <- fmt.Sprint(err) // send to channel ch
		return
	}

	nbytes, err := io.Copy(ioutil.Discard, resp.Body)
	resp.Body.Close()
	if err != nil {
		ch <- fmt.Sprintf("while reading %s: %v", url, err)
		return
	}
	secs := time.Since(start).Seconds()
	ch <- fmt.Sprintf("%.2fs %7d  %s", secs, nbytes, url)
}
