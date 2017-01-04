// Fetchall fetches URLs in parallel and reports their times and sizes.
package main

import (
	"bufio"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"os"
	"strings"
	"time"
)

func main() {
	start := time.Now()
	ch := make(chan string)
	outf := os.Args[1]

	f, err := os.Create(outf)
	if err != nil {
		fmt.Fprintf(os.Stderr, "fetchall: error creating file: %s", outf)
		os.Exit(1)
	}

	for _, url := range os.Args[2:] {
		if !strings.HasPrefix(url, "http://") {
			url = "http://" + url
		}
		go fetch(url, ch) // start a goroutine
	}

	// ignore write errors
	w := bufio.NewWriter(f)

	for range os.Args[2:] {
		out := <-ch + "\n" // receive from channel ch
		w.WriteString(out)
	}
	out := fmt.Sprintf("%.2fs elapsed\n", time.Since(start).Seconds())
	w.WriteString(out)

	f.Sync()
	w.Flush()
}

func fetch(url string, ch chan<- string) {
	start := time.Now()
	resp, err := http.Get(url)
	if err != nil {
		ch <- fmt.Sprint(err)
		return
	}

	nbytes, err := io.Copy(ioutil.Discard, resp.Body)
	resp.Body.Close()
	if err != nil {
		ch <- fmt.Sprintf("while reading %s: %v", url, err)
		return
	}

	secs := time.Since(start).Seconds()
	ch <- fmt.Sprintf("%.2fs %7d %s", secs, nbytes, url)
}
