// Copyright © 2016 Alan A. A. Donovan & Brian W. Kernighan.
// License: https://creativecommons.org/licenses/by-nc-sa/4.0/

// See page 241.

// Crawl2 crawls web links starting with the command-line arguments.
//
// This version uses a buffered channel as a counting semaphore
// to limit the number of concurrent calls to links.Extract.
package main

import (
	"flag"
	"fmt"
	"log"
	"net/http"
	"os"

	"golang.org/x/net/html"
)

var done = make(chan struct{})

func cancelled() bool {
	select {
	case <-done:
		return true
	default:
		return false
	}
}

//!+sema
// tokens is a counting semaphore used to
// enforce a limit of 20 concurrent requests.
var tokens = make(chan struct{}, 20)

func crawl(url string) []string {
	fmt.Println(url)
	tokens <- struct{}{} // acquire a token
	cancel := cancelled()
	resp, err := request(url, cancel) // Exercise 8.10, cannot see the use of cancel here?
	if err != nil {
		log.Print(err)
		return nil // fix this
	}
	list, err := Extract(resp)
	<-tokens // release the token

	if err != nil {
		log.Print(err)
	}
	return list
}

//!-sema

var maxDepth = flag.Int("depth", 0, "Depth of links to crawl")

type depthList struct {
	depth int
	list  []string
}

//!+
func main() {
	worklist := make(chan depthList)
	var n int // number of pending sends to worklist

	flag.Parse()

	// Start with the command-line arguments.
	n++
	go func() {
		worklist <- depthList{0, flag.Args()}
	}()

	// Cancel current http requests when input is detected
	go func() {
		os.Stdin.Read(make([]byte, 1)) // read a single byte
		close(done)
	}()

	// Crawl the web concurrently.
	seen := make(map[string]bool)
	for ; n > 0; n-- {
		dl := <-worklist
		if *maxDepth == 0 || dl.depth <= *maxDepth {
			for _, link := range dl.list {
				if !seen[link] {
					seen[link] = true
					n++
					go func(link string) {
						subDl := depthList{dl.depth + 1, crawl(link)}
						worklist <- subDl
					}(link)
				}
			}
		}
	}
}

func request(url string, cancel bool) (*http.Response, error) {
	req, _ := http.NewRequest("GET", url, nil)
	tr := &http.Transport{} // TODO: copy defaults from http.DefaultTransport
	client := &http.Client{Transport: tr}
	if cancel { //  Exercise 8.10, cannot see the use of cancel here?
		tr.CancelRequest(req)
		return nil, nil
	}
	return client.Do(req)
}

//  parse response as HTML, and returns the links in the HTML document.
func Extract(resp *http.Response) ([]string, error) {
	doc, err := html.Parse(resp.Body)
	resp.Body.Close()
	if err != nil {
		return nil, err
	}

	var links []string
	visitNode := func(n *html.Node) {
		if n.Type == html.ElementNode && n.Data == "a" {
			for _, a := range n.Attr {
				if a.Key != "href" {
					continue
				}
				link, err := resp.Request.URL.Parse(a.Val)
				if err != nil {
					continue // ignore bad URLs
				}
				links = append(links, link.String())
			}
		}
	}
	forEachNode(doc, visitNode, nil)
	return links, nil
}

//!-Extract

// Copied from gopl.io/ch5/outline2.
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

//!-
