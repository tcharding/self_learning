// Package links provides a link-extraction function.
package links

import (
	"fmt"
	"net/http"
	"os"
	"strings"

	"golang.org/x/net/html"
)

// Extract makes an HTTP GET request to the specified URL, parses the response
// as HTML, and returns the links in the HTML document.
func Extract(url string) ([]string, error) {
	resp, err := http.Get(url)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("getting %s: %s", url, resp.Status)
	}
	return extractFromResponse(resp)
}

func ExtractAndArchive(url string) ([]string, error) {
	resp, err := http.Get(url)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("getting %s: %s", url, resp.Status)
	}
	err = archiveURLResponse(url, resp)
	if err != nil {
		fmt.Printf("extractAndArchive: %v", err)
	}
	return extractFromResponse(resp)

}

func extractFromResponse(resp *http.Response) ([]string, error) {
	doc, err := html.Parse(resp.Body)
	if err != nil {
		return nil, fmt.Errorf("parsing HTML: %v", err)
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

// archiveURLResponse stores resp body, creating subdirectories as needed
func archiveURLResponse(url string, resp *http.Response) error {
	dir := subdirectory(url)
	filename := filename(url)
	if dir != "" {
		makeSubdirectories(dir)
	}
	f, err := os.Open(filename)
	if err != nil {
		return err
	}
	fmt.Fprintf(f, "%s", resp.Body)
	return nil
}

// forEachNode calls the function pre(x) and post(x) for each node x in the tree
// rooted at n. Both functions are optional.  pre is called before the children
// are visited (perorder) and post is called after (postorder).
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

// subdirectory returns the path as viewed from url root
// i.e www.website.com/test/path/test.html -> test/path/
func subdirectory(url string) string {
	basename := baseURL(url)
	startOfBasename := strings.Index(url, basename)
	endOfBasename := startOfBasename + len(basename)
	path := url[endOfBasename:]
	return path
}

func filename(url string) string {
	return ""
}

func makeSubdirectories(dir string) error {
	return nil
}

// baseURL i.e http://www.github.com returns github.com
func baseURL(url string) string {
	basename := url

	// remove protocol
	protocolIndex := strings.Index(basename, "://")
	if protocolIndex != -1 {
		basename = basename[protocolIndex+3:]
	}

	// remove www
	wwwIndex := strings.Index(basename, "www")
	if wwwIndex != -1 {
		basename = basename[wwwIndex+4:]
	}

	// remove subdirectories
	subIndex := strings.Index(basename, "/")
	if subIndex != -1 {
		basename = basename[:subIndex]
	}

	// add trailing slash
	if basename[len(basename)-1] != '/' {
		basename += "/"
	}
	return basename
}

// startOfBasename returns index
func startOfBasename(url string) int {
	basename := baseURL(url)
	basename = basename[:len(basename)-1] // remove trailing slash
	return strings.Index(url, basename)
}
