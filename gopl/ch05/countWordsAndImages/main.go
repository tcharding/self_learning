// countWordsAndImages
package main

import (
	"fmt"
	"net/http"
	"strings"

	"github.com/tcharding/utils"

	"golang.org/x/net/html"
)

func main() {
	words, images, err := CountWordsAndImages("http://tobin.cc")
	if err != nil {
		utils.FatalErr("countWordsAndImages", err)
	}
	fmt.Printf("images: %d words: %d\n", images, words)
}

func CountWordsAndImages(url string) (words, images int, err error) {
	resp, err := http.Get(url)
	if err != nil {
		return
	}
	doc, err := html.Parse(resp.Body)
	resp.Body.Close()
	if err != nil {
		err = fmt.Errorf("parsing HTML:; %s", err)
		return
	}
	words, images = countWordsAndImages(doc)
	return
}

func countWordsAndImages(n *html.Node) (words, images int) {
	if n == nil {
		return
	}
	if n.Type == html.ElementNode && n.Data == "img" {
		images++
	}

	if n.Type == html.TextNode {
		words += countWords(n.Data)
	}

	w, i := countWordsAndImages(n.FirstChild)
	words += w
	images += i

	w, i = countWordsAndImages(n.NextSibling)
	words += w
	images += i

	return words, images
}

func countWords(s string) int {
	return len(strings.Split(s, " "))
}
