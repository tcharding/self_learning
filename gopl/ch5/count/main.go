package main

import (
	"fmt"
	"net/http"
	"os"
	"unicode"

	"golang.org/x/net/html"
)

func main() {
	if len(os.Args) != 2 {
		fmt.Printf("Usage: count <url>\n")
		os.Exit(1)
	}
	words, images, err := CountWordsAndImages(os.Args[1])
	if err != nil {
		fmt.Printf("error: %s\n", err)
		os.Exit(1)
	}
	fmt.Printf("words: %d images: %d\n", words, images)
}

// CountWordsAndImages does an HTTP GET request for the HTML
// document url and returns the number of words and images in it.
func CountWordsAndImages(url string) (words, images int, err error) {
	resp, err := http.Get(url)
	if err != nil {
		return
	}
	doc, err := html.Parse(resp.Body)
	resp.Body.Close()
	if err != nil {
		err = fmt.Errorf("parsing HTML: %s", err)
		return
	}
	words, images = countWordsAndImages(doc)
	return
}

func countWordsAndImages(n *html.Node) (words, images int) {

	if n.Type == html.TextNode {
		words += countWords(n.Data)
	} else {
		if n.Type == html.ElementNode && n.Data == "img" {
			images++
		}
	}
	for c := n.FirstChild; c != nil; c = c.NextSibling {
		w, i := countWordsAndImages(c)
		words += w
		images += i
	}

	return words, images
}

// countWords counts number of words in string
func countWords(s string) int {
	numWords := 0
	inWord := !unicode.IsSpace(rune(s[0]))
	if inWord {
		numWords = 1
	}

	for _, rune := range s {
		if unicode.IsSpace(rune) {
			if inWord {
				inWord = false
			}
		} else {
			if !inWord {
				numWords++
				inWord = true
			}
		}
	}
	return numWords
}

func testCountWords() {
	strings := []string{
		" ",
		"aou",
		"oaeu aoue",
		"aoeu aou aoe ",
		" aoeu aou aoe stst ",
	}

	for i, s := range strings {
		res := countWords(s)
		if res != i {
			fmt.Printf("Error. loop: %d res: %d\n", i, res)
		}
	}
}
