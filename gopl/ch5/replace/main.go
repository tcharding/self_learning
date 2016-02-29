// Exercise 5.9
package main

import (
	"fmt"
	"strings"
	"unicode"
)

type tokenizer struct {
	original     string // string to tokenize
	currentIndex int
	validToken   bool
	token        string
	leadingSpace string
}

func main() {
	s := "This is the $first attempt at $expand"
	fmt.Printf("%s\n", s)
	expanded := expand(s, fudge)
	fmt.Println(expanded)
}

// Replace replaces each occurrence of
func expand(s string, f func(string) string) string {
	ret := ""
	t := newTokenizer(s)

	for nextToken(&t) != false {
		ret += t.leadingSpace

		if isReplaceable(t.token) {
			ret += f(t.token[1:])
		} else {
			ret += t.token
		}
	}
	return ret
}

// fudge example function to pass into expand
func fudge(s string) string {
	return strings.ToUpper(s)
}

func newTokenizer(s string) tokenizer {
	var t tokenizer
	t.original = s
	t.currentIndex = 0
	t.validToken = false

	return t
}

// nextToken gets next token, returns false if no more tokens available
func nextToken(t *tokenizer) bool {
	if t.currentIndex == len(t.original) {
		t.validToken = false
		return t.validToken
	}
	var end int  // start and end of token
	var s string // working slice

	s = t.original[t.currentIndex:]

	i := 0
	var r rune
	var space string

	// get leading white space
	for i, r = range s {
		if unicode.IsSpace(r) {
			space += string(r)
		} else {
			break
		}
	}
	t.leadingSpace = s[:i]
	s = s[i:] // skip leading space
	t.currentIndex += i

	end = strings.Index(s, " ")

	if end == -1 {
		t.token = s
		t.currentIndex = len(t.original) // we are done
	} else {
		t.token = s[:end]
		t.currentIndex += end
	}
	t.validToken = true

	return true
}

func isReplaceable(s string) bool {
	return s[0] == '$'
}

func printToken(t *tokenizer) {
	fmt.Printf("Token:\noriginal:%s\nvalid:%t\ncurrentIndex:%d\ntoken:%s\nspace:(%s)\n",
		t.original, t.validToken, t.currentIndex, t.token, t.leadingSpace)
}
