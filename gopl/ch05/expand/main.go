// expand: replace $foo with f(foo)
package main

import (
	"bytes"
	"fmt"
	"strings"
)

func main() {
	input := "This is a $foo string"
	expanded := expand(input, strings.ToUpper)
	fmt.Println(expanded)
}

func expand(s string, f func(string) string) string {
	var b bytes.Buffer
	words := strings.Split(s, " ")

	for _, word := range words {
		if word[0] == '$' {
			b.WriteString(f(word[1:]))
		} else {
			b.WriteString(word)
		}
		b.WriteString(" ")
	}
	return b.String()
}
