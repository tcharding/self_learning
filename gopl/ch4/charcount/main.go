package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"unicode"
	"unicode/utf8"
)

func main() {
	in := bufio.NewReader(os.Stdin)
	unichars, utflens, metrics := parseInput(in)
	dumpCountsLengthsAndMetrics(unichars, utflens, metrics)
}

func parseInput(in *bufio.Reader) (map[rune]int, []int, map[string]int) {
	unichars := make(map[rune]int) // counts of Unicode characters

	len := utf8.UTFMax + 1
	utflens := make([]int, len, len) // counts of character encoding lengths

	metrics := map[string]int{ // other metrics
		"numInvalid": 0,
		"numLetters": 0,
		"numDigits":  0,
	}

	for {
		r, n, err := in.ReadRune() // returns rune, nbytes, error
		if err == io.EOF {
			break
		}
		if err != nil {
			fmt.Fprintf(os.Stderr, "charcount: %v\n", err)
			os.Exit(1)
		}
		if r == unicode.ReplacementChar && n == 1 {
			metrics["numInvalid"]++
			continue
		}
		if unicode.IsLetter(r) {
			metrics["numLetters"]++
		}
		if unicode.IsDigit(r) {
			metrics["numDigits"]++
		}
		unichars[r]++
		utflens[n]++
	}

	return unichars, utflens, metrics
}

func dumpCountsLengthsAndMetrics(counts map[rune]int, utflens []int, metrics map[string]int) {
	fmt.Printf("rune\tcount\n")
	for c, n := range counts {
		fmt.Printf("%q\t%d\n", c, n)
	}
	fmt.Printf("length\tcount\n")
	for i, n := range utflens {
		if i > 0 {
			fmt.Printf("%d\t%d\n", i, n)
		}
	}
	for k, v := range metrics {
		fmt.Printf("\n%s: %d\n", k, v)
	}
}
