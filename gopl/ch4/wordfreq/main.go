package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	counts := make(map[string]int)
	files := os.Args[1:]
	if len(files) == 0 {
		countWordFreq(os.Stdin, counts)
	} else {
		for _, arg := range files {
			f, err := os.Open(arg)
			if err != nil {
				fmt.Fprintf(os.Stderr, "wordfreq: %v\n", err)
				continue
			}
			countWordFreq(f, counts)
			f.Close()
		}
	}
	for word, n := range counts {
		if n > 1 {
			fmt.Printf("%d\t%s\n", n, word)
		}
	}
}

func countWordFreq(f *os.File, counts map[string]int) {
	scanner := bufio.NewScanner(f)
	scanner.Split(bufio.ScanWords)
	for scanner.Scan() {
		counts[scanner.Text()]++
	}
}
