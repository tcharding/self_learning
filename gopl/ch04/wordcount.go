// Wordcount computes counts of words
package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	counts := make(map[string]int)
	total := 0

	scanner := bufio.NewScanner(os.Stdin)
	scanner.Split(bufio.ScanWords)
	for scanner.Scan() {
		total++

		if err := scanner.Err(); err != nil {
			fmt.Fprintln(os.Stderr, "reading input:", err)
		}
		counts[scanner.Text()]++
	}

	for word, n := range counts {
		fmt.Printf("%d: %s\n", n, word)
	}
	fmt.Printf("\n%d total: \n", total)
}
