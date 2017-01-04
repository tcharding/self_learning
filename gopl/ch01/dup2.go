// Dup2 prints the text of each line that appears more than
// once in the input, preceded by its count. It reads from stdin
// or from a list of name files.
package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
)

func main() {
	counts := make(map[string][]string)
	files := os.Args[1:]
	if len(files) == 0 {
		countLines("stdin", os.Stdin, counts)
	} else {
		for _, arg := range files {
			f, err := os.Open(arg)
			if err != nil {
				fmt.Fprintf(os.Stderr, "dup2: %v\n", err)
				continue
			}
			countLines(arg, f, counts)
			f.Close()
		}
	}
	printCounts(counts)
}

func countLines(fileName string, f *os.File, counts map[string][]string) {
	input := bufio.NewScanner(f)
	for input.Scan() {
		line := input.Text()
		if _, ok := counts[line]; ok {
			incCount(counts, line)
			counts[line] = append(counts[line], fileName)
		} else {
			counts[line] = []string{"1", fileName}
		}

	}
	// NOTE: ignoring potential errors from input.Err()
}

func printCounts(counts map[string][]string) {

	for line, meta := range counts {
		n, err := strconv.Atoi(meta[0])
		if err != nil {
			log.Fatal(err)
		}
		if n > 1 {
			fmt.Printf("%s\t%s\t%v\n", meta[0], line, meta[1:])
		}
	}

}

func incCount(counts map[string][]string, key string) {
	oldIntVal, _ := strconv.Atoi(counts[key][0])
	newStrVal := strconv.Itoa(oldIntVal + 1)

	counts[key] = append([]string{newStrVal}, counts[key][1:]...)
}
