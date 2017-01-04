package main

import (
	"fmt"
	"io"
	"os"

	"github.com/tcharding/gopl/ch2/popcount"
)

func main() {
	fmt.Println("Welcome to Population Count - number of set bits")
	for {
		var n int
		fmt.Printf("\nEnter an int: ")
		_, err := fmt.Scanf("%d", &n)

		if err != nil {
			if err == io.EOF {
				os.Exit(0)
			}
			fmt.Fprintf(os.Stderr, "error: %v\n", err)
			os.Exit(1)
		}

		pop := popcount.PopCount(uint64(n))
		fmt.Printf("pop count of %d: %d\n", n, pop)
	}
}
