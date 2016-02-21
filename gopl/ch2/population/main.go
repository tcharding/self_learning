package main

import (
	"fmt"

	"github.com/tcharding/gopl/ch2/popcount"
)

func main() {
	var i int
	fmt.Print("Enter number to count set bits: ")
	fmt.Scanf("%d", &i)
	fmt.Printf("Number of set bits (%d): %d\n", i, popcount.PopCountClear(uint64(i)))
}
