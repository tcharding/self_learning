// count number of different bits in two hashes
package main

import (
	"crypto/sha256"
	"fmt"
)

// pc[i] is the population count of i.
var pc [256]byte

func init() {
	for i := range pc {
		pc[i] = pc[i/2] + byte(i&1)
	}
}

func main() {
	a := "x"
	b := "X"

	hashA := sha256.Sum256([]byte(a))
	hashB := sha256.Sum256([]byte(b))

	fmt.Printf("bit difference: %d\n", (abs(PopCount(hashA) - PopCount(hashB))))
}

// PopCount: return number of set bits
func PopCount(buf [32]byte) int {
	count := 0
	for i := 0; i < len(buf); i++ {
		count += popCount(buf[i])
	}
	return count
}

// popCount: number of bits set in a byte
func popCount(b byte) int {
	return int(pc[b])
}

// abs: absolute value
func abs(n int) int {
	if n < 0 {
		return 0 - n
	}
	return n
}
