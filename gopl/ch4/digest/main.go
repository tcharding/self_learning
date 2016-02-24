// Fun with digests
package main

import (
	"crypto/sha256"
	"fmt"
)

// set[i] is the number of set bits in int 'i'
var set [256]byte

func init() {
	for i := range set {
		set[i] = set[i/2] + byte(i&1)
	}
}

func numBitsSet(digest *[32]byte) int {
	num := 0
	for _, byte := range digest {
		val := int(byte)
		num += int(set[val])
	}
	return num
}

func main() {
	c1 := sha256.Sum256([]byte("x"))
	c1BitsSet := numBitsSet(&c1)
	c2 := sha256.Sum256([]byte("X"))
	c2BitsSet := numBitsSet(&c2)
	fmt.Printf("digest type: %T\n", c1)
	fmt.Printf("c1: %x (bits set: %d)\n", c1, c1BitsSet)
	fmt.Printf("c2: %x (bits set: %d)\n", c2, c2BitsSet)
	dif := c2BitsSet - c1BitsSet
	fmt.Printf("hashes of 'x' and 'X' differ by %d bits\n", dif)
}
