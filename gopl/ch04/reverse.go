// slice reverse algorithms
package main

import (
	"fmt"
	"unicode"
	"unicode/utf8"
)

func main() {
	// Exercise 4.3 and 4.4
	// data := []int{1, 2, 3, 4, 5, 6}
	// reverse(data)
	// rotateSP(data, 2)
	// fmt.Println(data)

	// Exercise 4.5
	// v := []string{"this", "is", "the", "the", "vector"}
	// v = rmAdjacentDup(v)
	// fmt.Println(v)

	// Exercise 4.6
	// data := "this has   long    runs of    spaces"
	// squashed := sqush([]byte(data))
	// fmt.Println(string(squashed))

	// Exercise 4.7
	s := "abcdef"
	rev := reverseUtf([]byte(s))
	fmt.Println(string(rev))
}

// reverse slice in place
func reverse(s []int) {
	for i, j := 0, len(s)-1; i < j; i, j = i+1, j-1 {
		s[i], s[j] = s[j], s[i]
	}
}

// rotate s left by n positions
func rotate(s []int, n int) {
	reverse(s[:n])
	reverse(s[n:])
	reverse(s)
}

// rotate s left by n positions, single pass version
func rotateSP(s []int, n int) {
	save := make([]int, n)
	for i := 0; i < len(s)-n; i++ {
		if i < n {
			save[i] = s[i]
		}
		s[i] = s[i+n]
	}
	copy(s[i:], save)
}

// rmAdjacentDup: remove adjacent duplicate strings
func rmAdjacentDup(v []string) []string {
	keepIndex := 0
	for i := 0; i < len(v)-1; i++ {
		if v[i] != v[i+1] {
			v[keepIndex] = v[i] // in place
			keepIndex++
		}
	}
	v[keepIndex] = v[len(v)-1]
	return v[:keepIndex+1]
}

// sqaush: squash multiple adjacent unicode spaces into single space
func squash(p []byte) []byte {
	space := false
	keepIndex := 0
	for _, v := range p {
		if unicode.IsSpace(rune(v)) && space {
			continue
		}

		p[keepIndex] = v
		keepIndex++
		if unicode.IsSpace(rune(v)) {
			space = true
		} else {
			space = false
		}
	}
	return p[:keepIndex]
}

func reverseUtf(bs []byte) []byte {
	swap := utf8.RuneCount(bs) / 2
	for i := 0; i < swap; i++ {
		bs = swapRunes(bs, i)
	}
	return bs
}

// swapRunes: swaps in place rune n places from start with rune n places from end
func swapRunes(bs []byte, n int) []byte {
	swapped := make([]byte, len(bs))
	writeIndex := 0 // location to write within swapped

	var front, back rune // runes to swap
	frontIndex := n
	backIndex := len(bs) - (n + 1)

	nrunes := utf8.RuneCount(bs)

	// get runes, front and back
	iter := bs
	for i := 0; i < nrunes; i++ {
		r, size := utf8.DecodeRune(iter)
		if i == frontIndex {
			front = r
		} else if i == backIndex {
			back = r
		}
		iter = iter[size:] // move past rune
	}

	// build 'swapped'
	iter = bs
	for i := 0; i < nrunes; i++ {
		rsize := 0 // loop rune
		wsize := 0 // written rune
		r, rsize := utf8.DecodeRune(iter)

		if i == frontIndex {
			wsize = utf8.EncodeRune(swapped[writeIndex:], back)
		} else if i == backIndex {
			wsize = utf8.EncodeRune(swapped[writeIndex:], front)
		} else {
			wsize = utf8.EncodeRune(swapped[writeIndex:], r)
		}
		iter = iter[rsize:] // move past rune
		writeIndex += wsize
	}
	return swapped
}

// swapRunes: swaps in place rune n places from start with rune n places from end
// use no extra memory
// func swapRunesNoMem(bs []byte, int n) {

// }
