package main

import "fmt"

func main() {
	var x []int
	for i := 0; i < 10; i++ {
		x = appendInt(x, i, 0, 0, 0)
		fmt.Printf("%d cap=%3d\t%v\n", i, cap(x), x)
	}
}

func appendInt(x []int, y ...int) []int {
	var z []int
	zlen := len(x) + len(y)
	if zlen <= cap(x) {
		// There is room to grow. Extend the slice.
		z = x[:zlen]
	} else {
		// There is insufficient space. Allocate a new array.
		// Grow by doubling, for amortized linear complexity.
		zcap := zlen
		if zcap < 2*len(x) {
			zcap = 2 * len(x)
		}
		z = make([]int, zlen, zcap)
		copy(z, x) // a built-in function
	}
	copy(z[len(x):], y)
	return z
}
