// PopCount returns the population count (number of set bits) of x.
// also demonstrates the use of init()
package popcount

// pc[i] is the population count of i.
var pc [256]byte

func init() {
	for i := range pc {
		pc[i] = pc[i/2] + byte(i&1)
	}
}

// cannot get the types to match up?

// text book implementation
func PopCount(x uint64) int {
	return int(pc[byte(x>>(0*8))] +
		pc[byte(x>>(1*8))] +
		pc[byte(x>>(2*8))] +
		pc[byte(x>>(3*8))] +
		pc[byte(x>>(4*8))] +
		pc[byte(x>>(5*8))] +
		pc[byte(x>>(6*8))] +
		pc[byte(x>>(7*8))])
}

// Exercise 2.3
func PopCount(x uint64) int {
	var n int
	for i := 0; i < 8; i++ {
		n += pc[byte(x>>uint(i*8))]
	}
	return n
}

// Exercise 2.4
func PopCount(x uint64) int {
	n := 0
	for i := 0; i < 64; i++ {
		if x>>i&1 == 1 {
			n++
		}
	}
	return n
}

// Exercise 2.5
func PopCount(x uint64) int {
	n := 0
	for i := 0; i < 64; i++ {
		n += x - (x & (x - 1))
	}
	return n
}
