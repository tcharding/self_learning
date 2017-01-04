// PopCount returns the population count (number of set bits) of x.
// also demonstrates the use of init()
package popcount

import "sync"

// pc[i] is the population count of i.

var computeOnce sync.Once
var pc [256]byte

func computeValues() {
	for i := range pc {
		pc[i] = pc[i/2] + byte(i&1)
	}
}

// text book implementation
func PopCount(x uint64) int {
	computeOnce.Do(computeValues)
	return int(pc[byte(x>>(0*8))] +
		pc[byte(x>>(1*8))] +
		pc[byte(x>>(2*8))] +
		pc[byte(x>>(3*8))] +
		pc[byte(x>>(4*8))] +
		pc[byte(x>>(5*8))] +
		pc[byte(x>>(6*8))] +
		pc[byte(x>>(7*8))])
}
