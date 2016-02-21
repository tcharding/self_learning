package popcount

// pc[i] is the population count of i.
var pc [256]byte

func init() {
	for i := range pc {
		pc[i] = pc[i/2] + byte(i&1)
	}
}

// PopCount returns the population count (number of set bits) of x.
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

func PopCountLoop(x uint64) int {
	sum := 0
	var i uint
	for ; i < 8; i++ {
		sum += int(pc[byte(x>>(i*8))])
	}
	return sum
}

func PopCount64(x uint64) int {
	sum := 0
	for i := 0; i < 64; i++ {
		if iToB(x & 1) {
			sum++
		}
		x = x >> 1
	}
	return sum
}

func iToB(i uint64) bool {
	if i == 1 {
		return true
	}
	return false
}

func PopCountClear(x uint64) int {
	sum := 0
	for x > 0 {
		sum++
		x = x & (x - 1)
	}
	return sum
}
