// shift shifts characters of a string to the left.
package main

import (
	"fmt"
	"os"
)

func main() {
	var tests = []struct {
		fn     func(s string, n int) string
		fnName string
	}{
		{basic, "basic"},
		{juggle, "juggle"},
		{divShift, "divShift"},
	}
	for _, test := range tests {
		testFunction(test.fn, test.fnName)
	}
}

func msg(format string, args ...interface{}) {
	fmt.Fprintf(os.Stderr, format, args...)
}

func testFunction(fn func(s string, n int) string, fnName string) {
	var tests = []struct {
		input string
		shift int
		want  string
	}{
		{"abcdefg", 0, "abcdefg"},
		{"abcdefg", 3, "defgabc"},
		{"abcdefg", 2, "cdefgab"},
		{"abcdefghij", 3, "defghijabc"},
		{"abcdefg", 7, "abcdefg"},
	}
	for _, test := range tests {
		if got := fn(test.input, test.shift); got != test.want {
			msg("FAIL: %s(%s) = %s (want: %s)\n", fnName, test.input, got, test.want)
		}
	}
}

// basic: shift s left n characters.
func basic(s string, n int) string {
	t := s[n:]
	t += s[:n]
	return t
}

// juggle: shift s left n characters.
func juggle(s string, n int) string {
	n = n % len(s)
	if n == 0 {
		return s
	}
	buf := make([]byte, len(s))
	copy(buf, s)
	for i := 0; i < n; i++ {
		var (
			tmp byte
			j   int
		)
		tmp = buf[i]
		for j = i; j < len(buf)-n; j += n {
			buf[j] = buf[j+n]
		}
		if i != 0 {
			buf[j] = buf[len(buf)-1]
		}
		buf[len(buf)-1] = tmp
	}
	return string(buf)
}

// divShift: shift s left n characters using divide and conquer algorithm.
func divShift(s string, n int) string {
	n = n % len(s)
	if n == 0 {
		return s
	}
	buf := make([]byte, len(s))
	copy(buf, s)
	swap(buf, 0, n, len(buf))
	return string(buf)
}

// swap: swaps ranges recursively,
func swap(buf []byte, low, mid, high int) {
	if low < 0 || high > len(buf) {
		fmt.Fprintf(os.Stderr, "index out of bounds")
		os.Exit(1)
	}
	lenLeft := mid - low
	lenRight := high - mid

	if lenLeft == lenRight {
		swapEqual(buf, low, mid, mid, high)
	} else if lenLeft < lenRight {
		swapLeftShortest(buf, low, mid, high)
	} else {
		swapRightShortest(buf, low, mid, high)
	}
}

func swapEqual(buf []byte, lowLeft, highLeft, lowRight, highRight int) {
	if highRight-lowRight != highLeft-lowLeft {
		panic("programmer error")
	}

	for i := 0; i < highLeft-lowLeft; i++ {
		li := lowLeft + i
		ri := lowRight + i
		buf[li], buf[ri] = buf[ri], buf[li]
	}
}

func swapLeftShortest(buf []byte, low, mid, high int) {
	length := mid - low
	div := high - length
	swapEqual(buf, low, mid, div, high)
	swap(buf, low, mid, div)
}

func swapRightShortest(buf []byte, low, mid, high int) {
	length := high - mid
	div := low + length
	swapEqual(buf, low, div, mid, high)
	swap(buf, div, mid, high)
}
