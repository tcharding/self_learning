package main

import (
	"bytes"
	"fmt"
	"strings"
)

func main() {
	numbers := []float64{751751, 7513, 630, 6375607, 60460.4600460, 63.632632, 3.3}
	for i, n := range numbers {
		s := fmt.Sprintf("%g", n)
		fmt.Printf("%d: %g (%s)\n", i, n, comma(s))
	}
	fmt.Println(comma("+630.6326360"))
}

func comma(s string) string {
	var sign string
	if s[0] == '-' || s[0] == '+' {
		sign = s[0:1]
		s = s[1:]
	}
	i := strings.Index(s, ".")
	if i == -1 {
		return sign + commarize_int(s)
	}
	return sign + commarize_int(s[:i]) + "." + commarize_decimal(s[i+1:])
}

func commarize_int(s string) string {
	var buf bytes.Buffer
	cnt := 0
	for i := len(s) - 1; i >= 0; i-- {
		cnt++
		buf.WriteByte(s[i])
		if cnt == 3 && i != 0 {
			buf.WriteByte(',')
			cnt = 0
		}
	}
	backwards := buf.String()
	return reverseString(backwards)
}

func commarize_decimal(s string) string {
	var buf bytes.Buffer
	for i, n := range s {
		if i%3 == 0 && i > 0 {
			buf.WriteByte(',')
		}
		buf.WriteRune(n)
	}
	return buf.String()
}

// comma inserts commas in a non-negative decimal integer string.
func commaRecursive(s string) string {
	n := len(s)
	if n <= 3 {
		return s
	}
	return comma(s[:n-3]) + "," + s[n-3:]
}

func reverseString(s string) string {
	var buf bytes.Buffer
	for i := len(s) - 1; i >= 0; i-- {
		buf.WriteByte(s[i])
	}
	return buf.String()
}
