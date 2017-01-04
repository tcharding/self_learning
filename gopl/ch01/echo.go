package main

import (
	"fmt"
	"os"
	"strconv"
)

func main() {
	var s string

	for i, arg := range os.Args[1:] {
		s = strconv.Itoa(i + 1)
		s += ": " + arg
		fmt.Println(s)
	}
}
