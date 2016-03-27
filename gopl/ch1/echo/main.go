// Echo
package main

import (
	"fmt"
	"os"
	"strconv"
)

func main() {

	var s, sep string
	//	var s string
	var loop int

	loop, err := strconv.Atoi(os.Args[1])
	if err != nil {
		os.Exit(1)
	}

	for i := 0; i < loop; i++ {
		// s = strings.Join(os.Args[:], " ")
		s = ""
		sep = ""
		for _, arg := range os.Args[2:] {
			s += sep + arg
			sep = " "
		}
	}
	fmt.Println(s)
}
