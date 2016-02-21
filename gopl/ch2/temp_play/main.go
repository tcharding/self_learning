package main

import (
	"fmt"

	"github.com/tcharding/gopl/ch2/tempconv"
)

func main() {
	var c tempconv.Celsius = 30
	fmt.Printf("It is %v today, and for our American cousins, that is %v\n",
		c, tempconv.CToF(c))

	fmt.Println("And now for some Kelvin fun.")
	k := tempconv.CToK(c)
	f := tempconv.CToF(c)
	fmt.Printf("C: %v F: %v K: %v (K: %v)\n", c, f, k, tempconv.FToK(f))
}
