// Play around with the lenconv package
package main

import (
	"fmt"

	"github.com/tcharding/gopl/ch2/lenconv"
)

func main() {
	d := 5.0
	f := lenconv.MToF(lenconv.Metre(d))
	m := lenconv.FToM(lenconv.Feet(d))
	fmt.Printf("d: %v M: %v F: %v\n", d, m, f)
	fmt.Printf("ones: %v, %v\n", lenconv.Metre(1), lenconv.Feet(1))
}
