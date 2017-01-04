// Conv converts its numeric argument to various measurement units
package main

import (
	"fmt"
	"io"
	"os"
	"strconv"

	"github.com/tcharding/gopl/ch2/units"
)

func main() {
	if len(os.Args) == 1 {
		for {
			var n float64
			_, err := fmt.Scanf("%f", &n)

			if err != nil {
				if err == io.EOF {
					os.Exit(0)
				}
				fmt.Fprintf(os.Stderr, "error: %v\n", err)
				os.Exit(1)
			}
			convert(n)
		}
	}
	for _, arg := range os.Args[1:] {
		n, err := strconv.ParseFloat(arg, 64)
		if err != nil {
			fmt.Fprintf(os.Stderr, "cf: %v\n", err)
			os.Exit(1)

		}
		convert(n)
	}
}

func convert(n float64) {
	f := units.Fahrenheit(n)
	c := units.Celsius(n)

	kg := units.Kilogram(n)
	p := units.Pound(n)

	m := units.Mile(n)
	km := units.Kilometre(n)

	fmt.Println("Temperature")
	fmt.Printf("%s = %s, %s = %s\n",
		f, units.FToC(f), c, units.CToF(c))

	fmt.Println()
	fmt.Println("Weight")
	fmt.Printf("%s = %s, %s = %s\n",
		kg, units.KToP(kg), p, units.PToK(p))

	fmt.Println()
	fmt.Println("Distance")
	fmt.Printf("%s = %s, %s = %s\n",
		km, units.KToM(km), m, units.MToK(m))

}
