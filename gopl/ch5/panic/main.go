// panic/recover example. Exercise 5.19
package main

import "fmt"

func main() {
	fmt.Printf("Got: %v\n", f())
}

// f artificially uses panic to return int
func f() (err error) {
	defer func() {
		recover()
		err = fmt.Errorf("1")
	}()
	panic("aoue")
}
