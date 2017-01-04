// return: uses panic and recover to return
// a non-zero value without using return statement
package main

import "fmt"

func main() {
	defer func() {
		p := recover()
		if p != nil {
			fmt.Printf("we got: %d\n", p)
		}
	}()
	fn()
}

func fn() {
	panic(1)
}
