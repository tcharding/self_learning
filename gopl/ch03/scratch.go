package main

import "fmt"

func main() {
	res := "oh yes"
	if false != false {
		res = "shit"
	}
	fmt.Println(res)
}
