// input, example of reading user input
package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	reader := bufio.NewReader(os.Stdin)
	text, _ := reader.ReadString('\n')
	fmt.Printf("we got: %s\n", text)
}

func printMenu() {
	fmt.Printf(`
1. Item one
2. Second item
3. oh my
`)
}
