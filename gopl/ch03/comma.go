// comma
package main

import (
	"bufio"
	"fmt"
	"os"

	"github.com/tcharding/gopl/ch3/comma"
)

func main() {
	reader := bufio.NewReader(os.Stdin)
	for {
		line, _ := reader.ReadString('\n')
		line = line[:len(line)-1] // strip trailing newline

		fmt.Println(comma.Comma(line))
	}
}
