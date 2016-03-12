// netcat implementation
package main

import (
	"fmt"
	"io"
	"log"
	"net"
	"os"

	//cc	"github.com/tcharding/utils"
)

func main() {
	if len(os.Args) < 3 {
		fmt.Printf("Usage: %s host port\n", os.Args[0])
		os.Exit(1)
	}
	host := os.Args[1]
	port := os.Args[2]
	url := fmt.Sprintf("%s:%s", host, port)

	conn, err := net.Dial("tcp", url)
	if err != nil {
		log.Fatal(err)
	}
	defer conn.Close()
	go mustCopy(os.Stdout, conn)
	mustCopy(conn, os.Stdin)
}

// mustCopy copy src to dst
// ref: The Go Programming Language p221
func mustCopy(dst io.Writer, src io.Reader) {
	if _, err := io.Copy(dst, src); err != nil {
		log.Fatal(err)
	}
}
