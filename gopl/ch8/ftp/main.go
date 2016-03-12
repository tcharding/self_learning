// ftp server
package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"net"
	"os"
)

func main() {
	if len(os.Args) != 2 {
		fmt.Printf("Usage: %s <port>\n", os.Args[0])
		os.Exit(1)
	}
	port := os.Args[1]
	url := fmt.Sprintf("localhost:%s", port)

	listener, err := net.Listen("tcp", url)
	if err != nil {
		log.Fatal(err)
	}
	for {
		conn, err := listener.Accept()
		if err != nil {
			log.Print(err) // e.g connection aborted
			continue
		}
		go handleConn(conn) // handle one connection at a time
	}
}

// handleConn ftp connection
func handleConn(c net.Conn) {
	defer c.Close()
	_, err := io.WriteString(c, fmt.Sprintf("ftp> "))
	if err != nil {
		return // e.g client disconnected
	}

	input := bufio.NewScanner(c)
	for input.Scan() {
		_, err := io.WriteString(c, fmt.Sprintf("You sent: %s\n", input.Text()))
		if err != nil {
			return // e.g client disconnected
		}
	}
}
