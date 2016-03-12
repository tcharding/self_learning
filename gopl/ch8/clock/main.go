// clock package implements network time server
package main

import (
	"fmt"
	"io"
	"log"
	"net"
	"os"
	"time"
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

// handleConn serve time continuously to single connection
func handleConn(c net.Conn) {
	defer c.Close()
	for {
		_, err := io.WriteString(c, time.Now().Format("15:04:05\n"))
		if err != nil {
			return // e.g client disconnected
		}
		time.Sleep(1 * time.Second)
	}
}
