// client: send input from stdin to server, print response on stdout
package main

import (
	"flag"
	"io"
	"log"
	"net"
	"os"
)

var port = flag.String("port", "8000", "port number")

func main() {
	flag.Parse()
	host := "localhost:" + *port
	conn, err := net.Dial("tcp", host)
	if err != nil {
		log.Fatal(err)
	}
	done := make(chan int)
	go func() {
		io.Copy(os.Stdout, conn) // NOTE: ignoring errors
		log.Printf("done")
		done <- 1 // signal the main goroutine
	}()
	mustCopy(conn, os.Stdin)
	conn.(*net.TCPConn).CloseWrite()
	<-done // wait for background goroutine
}

func mustCopy(dst io.Writer, src io.Reader) {
	if _, err := io.Copy(dst, src); err != nil {
		log.Fatal(err)
	}
}
