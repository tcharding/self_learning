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
	defer conn.Close()
	mustCopy(os.Stdout, conn)
}

func mustCopy(dst io.Writer, src io.Reader) {
	if _, err := io.Copy(dst, src); err != nil {
		log.Fatal(err)
	}
}
