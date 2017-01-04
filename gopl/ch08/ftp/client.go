package main

import (
	"bufio"
	"flag"
	"fmt"
	"io"
	"log"
	"net"
	"os"
	"strings"

	"github.com/tcharding/gopl/ch8/ftp/lib"
)

const (
	filePerms = 0644
)

var port = flag.String("port", "8021", "port number")

func main() {
	flag.Parse()
	host := "localhost:" + *port
	conn, err := net.Dial("tcp", host)
	if err != nil {
		log.Fatal(err)
	}
	defer conn.Close()
	handleConn(conn)
}

// handleConn: handle the ftp client
func handleConn(conn net.Conn) {
	readerUser := bufio.NewReader(os.Stdin)
	readerConn := bufio.NewReader(conn)

	//	io.Copy(os.Stdout, conn)
	text, err := readerConn.ReadString('\n')
	if err != nil {
		log.Fatal(err)
	}
	fmt.Printf("connected to %s\n", conn.RemoteAddr().String())
	fmt.Printf("%s", text)

	for {
		fmt.Printf("FTP> ")
		query, err := readerUser.ReadString('\n')
		if err != nil {
			log.Fatal(err)
		}
		err = handleQuery(conn, query)
		if err != nil {
			fmt.Print(err)
		}
	}
	fmt.Printf("Closing connection...\n")
}

// handleQuery: send query to server and process response
func handleQuery(conn net.Conn, query string) error {
	r := bufio.NewReader(conn)
	query = strings.TrimSpace(query)
	argv := strings.Split(query, " ")

	if lib.IsClose(query) {
		fmt.Fprintf(os.Stderr, "Closing connection...\n")
		conn.Close()
		os.Exit(0)
	}

	if !lib.IsValid(argv) {
		return fmt.Errorf("invalid query: %s\n", query)
	}

	//fmt.Fprintf(os.Stderr, "sending query: %s\n", query)
	fmt.Fprintf(conn, "%s\n", query)

	cmnd := argv[0]

	switch cmnd {
	case "ls":
		result, err := lib.ReadUntilDone(r)
		if err != nil {
			fmt.Fprintf(os.Stderr, "server error\n")
		}
		fmt.Printf("%s", string(result))
	case "get":
		if err := get(conn, argv[1]); err != nil {
			return err
		}
	case "put":
		if err := put(conn, argv[1]); err != nil {
			return err
		}
	}
	return nil
}

// get command, precondition: argv is valid
func get(conn net.Conn, fileName string) error {
	r := bufio.NewReader(conn)
	f, err := os.Create(fileName)
	if err != nil {
		return err
	}
	result, err := lib.ReadUntilDone(r)
	if err != nil {
		fmt.Fprintf(os.Stderr, "server error\n")
	}
	_, err = f.WriteString(result)
	if err != nil {
		return err
	}
	f.Close()
	err = os.Chmod(fileName, filePerms)
	if err != nil {
		return err
	}
	fmt.Printf("Successfully transferred file: %s\n", fileName)
	return nil
}

// put command, precondition: argv is valid
func put(conn net.Conn, fileName string) error {
	r := bufio.NewReader(conn)
	response, err := r.ReadString('\n')
	if !lib.OkToPut(response) {
		return fmt.Errorf("server refused to put file (%s): %s\n", fileName, response)
	}

	f, err := os.Open(fileName)
	if err != nil {
		lib.WriteDone(conn)
		return err
	}
	defer f.Close()

	_, err = io.Copy(conn, f)
	if err != nil {
		lib.WriteDone(conn)
		return err
	}
	lib.WriteDone(conn)

	// if nread != nwriten {
	// 	return fmt.Errorf("read %d bytes for file but only wrote %d to connection\n",
	// 		nread, nwriten)
	// }
	fmt.Printf("Successfully transferred file: %s\n", fileName)
	return nil
}
