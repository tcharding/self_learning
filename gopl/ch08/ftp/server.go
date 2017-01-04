// simple FTP server
package main

import (
	"bufio"
	"bytes"
	"flag"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"net"
	"os"
	"strings"

	"github.com/tcharding/gopl/ch8/ftp/lib"
)

var port = flag.String("port", "8021", "port number")

const filePerms = 0644

func main() {
	flag.Parse()
	host := "localhost:" + *port
	listener, err := net.Listen("tcp", host)
	if err != nil {
		log.Fatal(err)
	}
	for {
		conn, err := listener.Accept()
		if err != nil {
			log.Print(err)
			continue
		}
		go handleConn(conn)
	}
}

// handleConn: handle single connection
func handleConn(conn net.Conn) error {
	reader := bufio.NewReader(conn)
	fmt.Fprintf(conn, "Connection established ...\n")

	for {
		query, err := reader.ReadString('\n')
		if err != nil {
			return err
		}
		if err != nil && err != io.EOF {
			fmt.Fprintf(conn, "Server error")
			return err
		}
		if err == io.EOF {
			return nil
		}
		fmt.Fprintf(os.Stderr, "got query: %s", query)

		query = strings.TrimSpace(query)
		if query == "" {
			continue
		}
		argv := strings.Split(query, " ")
		cmnd := argv[0]

		if cmnd == "close" {
			fmt.Fprintf(conn, "Closing connection...\n")
			return nil
		}

		ok := lib.IsValid(argv)
		if !ok {
			fmt.Fprintf(conn, "invalid command: %s\n did you supply the correct arguments\n", cmnd)
			continue
		}

		err = handleCommand(conn, argv)
		if err != nil {
			fmt.Fprintf(conn, "Command error: %s\n", err.Error())
		}

		printCwd()

		fmt.Fprintf(conn, "done\n")
	}
	return fmt.Errorf("handleConn: shouldn't get here")
}

// handleCommand:
func handleCommand(conn net.Conn, argv []string) error {
	cmnd := argv[0]
	switch cmnd {
	case "ls":
		return ls(conn)
	case "get":
		return get(conn, argv[1])
	case "put":
		return put(conn, argv[1])
	case "cd":
		return cd(conn, argv[1])
	default:
		return fmt.Errorf("unknown command")
	}
}

func ls(conn net.Conn) error {
	var buf bytes.Buffer
	files, _ := ioutil.ReadDir("./")
	for _, f := range files {
		buf.WriteString(fmt.Sprintln(f.Name()))
	}
	b := buf.Bytes()
	fmt.Fprintf(os.Stderr, "writing %d bytes to connection\n", len(b))
	conn.Write(b)
	return nil
}

// get: send file to client
func get(conn net.Conn, fileName string) error {
	f, err := os.Open(fileName)
	if err != nil {
		return err
	}
	if _, err := io.Copy(conn, f); err != nil {
		return err
	}

	return nil
}

// put: receive file from client
func put(conn net.Conn, fileName string) error {
	lib.WriteOkToPut(conn)

	r := bufio.NewReader(conn)

	f, err := os.Create(fileName)
	if err != nil {
		fmt.Fprintf(conn, "error creating file on server: %s\n", err.Error())
		return err
	}
	lib.WriteOkToPut(conn)

	result, err := lib.ReadUntilDone(r)
	if err != nil {
		fmt.Fprintf(os.Stderr, "server error\n")
	}
	_, err = f.WriteString(result)
	if err != nil {
		f.Close()
		return err
	}
	f.Close()
	err = os.Chmod(fileName, filePerms)
	if err != nil {
		return err
	}
	fmt.Fprintf(os.Stderr, "received file: %s\n", fileName)
	return nil
}

// cd: change directory
func cd(conn net.Conn, path string) error {
	err := os.Chdir(path)
	if err != nil {
		return err
	}
	printCwd()
	return nil
}

func printCwd() {
	cwd, err := os.Getwd()
	if err != nil {
		log.Fatal(err)
	}
	fmt.Fprintf(os.Stderr, "cwd: %s\n", cwd)
}
