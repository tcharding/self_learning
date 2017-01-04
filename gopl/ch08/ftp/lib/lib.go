// ftp client/server library
package lib

import (
	"bufio"
	"bytes"
	"fmt"
	"net"
	"strings"
)

// IsValid: check if argv is valid command with valid arguments
func IsValid(argv []string) bool {
	cmnd := argv[0]
	switch cmnd {
	case "ls":
		return true
	case "get", "put", "cd":
		return len(argv) == 2
	default:
		return false
	}
}

const (
	done = "done"
	yes  = "yes"
	no   = "no"
)

func OkToPut(s string) bool {
	s = strings.TrimSpace(s) // just in case
	return s == yes
}

func isDone(s string) bool {
	s = strings.TrimSpace(s)
	return s == done
}

func isNo(s string) bool {
	s = strings.TrimSpace(s)
	return s == no
}

func WriteOkToPut(conn net.Conn) {
	fmt.Fprintf(conn, "%s\n", yes)
}

func WriteDone(conn net.Conn) {
	fmt.Fprintf(conn, "%s\n", done)
}

func WriteNo(conn net.Conn) {
	fmt.Fprintf(conn, "%s\n", no)
}

// ReadUntilDone: read data from r until "done"
func ReadUntilDone(r *bufio.Reader) (string, error) {
	var buf bytes.Buffer

	text, err := r.ReadString('\n')
	if err != nil {
		return "", err
	}

	for !isDone(text) {
		buf.WriteString(text)
		text, err = r.ReadString('\n')
		if err != nil {
			return buf.String(), err
		}
	}
	return buf.String(), nil
}

// IsClose: close command
func IsClose(s string) bool {
	s = strings.TrimSpace(s)
	return s == "close" || s == "quit" || s == "exit"
}
