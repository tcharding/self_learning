// hash stdin
package main

import (
	"bytes"
	"crypto/sha512"
	"fmt"
	"io"
	"os"
	"strings"
)

func main() {
	var digest string
	if len(os.Args) > 1 {
		digest = parseArgs(os.Args)
	}
	bytes := readStdin()
	doHash(&bytes, digest)
}

func parseArgs(args []string) string {
	option := args[1]
	var digest string
	if option == "-h" || option == "--help" {
		usage()
	} else if strings.Contains(option, "digest") {
		i := strings.Index(option, "=")
		if i != -1 {
			digest = option[i+1:]
		}
	} else {
		fmt.Printf("Unknown option: %s\n", option)
		usage()
	}
	return digest
}

// Print usage and exit
func usage() {
	usage := `Usage: hash_input [options]

Options:
  -h | --help              Show this menu.
  --digest=<algorithm>     Select algorithm to use.`
	fmt.Println()
	fmt.Println(usage)
	os.Exit(0)

}

func readStdin() []byte {
	buf := bytes.NewBuffer(nil)
	_, err := io.Copy(buf, os.Stdin)
	if err != nil {
		fmt.Printf("Input error ... aborting\n")
		os.Exit(1)
	}
	return buf.Bytes()
}

func doHash(p *[]byte, digest string) {
	if digest == "sha512" {
		var hash [sha512.Size]byte
		hash = sha512.Sum512(*p)
		fmt.Printf("%x\n", hash)

	} else if digest == "sha384" {
		var hash [sha512.Size384]byte
		hash = sha512.Sum384(*p)
		fmt.Printf("%x\n", hash)
	} else if digest != "" {
		fmt.Println("Unknown algorithm, we only support sha256, sha384, and sha512")
	} else {
		var hash [sha512.Size256]byte
		hash = sha512.Sum512_256(*p)
		fmt.Printf("%x\n", hash)
	}

}
