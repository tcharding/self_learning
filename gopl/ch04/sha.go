// sha: print sha sum of stdin, default to sha256
package main

import (
	"bufio"
	"crypto/sha256"
	"crypto/sha512"
	"fmt"
	"os"
)

func main() {
	algo := "256"
	if len(os.Args) != 1 {
		if os.Args[1] == "-s" {
			algo = os.Args[2]
		} else {
			fmt.Printf("Unsupported option: %s\n", os.Args[1])
			os.Exit(1)
		}
	}
	reader := bufio.NewReader(os.Stdin)
	line, err := reader.ReadString('\n')
	if err != nil {
		fmt.Fprintf(os.Stderr, "read error")
		os.Exit(1)
	}
	sum, err := hash(line, algo)
	if err != nil {
		fmt.Fprintf(os.Stderr, "hash error: %v", err)
	}
	fmt.Printf("%s\n", sum)
}

func hash(s, algo string) (sum, err string) {
	if sum == "256" {
		hash := sha256.Sum256([]byte(line))
		return hash.String(), nil
	} else if sum == "384" {
		hash := sha512.Sum384([]byte(line))
		return hash.String(), nil
	} else if sum == "512" {
		hash := sha512.Sum512([]byte(line))
		return hash.String(), nil
	} else {
		msg := fmt.Sprintf("Unsupported hashing algorithm:", algo)
		return "", msg
	}

}
