// generate-input generates file of random integers.
package main

import (
	"fmt"
	"log"
	"math/rand"
	"os"
	"strconv"
	"time"
)

const (
	MaxValue = 10000000
	MinValue = 999999
)

func main() {
	if len(os.Args) < 3 {
		fmt.Fprintf(os.Stderr, "Usage: %s <file> <num integers>\n", os.Args[0])
		os.Exit(1)
	}
	fileName := os.Args[1]
	nVals, err := strconv.Atoi(os.Args[2])
	if err != nil {
		log.Fatal(err)
	}

	f, err := os.Create(fileName)
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	rand.Seed(time.Now().UTC().UnixNano())
	for i := 0; i < nVals; i++ {
		x := MinValue + rand.Intn(MaxValue-MinValue)
		fmt.Fprintf(f, "%d\n", x)
	}
}
