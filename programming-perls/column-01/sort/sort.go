// Sort using bit vector. Column 1, Exercise 3.
package main

import (
	"bufio"
	"flag"
	"fmt"
	"log"
	"os"
	"sort"
	"strconv"

	"github.com/tcharding/programming-perls/column-01/bitvec"
)

const SetSize = 10000000

var sortFlag = flag.String("sortFlag", "bitvec", "Sort algorithm to use")

func main() {
	flag.Parse()
	if len(flag.Args()) < 1 {
		fmt.Fprintf(os.Stderr, "Usage: %s <file name>\n", os.Args[0])
		os.Exit(1)
	}
	path := flag.Args()[0]
	switch *sortFlag {
	case "lib":
		readAndSortLib(path)
	default:
		readAndSortBV(path)
	}
}

func readAndSortLib(path string) error {
	xs, err := readFileToSlice(path)
	if err != nil {
		return err
	}
	sort.Sort(sort.IntSlice(xs))
	for _, x := range xs {
		fmt.Printf("%d\n", x)
	}
	return nil
}

func readAndSortBV(path string) error {
	v, err := readFileToBitVector(path)
	if err != nil {
		return err
	}
	writeBitVector(v)
	return nil
}

func readFileToSlice(path string) ([]int, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()

	var xs []int
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		x, err := strconv.Atoi(scanner.Text())
		if err != nil {
			return nil, err
		}
		xs = append(xs, x)
	}
	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
	return xs, nil
}

func readFileToBitVector(path string) (*bitvec.BitVec, error) {
	var (
		v = bitvec.New(SetSize)
	)
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		x, err := strconv.Atoi(scanner.Text())
		if err != nil {
			return nil, err
		}
		v.Set(x)
	}
	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}
	return v, nil
}

func writeBitVector(v *bitvec.BitVec) {
	for i := 0; i < SetSize; i++ {
		if v.IsSet(i) {
			fmt.Printf("%d\n", i)
		}
	}
}
