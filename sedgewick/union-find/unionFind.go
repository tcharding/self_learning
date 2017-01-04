// Program 1.1
package unionFind

import (
	"fmt"
	"io"
	"log"
	"os"
)

const N = 10000

var (
	in    io.Reader = os.Stdin
	out   io.Writer = os.Stdout
	debug           = true
)

func msg(format string, args ...interface{}) {
	if debug {
		fmt.Fprintf(os.Stderr, format, args...)
	}
}

// Program 1.1
//
// This program reads a sequence of pairs of non-negative integers less than
// N from standard input (interpreting the pair p q to mean 'connect object
// p to object q') and prints out the pairs representing objects that ore not
// yet connected. It maintains an array id that has an entry for each object,
// with the property that id[p] ind id[q] are equal if and only if p and q are
// connected.
func quickFind() {
	var (
		p, q int
		id   = make([]int, N)
	)
	for i := 0; i < N; i++ {
		id[i] = i
	}
	for {
		_, err := fmt.Fscanf(in, "%d %d\n", &p, &q)
		if err != nil && err != io.EOF {
			log.Fatal(err)
		} else if err != nil && err == io.EOF {
			break
		}
		if id[p] == id[q] {
			continue
		}
		t := id[p]
		for i := 0; i < N; i++ {
			if id[i] == t {
				id[i] = id[q]
			}
		}
		fmt.Fprintf(out, "%d %d\n", p, q)
	}
}

// Program 1.2
//
// If we replace the body of the while loop in Program 1.1 by this code, we have
// a program that meets the same specifications as Program 1.1, but does less
// computation for the find operation. The for loops and subsequent if statement
// in this code specify the necessary and sufficient conditions on the id array
// for p and q to be connected. The assignment statement id[i] = j implements the
// union operation.
func quickUnion() {
	var (
		p, q int
		id   = make([]int, N)
	)
	for i := 0; i < N; i++ {
		id[i] = i
	}
	for {
		_, err := fmt.Fscanf(in, "%d %d\n", &p, &q)
		if err != nil {
			if err != io.EOF {
				log.Fatal(err)
			} else {
				break
			}
		}

		var i, j int
		for i = p; i != id[i]; i = id[i] {
			// do nothing
		}
		for j = q; j != id[j]; j = id[j] {
			// do nothing
		}
		if i == j {
			continue
		}
		id[i] = j
		fmt.Fprintf(out, "%d %d\n", p, q)
	}
}

// Program 1.3
//
// This program is a modification to the quick-union algorithm above that keeps an
// additional array sz for the purpose of maintaining, for each object with
// id[i] = i, the number of nodes in the associated tree, so that the union operation
// can link the smaller of the two specified trees to the larger, thus preventing the
// growth of long paths in the trees.
func quickUnionWeighted() {
	var (
		p, q int
		id   = make([]int, N)
		sz   = make([]int, N)
	)
	for i := 0; i < N; i++ {
		id[i] = i
		sz[i] = 1
	}
	for {
		_, err := fmt.Fscanf(in, "%d %d\n", &p, &q)
		if err != nil {
			if err != io.EOF {
				log.Fatal(err)
			} else {
				break
			}
		}

		var i, j int
		for i = p; i != id[i]; i = id[i] {
			// do nothing
		}
		for j = q; j != id[j]; j = id[j] {
			// do nothing
		}
		if i == j {
			continue
		}
		if sz[i] < sz[j] {
			id[i] = j
			sz[j] += sz[i]
		} else {
			id[j] = i
			sz[i] += sz[j]
		}
		fmt.Fprintf(out, "%d %d\n", p, q)
	}
}

// Program 1.4
//
// Here we halve the length of any path that we traverse. The net result of this change
// is that the trees become almost completely flat after a long sequence of operations.
func quickUnionPathCompressionByHalving() {
	var (
		p, q int
		id   = make([]int, N)
		sz   = make([]int, N)
	)
	for i := 0; i < N; i++ {
		id[i] = i
		sz[i] = 1
	}
	for {
		_, err := fmt.Fscanf(in, "%d %d\n", &p, &q)
		if err != nil {
			if err != io.EOF {
				log.Fatal(err)
			} else {
				break
			}
		}

		var i, j int
		for i = p; i != id[i]; i = id[i] {
			id[i] = id[id[i]]
		}
		for j = q; j != id[j]; j = id[j] {
			id[j] = id[id[j]]
		}
		if i == j {
			continue
		}
		if sz[i] < sz[j] {
			id[i] = j
			sz[j] += sz[i]
		} else {
			id[j] = i
			sz[i] += sz[j]
		}
		fmt.Fprintf(out, "%d %d\n", p, q)
	}

}
