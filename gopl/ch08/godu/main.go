// du calculates size of directories from command line concurrently
// periodically displays running totals
package main

import (
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"sync"
	"time"
)

var sema = make(chan struct{}, 20) // total number of goroutines active

func main() {
	flag.Parse()
	roots := flag.Args()
	for _, root := range roots {
		du(root)
	}
}

func du(dir string) {
	fileSizes := make(chan int64)
	tick := time.Tick(500 * time.Millisecond)
	var n sync.WaitGroup
	n.Add(1)
	go walkDir(dir, &n, fileSizes)
	go func() {
		n.Wait()
		close(fileSizes)
	}()
	var nfiles, nbytes int64
loop:
	for {
		select {
		case size, ok := <-fileSizes:
			if !ok {
				break loop // fileSizes was closed
			}
			nfiles++
			nbytes += size
		case <-tick:
			printDiskUsage("accumulative: ", nfiles, nbytes)
		}
	}
	msg := fmt.Sprintf("Total for %s: ", dir)
	printDiskUsage(msg, nfiles, nbytes) // final totals
}

// walkDir recursively walks the file tree rooted at dir
// and sends the size of each found file on fileSizes
func walkDir(dir string, wg *sync.WaitGroup, fileSizes chan<- int64) {
	defer wg.Done()
	for _, entry := range dirents(dir) {
		if entry.IsDir() {
			subdir := filepath.Join(dir, entry.Name())
			wg.Add(1)
			walkDir(subdir, wg, fileSizes)
		} else {
			fileSizes <- entry.Size()
		}
	}
}

func printDiskUsage(msg string, nfiles, nbytes int64) {
	fmt.Printf("%s %d files %.1f GB\n", msg, nfiles, float64(nbytes)/1e9)
}

// dirents returns the entries of directory dir
func dirents(dir string) []os.FileInfo {
	sema <- struct{}{}        // acquire token
	defer func() { <-sema }() // release token

	entries, err := ioutil.ReadDir(dir)
	if err != nil {
		return nil
	}
	return entries
}
