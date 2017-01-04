// heavy runs busy loop to measure performance effect of GOMAXPROCS
package main

import (
	"runtime"
	"time"
)

func main() {
	// unnecessary as this is set by default
	ncpu := runtime.NumCPU()
	runtime.GOMAXPROCS(2)

	for i := 0; i < ncpu; i++ {
		go busy()
	}
	time.Sleep(1 * time.Hour)
}

// busy loop indefinitely
func busy() {
	count := 0
	for {
		count++
	}
}
