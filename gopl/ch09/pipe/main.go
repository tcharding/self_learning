// link multiple goroutines together using channels (i.e pipeline)
package main

import "log"

const npipes = 10

func main() {
	input := make(chan int)
	in := input
	//	for i := 0; i < npipes; i++ {
	count := 0
	for {
		out := make(chan int)
		log.Printf("goroutine # %d\n", count)
		go func(in <-chan int, out chan<- int) {
			defer close(out)
			x := <-in
			out <- x + 1

		}(in, out)
		count++
		in = out
	}
	output := in
loop:
	for {
		select {
		case input <- 0:
			// seed pipeline
		case res := <-output:
			log.Printf("res: %d\n", res)
			break loop
		}
	}
	close(input)
}

type Func func(int) int
