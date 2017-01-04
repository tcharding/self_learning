// two goroutines message in ping pong fashion indefinitely over two channels
package main

import (
	"fmt"
	"sync"
	"time"
)

var left, right, reset chan struct{}
var counts chan int

func init() {
	left = make(chan struct{})
	right = make(chan struct{})
	reset = make(chan struct{})
	counts = make(chan int)
}

func main() {
	tick := time.Tick(1 * time.Second)
	var n sync.WaitGroup
	n.Add(1)
	go ping()
	go pong()

	for {
		select {
		case <-tick:
			reset <- struct{}{}
			count := <-counts
			fmt.Printf("%d round trip channel communications in previous second\n", count)
		}
	}
	n.Wait() // wait indefinitely
}

func ping() {
	for {
		left <- struct{}{}
		<-right
	}
}

func pong() {
	// for {
	// 	<-left
	// 	log.Print("pong read left")
	// 	right <- struct{}{}
	// }
	count := 0
	for {
		select {
		case <-reset:
			counts <- count
			count = 0
		case <-left:
			right <- struct{}{}
			count++
		}
	}

}
