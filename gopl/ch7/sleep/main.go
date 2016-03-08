// Sleep shows use of flags to parse command line options
package main

import (
	"flag"
	"fmt"
	"time"
)

var period = flag.Duration("-period", 1*time.Second, "sleep period")
var skip = flag.Bool("skip", false, "skip sleep")

func main() {
	flag.Parse()
	fmt.Printf("Sleeping for ... ")
	if *skip {
		fmt.Println("Skipping sleep today")
	} else {
		fmt.Printf(" %v", period)
		time.Sleep(*period)
	}
	fmt.Println()
}
