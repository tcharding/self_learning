// Get json data from github
package main

import (
	"fmt"
	"log"
	"os"

	"github.com/tcharding/gopl/ch4/github"
)

func main() {
	result, err := github.SearchIssues(os.Args[1:])
	if err != nil {
		log.Fatal(err)
	}
	fmt.Printf("%d issues:\n", result.TotalCount)
	//	printTable(result)
	printDateCreated(result)
}
