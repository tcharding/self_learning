// Issues query/close/create/update to GitHub issues
package main

import (
	"encoding/json"
	"fmt"
	"log"
	"os"

	"github.com/tcharding/gopl/ch4/github"
)

const (
	user = "tcharding"
)

func main() {
	if len(os.Args) == 1 || os.Args[1] == "-h" || os.Args[1] == "--help" {
		usage()
		os.Exit(1)
	}
	command := os.Args[1]

	// Is there a fancier way to do this?
	switch command {
	case "get":
		terms := os.Args[2:]
		get(terms)
	case "new":
		repo := os.Args[2]
		new(repo)
	case "update":
		update()
	case "close":
		close()
	default:
		fmt.Printf("Unknown command: %s\n", command)
		usage()
		os.Exit(1)
	}
}

func usage() {
	usage := `Usage: issues [options] <command>

Options:
  -h | --help - show this menu

Commands:
  get <terms> - get issues
  new <repo> - create new issue
  update <num> - update issue
  close <num> - close issue

Terms:
  Github terms e.g repo:<username>/<repo> is:open is:issue`
	fmt.Println(usage)
}

func get(terms []string) {
	result, err := github.SearchIssues(terms)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Printf("%d issues:\n", result.TotalCount)
	github.PrintTable(result)
}

// 	u := fmt.Sprintf("repos/%v/%v/labels", owner, repo)

func new(repo string) {
	// TODO: Do this programmatically
	url := github.APIURL + "repos/tcharding/" + repo
	newIssue := github.NewIssue{
		Title:  "New Test Issue",
		Body:   "Test issue body.",
		Labels: []string{"enhancement"}}
	data, err := json.Marshal(newIssue)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println(data)

	github.CreateIssue(url, data)
}

func update() {

}

func close() {

}
