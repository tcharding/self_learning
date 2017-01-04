// Issues: get issues from GitHub
package main

import (
	"fmt"
	"log"
	"os"
	"time"

	"github.com/tcharding/gopl/ch4/github"
)

func main() {
	listIssues(os.Args[1:], OAuth)
}

// listIssues: list all issues matching terms
func listIssues(terms []string, authToken string) {
	res, err := github.SearchIssues(terms, authToken)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Printf("%d issues\n", res.TotalCount)
	for _, issue := range res.Items {
		printIssue(issue)
	}
}

// listIssuesByDate: list all issues matching terms grouped by date
func listIssuesByDate(query []string) {
	res, err := github.SearchIssues(query, OAuth)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Printf("%d issues\n", res.TotalCount)

	fmt.Println("\nIssues this month:")
	for _, issue := range res.Items {
		if isThisMonth(issue) {
			printIssue(issue)
		}
	}

	fmt.Println("\nIssues this year:")
	for _, issue := range res.Items {
		if isThisYear(issue) && !isThisMonth(issue) {
			printIssue(issue)
		}
	}

	fmt.Println("\nIssues older than one year:")
	for _, issue := range res.Items {
		if !isThisYear(issue) && !isThisMonth(issue) {
			printIssue(issue)
		}
	}
}

func printIssue(i *github.Issue) {
	fmt.Printf("#%-5d %10.10s %.55s\n",
		i.Number, i.User.Login, i.Title)

}

func isThisMonth(i *github.Issue) bool {
	t := time.Now()
	return t.Month() == i.CreatedAt.Month()
}

func isThisYear(i *github.Issue) bool {
	t := time.Now()
	return t.Year() == i.CreatedAt.Year()
}
