package github

import (
	"fmt"
	"strings"
	"time"
)

func PrintTable(result *IssuesSearchResult) {
	for _, item := range result.Items {
		fmt.Printf("#%-5d %9.9s %.55s\n",
			item.Number, item.User.Login, item.Title)
	}
}

func PrintDateCreated(result *IssuesSearchResult) {
	issuesByDate := make(map[string]*Issue)

	// sort into three slices
	var thisMonth []string
	var thisYear []string
	var beforeThat []string

	for _, item := range result.Items {
		created_at := fmt.Sprintf("%s\n", item.CreatedAt)
		first_space := strings.Index(created_at, " ")
		date := created_at[:first_space]
		if isThisMonth(date) {
			thisMonth = append(thisMonth, date)
		} else if isThisYear(date) {
			thisYear = append(thisYear, date)
		} else {
			beforeThat = append(beforeThat, date)
		}
		issuesByDate[date] = item
	}

	dumpIssues(issuesByDate, thisMonth, thisYear, beforeThat)

}

func isThisMonth(date string) bool {
	today := todaysDate()
	todaysMonth := monthFromDate(today)
	month := monthFromDate(date)
	if month == todaysMonth && isThisYear(date) {
		return true
	}
	return false
}

func isThisYear(date string) bool {
	today := todaysDate()
	todaysYear := yearFromDate(today)
	year := yearFromDate(date)
	return year == todaysYear

}

func dumpIssues(issuesByDate map[string]*Issue, thisMonth []string, thisYear []string, beforeThat []string) {
	fmt.Printf("\nThis Month\n\n")
	dumpIssuesForSlice(issuesByDate, thisMonth)
	fmt.Printf("\nThis Year\n\n")
	dumpIssuesForSlice(issuesByDate, thisYear)
	fmt.Printf("\nOlder than One Year\n\n")
	dumpIssuesForSlice(issuesByDate, beforeThat)
}

// get month from date, form: 2016-01-21
func monthFromDate(date string) string {
	dash := strings.Index(date, "-")
	return date[dash+1 : dash+3]
}

// get year from date, form: 2016-01-21
func yearFromDate(date string) string {
	return date[:5]
}

func dumpIssuesForSlice(issues map[string]*Issue, s []string) {
	sortInPlace(s)
	for _, date := range s {
		item := issues[date]
		fmt.Printf("#%-5d %g %9.9s %.55s\n",
			item.Number, item.Score, item.User.Login, item.Title)
	}
}

func sortInPlace(s []string) {
	// TODO
}

func todaysDate() string {
	dateString := fmt.Sprintf("%v\n", time.Now())
	firstSpace := strings.Index(dateString, " ")
	return dateString[:firstSpace]
}
