// Copyright © 2016 Alan A. A. Donovan & Brian W. Kernighan.
// License: https://creativecommons.org/licenses/by-nc-sa/4.0/

// See page 187.

// Sorting sorts a music playlist into a variety of orders.
package main

import (
	"bufio"
	"fmt"
	"html/template"
	"os"
	"sort"
	"strings"
	"time"
)

//!+main
type Track struct {
	Title  string
	Artist string
	Album  string
	Year   int
	Length time.Duration
}

var tracks = []*Track{
	{"Go", "Delilah", "From the Roots Up", 2012, length("3m38s")},
	{"Go", "Moby", "Moby", 1992, length("3m37s")},
	{"Go Ahead", "Alicia Keys", "As I Am", 2007, length("4m36s")},
	{"Ready 2 Go", "Martin Solveig", "Smash", 2011, length("4m24s")},
}

func length(s string) time.Duration {
	d, err := time.ParseDuration(s)
	if err != nil {
		panic(s)
	}
	return d
}

const templ = `
<!DOCTYPE html>
<html>
 <head>
  <title>Tracks</title>
 </head>
 <body>
  <table border="1">
   <tr>
    <th>Title</th>
    <th>Artist</th>
    <th>Album</th>
    <th>Year</th>
    <th>Length</th>
   </tr>
 {{range .}}
   <tr>
    <td>{{.Title}}</td>
    <td>{{.Artist}}</td>
    <td>{{.Album}}</td>
    <td>{{.Year}}</td>
    <td>{{.Length}}</td>
   </tr>
 {{end}}
  </table>
 </body>
</html>
`

var table = template.Must(template.New("tracklist").Parse(templ))

// use text/template to print tracks
func printTracks(tracks []*Track) {
	if err := table.Execute(os.Stdout, tracks); err != nil {
		fmt.Fprintf(os.Stderr, "Error printing tracks with template")
	}
}

// //!+printTracks
// func printTracks(tracks []*Track) {
// 	const format = "%v\t%v\t%v\t%v\t%v\t\n"
// 	tw := new(tabwriter.Writer).Init(os.Stdout, 0, 8, 2, ' ', 0)
// 	fmt.Fprintf(tw, format, "Title", "Artist", "Album", "Year", "Length")
// 	fmt.Fprintf(tw, format, "-----", "------", "-----", "----", "------")
// 	for _, t := range tracks {
// 		fmt.Fprintf(tw, format, t.Title, t.Artist, t.Album, t.Year, t.Length)
// 	}
// 	tw.Flush() // calculate column widths and print table
// }

// sort types

type byAlbum []*Track

func (x byAlbum) Len() int           { return len(x) }
func (x byAlbum) Less(i, j int) bool { return x[i].Album < x[j].Album }
func (x byAlbum) Swap(i, j int)      { x[i], x[j] = x[j], x[i] }

type byArtist []*Track

func (x byArtist) Len() int           { return len(x) }
func (x byArtist) Less(i, j int) bool { return x[i].Artist < x[j].Artist }
func (x byArtist) Swap(i, j int)      { x[i], x[j] = x[j], x[i] }

type byTitle []*Track

func (x byTitle) Len() int           { return len(x) }
func (x byTitle) Less(i, j int) bool { return x[i].Title < x[j].Title }
func (x byTitle) Swap(i, j int)      { x[i], x[j] = x[j], x[i] }

type byYear []*Track

func (x byYear) Len() int           { return len(x) }
func (x byYear) Less(i, j int) bool { return x[i].Year < x[j].Year }
func (x byYear) Swap(i, j int)      { x[i], x[j] = x[j], x[i] }

type multi []*Track

func (x multi) Len() int           { return len(x) }
func (x multi) Less(i, j int) bool { return multiLess(x[i], x[j]) }
func (x multi) Swap(i, j int)      { x[i], x[j] = x[j], x[i] }

//
//
//

type queue []string

func (q *queue) enqueue(front string) {
	var new []string
	new = append(new, front)
	for _, s := range *q {
		if s != front {
			new = append(new, s)
		}
	}
	*q = new
}

var order queue

func main() {
	//fmt.Printf("Sort by column. ")
	//promptAndSort()
	printTracks(tracks)
}

// promptAndSort: prompt user for sort order
func promptAndSort() error {
	q := &order
	reader := bufio.NewReader(os.Stdin)
	for {
		fmt.Printf("Enter a column (q to quit): ")
		input, err := reader.ReadString('\n')
		if err != nil {
			return err
		}

		input = strings.TrimSpace(input)

		switch input {
		case "q":
			return nil
		case "album":
			q.enqueue("album")
		case "year":
			q.enqueue("year")
		case "title":
			q.enqueue("title")
		case "artist":
			q.enqueue("artist")
		default:
			fmt.Printf("input error (or column unsupported): %s\n", input)
		}
		fmt.Printf("%v\n", *q)
		sort.Sort(multi(tracks))

		fmt.Println()
		printTracks(tracks)
		fmt.Println()
	}
}

// true if a is less than b
// uses queue to determine ordering
func multiLess(a, b *Track) bool {
	res := false
	for i := 0; i < len(order); i++ {
		if res == true {
			break
		}
		switch order[i] {
		case "artist":
			if a.Artist < b.Artist {
				res = true
			}
		case "album":
			if a.Album < b.Album {
				res = true
			}
		case "title":
			if a.Title < b.Title {
				res = true
			}
		case "year":
			if a.Year < b.Year {
				res = true
			}
		}
	}
	return res
}
