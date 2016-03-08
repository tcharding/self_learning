// sorting interface example
package main

import (
	"fmt"
	"os"
	"sort"
	"text/tabwriter"
	"time"
)

type Track struct {
	Title  string
	Artist string
	Album  string
	Year   int
	Length time.Duration
}

var tracks = []*Track{
	{"Go", "Delilia", "From the Roots Up", 2012, length("3m38s")},
	{"Go", "Moby", "Moby", 1992, length("3m37s")},
	{"Go Ahead", "Alicia Keys", "As I Am", 2007, length("4m36s")},
	{"Ready to Go", "Martin Solveig", "Smash", 2001, length("4m24s")},
}

func main() {
	var input string
	printTracks(tracks)
	fmt.Println()

	for {
		fmt.Printf("\nChoose sort order (i.e artist, title etc), q to quit: ")
		fmt.Scanln(&input)

		switch input {
		case "title":
			sortByTitle(tracks)
		case "artist":
			sortByArtist(tracks)
		case "album":
			sortByAlbum(tracks)
		case "year":
			sortByYear(tracks)
		case "length":
			sortByLength(tracks)
		case "q":
			os.Exit(0)
		default:
			fmt.Printf("Unknown order: %s (artist, album, title, year length)\n",
				input)
			continue
		}
		fmt.Println()
		printTracks(tracks)
	}
}

func length(s string) time.Duration {
	d, err := time.ParseDuration(s)
	if err != nil {
		panic(s)
	}
	return d
}

func printTracks(tracks []*Track) {
	const format = "%v\t%v\t%v\t%v\t%v\t\n"
	tw := new(tabwriter.Writer).Init(os.Stdout, 0, 8, 2, ' ', 0)
	fmt.Fprintf(tw, format, "Title", "Artist", "Album", "Year", "Length")
	fmt.Fprintf(tw, format, "-----", "------", "-----", "----", "------")
	for _, t := range tracks {
		fmt.Fprintf(tw, format, t.Title, t.Artist, t.Album, t.Year, t.Length)
	}
	tw.Flush()
}

type byArtist []*Track

func (x byArtist) Len() int           { return len(x) }
func (x byArtist) Less(i, j int) bool { return x[i].Artist < x[j].Artist }
func (x byArtist) Swap(i, j int)      { x[i], x[j] = x[j], x[i] }

type customSort struct {
	t    []*Track
	less func(x, y *Track) bool
}

func (x customSort) Len() int           { return len(x.t) }
func (x customSort) Less(i, j int) bool { return x.less(x.t[i], x.t[j]) }
func (x customSort) Swap(i, j int)      { x.t[i], x.t[j] = x.t[j], x.t[i] }

func sortByArtist(tracks []*Track) {
	sort.Sort(customSort{tracks, func(x, y *Track) bool {
		return x.Artist < y.Artist
	}})
}

func sortByAlbum(tracks []*Track) {
	sort.Sort(customSort{tracks, func(x, y *Track) bool {
		return x.Album < y.Album
	}})
}

func sortByTitle(tracks []*Track) {
	sort.Sort(customSort{tracks, func(x, y *Track) bool {
		return x.Title < y.Title
	}})
}

func sortByYear(tracks []*Track) {
	sort.Sort(customSort{tracks, func(x, y *Track) bool {
		return x.Year < y.Year
	}})
}

func sortByLength(tracks []*Track) {
	sort.Sort(customSort{tracks, func(x, y *Track) bool {
		return x.Length < y.Length
	}})
}

// keep previous sort actions as state
type state struct {
	//	var seq []string
}

func (s *state) add(a string) {

}

func (s *state) LIFOSeq() []string {
	return nil
}

func remove(i int, s []string) []string {
	for ; i < len(s)-1; i++ {
		s[i] = s[i+1]
	}
	s[len(s)-1] = ""
	return s
}
