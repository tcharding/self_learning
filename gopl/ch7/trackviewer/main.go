package main

import (
	"fmt"
	"os"
	"time"

	"github.com/tcharding/gopl/ch7/track"
)

var tracks = []*track.Track{
	{"Go", "Delilia", "From the Roots Up", 2012, length("3m38s")},
	{"Go", "Moby", "Moby", 1992, length("3m37s")},
	{"Go Ahead", "Alicia Keys", "As I Am", 2007, length("4m36s")},
	{"Ready to Go", "Martin Solveig", "Smash", 2001, length("4m24s")},
}

func length(s string) time.Duration {
	d, err := time.ParseDuration(s)
	if err != nil {
		panic(s)
	}
	return d
}

func main() {
	var input string
	track.PrintTracks(tracks)
	fmt.Println()

	for {
		fmt.Printf("\nChoose sort order (i.e artist, title etc), q to quit: ")
		fmt.Scanln(&input)

		switch input {
		case "title":
			track.SortByTitle(tracks)
		case "artist":
			track.SortByArtist(tracks)
		case "album":
			track.SortByAlbum(tracks)
		case "year":
			track.SortByYear(tracks)
		case "length":
			track.SortByLength(tracks)
		case "q":
			os.Exit(0)
		default:
			fmt.Printf("Unknown order: %s (artist, album, title, year length)\n",
				input)
			continue
		}
		fmt.Println()
		track.PrintTracks(tracks)
	}
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
