package tracks

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"sort"
	"strings"
)

// sorting Stuff

func Enqueue(s string) error {
	switch s {
	case "artist", "album", "title", "year", "length":
		order.enqueue(s)
		return nil
	default:
		return fmt.Errorf("invalid sort string: %v", s)
	}
}

func DebugQueue(w io.Writer) {
	fmt.Fprintf(w, "%v\n", order)
}

func ClearQueue() {
	var clear queue
	order = clear
}

// queue used to do Multi level sort
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

type Multi []*Track

func (x Multi) Len() int           { return len(x) }
func (x Multi) Less(i, j int) bool { return multiLess(x[i], x[j]) }
func (x Multi) Swap(i, j int)      { x[i], x[j] = x[j], x[i] }

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
		case "length":
			if a.Length < b.Length {
				res = true
			}
		}
	}
	return res
}

// promptAndSort: prompt user for sort order
func promptAndSort(tracks []*Track) error {
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
		case "length":
			q.enqueue("length")
		default:
			fmt.Printf("input error (or column unsupported): %s\n", input)
		}
		fmt.Printf("%v\n", *q)
		sort.Sort(Multi(tracks))

		fmt.Println()
		PrintTracksTable(tracks)
		fmt.Println()
	}
}
