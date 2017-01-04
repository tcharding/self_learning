// web server displaying sortable table of tracks
package main

import (
	"fmt"
	"log"
	"net/http"
	"os"
	"sort"
	"sync"

	"github.com/tcharding/gopl/ch7/tracks"
)

// we can only handle one request at a time because of global write access to global
var collection = []*tracks.Track{
	{"Go", "Delilah", "From the Roots Up", 2012, tracks.Length("3m38s")},
	{"Go", "Moby", "Moby", 1992, tracks.Length("3m37s")},
	{"Go Ahead", "Alicia Keys", "As I Am", 2007, tracks.Length("4m36s")},
	{"Ready 2 Go", "Martin Solveig", "Smash", 2011, tracks.Length("4m24s")},
}

var mutex = &sync.Mutex{} // protect access to collection

func main() {
	http.HandleFunc("/", sortAndServe)
	log.Fatal(http.ListenAndServe("localhost:8000", nil))
}

func sortAndServe(w http.ResponseWriter, req *http.Request) {
	mutex.Lock()
	switch req.URL.Path {
	case "/":
		// no new sort order
	case "/artist":
		tracks.Enqueue("artist")
	case "/album":
		tracks.Enqueue("album")
	case "/title":
		tracks.Enqueue("title")
	case "/year":
		tracks.Enqueue("year")
	case "/length":
		tracks.Enqueue("length")

	default:
		w.WriteHeader(http.StatusNotFound) // 404
		fmt.Fprintf(w, "invalid sort order: %v\n", req.URL)
	}
	tracks.DebugQueue(os.Stderr)
	sort.Sort(tracks.Multi(collection))
	mutex.Unlock()
	tracks.PrintTracksHTML(w, collection)
}
