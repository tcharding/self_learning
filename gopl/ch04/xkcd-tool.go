// read JSON comic from stdin, dump index entry to stdout

package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"os"
	"strings"
)

const (
	index   = "index.json"
	baseURL = "https:/xkd.com/" // must have trailing slash
)

type Comic struct {
	Num        int    `json:"num"`
	Transcript string `json:"transcript"`
}

var reindex = flag.Bool("index", false, "rebuild index")

func usage() {
	fmt.Printf("Usage: xkcd-tool [-index] search terms ...")
}

func main() {
	if len(os.Args) < 2 {
		usage()
	}
	flag.Parse()
	if *reindex {
		genIndex("xkcd")
	}
	grepIndex(flag.Args())
}

// genIndex: generate index file from JSON files in dir
func genIndex(dir string) {
	comics := parseComics(dir)

	f, err := os.Create(index)
	if err != nil {
		log.Fatal(err)
	}
	comicsToJSON(f, comics)
	f.Close()
}

//grepIndex: search index for terms, print to stdout
func grepIndex(terms []string) {
	comics := parseIndex()
	for _, comic := range comics {
		if grepComic(comic, terms) {
			printComic(comic)
		}
	}
}

// parseComics: parse input JSON, build list of comics
func parseComics(dir string) []Comic {
	files, err := ioutil.ReadDir(dir)
	if err != nil {
		log.Fatal(err)
	}
	comics := make([]Comic, len(files))
	for _, fi := range files {
		comics = append(comics, parseComic(dir+"/"+fi.Name()))
	}
	return comics
}

// comicsToJSON: print comics in human readable JSON
func comicsToJSON(out io.Writer, comics []Comic) {
	data, err := json.MarshalIndent(comics, "", "    ")
	if err != nil {
		log.Fatalf("JSON marshaling failed: %s", err)
	}
	fmt.Fprintf(out, "%s\n", data)
}

// parseIndex: parse JSON, build list of comics
func parseIndex() []Comic {
	var comics []Comic
	f, err := os.Open(index)
	if err != nil {
		log.Fatal(err)
	}
	if err := json.NewDecoder(f).Decode(&comics); err != nil {
		log.Fatal(err)
	}
	return comics
}

// grepComic: grep comic for terms
func grepComic(comic Comic, terms []string) bool {
	for _, term := range terms {
		if strings.Contains(comic.Transcript, term) {
			return true
		}
	}
	return false
}

// printComic: to stdout
func printComic(comic Comic) {
	fmt.Printf("%s%d: %s\n", baseURL, comic.Num, comic.Transcript)
}

// parseComic: parses JSON file and returns Comic
func parseComic(path string) Comic {
	var comic Comic
	f, err := os.Open(path)
	if err != nil {
		log.Fatal(err)
	}
	if err := json.NewDecoder(f).Decode(&comic); err != nil {
		log.Fatal(err)
	}
	return comic
}

func numToURL(n int) string {
	return baseURL + string(n)
}
