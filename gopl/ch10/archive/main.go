// archive command reads zip or tar archives
package main

import (
	"archive/tar"
	"archive/zip"
	"flag"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"os"
	"strings"
)

var format = flag.String("format", "guess", "file format")

func main() {
	flag.Parse()
	if len(flag.Args()) == 0 {
		fmt.Fprintf(os.Stderr, "Usage: ./archive file.zip")
		os.Exit(1)
	}
	file := flag.Args()[0]
	switch *format {
	case "zip":
		writeZip(os.Stdout, file)
	default:
		switch {
		case strings.Contains(file, ".zip"):
			writeZip(os.Stdout, file)
		case strings.Contains(file, ".tar"):
			writeTar(os.Stdout, file)
		default:
			writeText(os.Stdout, file)
		}
	}

}

func writeText(w io.Writer, file string) {
	buf, err := ioutil.ReadFile(file)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Fprintf(w, "%s\n", string(buf))
}

func writeZip(w io.Writer, file string) {
	r, err := zip.OpenReader(file)
	if err != nil {
		log.Fatal(err)
	}
	defer r.Close()
	for _, f := range r.File {
		fmt.Fprintf(w, "contents of %s\n", f.Name)
		rc, err := f.Open()
		if err != nil {
			log.Println(err)
		}
		defer rc.Close()
		_, err = io.CopyN(w, rc, 68)
		if err != nil {
			log.Fatal(err)
		}
		fmt.Fprintln(w)
	}
}

func writeTar(w io.Writer, file string) error {
	f, err := os.Open(file)
	if err != nil {
		return err
	}
	defer f.Close()

	tr := tar.NewReader(f)
	// Iterate through the files in the archive.
	for {
		hdr, err := tr.Next()
		if err == io.EOF {
			// end of tar archive
			break
		}
		if err != nil {
			return err
		}
		fmt.Printf("Contents of %s:\n", hdr.Name)
		if _, err := io.Copy(w, tr); err != nil {
			log.Fatalln(err)
		}
		fmt.Println()
	}
	return nil
}
