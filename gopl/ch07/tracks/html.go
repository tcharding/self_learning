package tracks

import (
	"fmt"
	"io"
	"os"
	"text/template"
)

// output tracks as HTML

const templ = `
<!DOCTYPE html>
<html>
 <head>
  <title>Tracks</title>
 </head>
 <body>
  <table border="1">
   <tr>
    <th><a href="title">Title</a></th>
    <th><a href="artist">Artist</a></th>
    <th><a href="album">Album</a></th>
    <th><a href="year">Year</a></th>
    <th><a href="length">Length</a></th>
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

// use text/html to print tracks
func PrintTracksHTML(w io.Writer, tracks []*Track) {
	if err := table.Execute(w, tracks); err != nil {
		fmt.Fprintf(os.Stderr, "Error printing tracks with template")
	}
}
