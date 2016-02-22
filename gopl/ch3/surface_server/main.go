// Surface computes an SVG rendering of a 3-D surface function
package main

//	"github.com/tcharding/gopl/ch3/surface"
import (
	"log"
	"net/http"

	"github.com/tcharding/gopl/ch3/surface"
)

func main() {
	http.HandleFunc("/", handler)
	log.Fatal(http.ListenAndServe("localhost:8000", nil))
}

func handler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "image/svg+xml")
	surface.SurfacePlot(w)
}
