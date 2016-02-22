// Serve a PNG image of the Mandelbrot fractal on port 8000
package main

import (
	"log"
	"net/http"

	"github.com/tcharding/gopl/ch3/mandelbrot"
)

func main() {
	http.HandleFunc("/", handler)
	log.Fatal(http.ListenAndServe("localhost:8000", nil))
}

func handler(w http.ResponseWriter, r *http.Request) {
	mandelbrot.Output(w)
}
