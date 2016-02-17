// Combination of server3 and the giff program from lissajous
package main

import (
	"fmt"
	"image"
	"image/color"
	"image/gif"
	"io"
	"log"
	"math"
	"math/rand"
	"net/http"
)

var palette = []color.Color{color.Black, color.RGBA{0x00, 0xFF, 0x00, 0xff}}

const (
	backgroundIndex = 0 // first color in palette
	linesIndex      = 1 // next color in palette
)

func main() {
	http.HandleFunc("/", defaultHandler)
	http.HandleFunc("/debug", debugHandler)
	http.HandleFunc("/lissajous", lissajousHandler)
	log.Fatal(http.ListenAndServe("localhost:8000", nil))
}

func defaultHandler(w http.ResponseWriter, r *http.Request) {
	var suggestion = getRandomSuggestion()
	fmt.Fprintf(w, "Requested page: %s does not exist, have you tried %s\n",
		r.URL, suggestion)
}

// handler echoes the HTTP request
func debugHandler(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, "%s %s %s\n", r.Method, r.URL, r.Proto)
	for k, v := range r.Header {
		fmt.Fprintf(w, "Header[%q] = %q\n", k, v)
	}
	fmt.Fprintf(w, "Host = %q\n", r.Host)
	fmt.Fprintf(w, "RemoteAddr = %q\n", r.RemoteAddr)
	if err := r.ParseForm(); err != nil {
		log.Print(err)
	}
	for k, v := range r.Form {
		fmt.Fprintf(w, "Form[%q] = %q\n", k, v)
	}
}

func lissajousHandler(w http.ResponseWriter, r *http.Request) {
	lissajous(w)
}

func lissajous(out io.Writer, ) {
	const (
		cycles  = 5     // number of complete x oscillator revolutions
		res     = 0.001 // angular resolution
		size    = 100   // image canvas covers [-size..+size]
		nframes = 64    // number of animation frames
		delay   = 8     // delay between frames in 10ms units
	)
	freq := rand.Float64() * 3.0 // relative frequency of y oscillator
	anim := gif.GIF{LoopCount: nframes}
	phase := 0.0 // phase difference
	for i := 0; i < nframes; i++ {
		rect := image.Rect(0, 0, 2*size+1, 2*size+1)
		img := image.NewPaletted(rect, palette)
		for t := 0.0; t < cycles*2*math.Pi; t += res {
			x := math.Sin(t)
			y := math.Sin(t*freq + phase)
			img.SetColorIndex(size+int(x*size+0.5), size+int(y*size+0.5),
				linesIndex)
		}
		phase += 0.1
		anim.Delay = append(anim.Delay, delay)
		anim.Image = append(anim.Image, img)
	}
	gif.EncodeAll(out, &anim) // NOTE: ignoring encoding errors
}

func getRandomSuggestion() string {
	var pages = []string{"debug", "lissajou"}
	var index int
	index = rand.Intn(2)

	return pages[index]
}
