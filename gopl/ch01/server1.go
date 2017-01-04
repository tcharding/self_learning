// Server1 is a minimal "echo" server
package main

import (
	"image"
	"image/color"
	"image/gif"
	"io"
	"log"
	"math"
	"math/rand"
	"net/http"
	"time"
)

func main() {
	rand.Seed(time.Now().UTC().UnixNano())

	handler := func(w http.ResponseWriter, r *http.Request) {
		lissajous(w)
	}

	http.HandleFunc("/", handler) // called for each request
	log.Fatal(http.ListenAndServe("localhost:8000", nil))
}

// handler echoes the Path component of the request URL r.
//func handler(w http.ResponseWriter, r *http.Request) {
//	fmt.Fprintf(w, "URL.Path = %q\n", r.URL.Path)
//}

var palette = []color.Color{color.Black, color.RGBA{0x00, 0xFF, 0x00, 0xFF}}

const (
	bgIndex = 0
	fgIndex = 1
)

func lissajous(out io.Writer) {
	const (
		cycles  = 5     // number of complete x oscillator revolutions
		res     = 0.001 // angular resolution
		size    = 100   // image canvas covers [-size..+size]
		nframes = 64    // number of animation frames
		delay   = 8     // delay between frames in 10ms units
	)
	var fgIndex uint8
	freq := rand.Float64() * 3.0 // relative frequency of y oscillator
	anim := gif.GIF{LoopCount: nframes}
	phase := 0.0 // phase difference
	for i := 0; i < nframes; i++ {
		rect := image.Rect(0, 0, 2*size+1, 2*size+1)
		img := image.NewPaletted(rect, palette)
		for c := 0; c < cycles; c++ {
			fgIndex = uint8(math.Mod(float64(fgIndex+1), float64(len(palette))))
			for t := 0.0; t < cycles*2*math.Pi; t += res {
				x := math.Sin(t)
				y := math.Sin(t*freq + phase)
				img.SetColorIndex(size+int(x*size+0.5), size+int(y*size+0.5),
					fgIndex)
			}
		}
		phase += 0.1
		anim.Delay = append(anim.Delay, delay)
		anim.Image = append(anim.Image, img)
	}
	gif.EncodeAll(out, &anim) // NOTE: ignoring encoding errors
}
