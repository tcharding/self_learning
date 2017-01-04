// Copyright © 2016 Alan A. A. Donovan & Brian W. Kernighan.
// License: https://creativecommons.org/licenses/by-nc-sa/4.0/

// Run with "web" command-line argument for web server.
// See page 13.
//!+main

// Lissajous generates GIF animations of random Lissajous figures.
package main

import (
	"image"
	"image/color"
	"image/gif"
	"io"
	"math"
	"math/rand"
	"os"
)

//!-main
// Packages not needed by version in book.
import "time"

//!+main

var palette = []color.Color{color.Black,
	color.RGBA{0xFF, 0x00, 0x00, 0xFF},
	color.RGBA{0x00, 0xFF, 0x00, 0xFF},
	color.RGBA{0x00, 0x00, 0xFF, 0xFF}}

const (
	bgIndex    = 0
	redIndex   = 1
	greenIndex = 2
	blueIndex  = 3
)

func main() {
	rand.Seed(time.Now().UTC().UnixNano())

	lissajous(os.Stdout)
}

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
