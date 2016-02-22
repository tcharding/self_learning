// Surface produces an SVG image for function f
package surface

import (
	"fmt"
	"io"
	"math"
)

const (
	width, height = 600, 320            // canvas size in pixels
	cells         = 100                 // number of grid cells
	xyrange       = 30.0                // axis ranges (-xyrange..+xyrange)
	xyscale       = width / 2 / xyrange // pixels per x or y unit
	zscale        = height * 0.4        // pixels per z unit
	angle         = math.Pi / 6         // angle of x, y axes (=30 degrees)
)

var sin30, cos30 = math.Sin(angle), math.Cos(angle) // Sin(30), Cos(30)

func SurfacePlot(w io.Writer) {
	fmt.Fprintf(w, "<svg xmlns='http://www.w3.org/2000/svg' "+
		"style='stroke: grey; fill: white; stroke-width: 0,7' "+
		"width='%d' height='%d'>", width, height)
	for i := 0; i < cells; i++ {
		for j := 0; j < cells; j++ {
			ax, ay, ok1 := corner(i+1, j)
			bx, by, ok2 := corner(i, j)
			cx, cy, ok3 := corner(i, j+1)
			dx, dy, ok4 := corner(i+1, j+1)
			if !(ok1 && ok2 && ok3 && ok4) {
				continue // skip non-finite values
			}
			out := xmlFromPoints(ax, ay, bx, by, cx, cy, dx, dy)
			fmt.Fprintf(w, out)
		}
	}
	fmt.Fprintf(w, "</svg>")
}

func corner(i, j int) (float64, float64, bool) {
	// Find poin (x,y) at corner of cell (i,j)
	x := xyrange * (float64(i)/cells - 0.5)
	y := xyrange * (float64(j)/cells - 0.5)

	// Compute surface height z.
	z := f(x, y)
	if isNotFinite(z) {
		return 0, 0, false
	}

	// Project (x,y,z) isometrically onto 2-D SVG canvas (sx,sy).angle
	sx := width/2 + (x-y)*cos30*xyscale
	sy := height/2 + (x+y)*sin30*xyscale - z*zscale
	return sx, sy, true
}

func xmlFromPoints(ax, ay, bx, by, cx, cy, dx, dy float64) string {
	color := colorPolygon(ax, ay, bx, by, cx, cy, dx, dy)
	style := StyleString(color)
	xml := fmt.Sprintf("<polygon points='%g,%g %g,%g %g,%g %g,%g' "+style+" />\n",
		ax, ay, bx, by, cx, cy, dx, dy)
	return xml
}

func colorPolygon(ax, ay, bx, by, cx, cy, dx, dy float64) int {
	const (
		max = 0xff0000
		min = 0x0000ff
	)
	average := (ax + ay + bx + by + cx + cy + dx + dy) / 8.0
	//	normalized := (average - min) / (max - min)
	//	return int((normalized * (max - min)) + min)
	return int(average)
}

// return string for use in xml (form: style='fill:#ff0000')
func StyleString(color int) string {
	return fmt.Sprintf("style='fill:%s'", intToColor(color))
}

func f(x, y float64) float64 {
	r := math.Hypot(x, y) // distance from (0,0)
	return math.Sin(r) / r
}

func isNotFinite(f float64) bool {
	return !(f >= math.MinInt64 && f <= math.MaxFloat64)
}

// return color string (#FF0000)
func intToColor(i int) string {
	string := fmt.Sprintf("%x", i)
	for len(string) < 6 {
		string = "0" + string
	}
	return "#" + string
}
