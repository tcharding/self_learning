// lenconv converts feet to metres and vice-versa
package lenconv

import "fmt"

const feetPerMetre = 3.28

type Metre float64
type Feet float64

func (f Feet) String() string {
	var desc string
	if f == 1.0 {
		desc = "Foot"
	} else {
		desc = "Feet"
	}
	return fmt.Sprintf("%g %s", f, desc)
}

func (m Metre) String() string {
	desc := "Metre"
	if m != 1.0 {
		desc += "s"
	}
	return fmt.Sprintf("%g %s", m, desc)
}

// FToM converts feet to metres
func FToM(f Feet) Metre {
	return Metre(f / feetPerMetre)
}

// MToF converts metres to feet
func MToF(m Metre) Feet {
	return Feet(m * feetPerMetre)
}
