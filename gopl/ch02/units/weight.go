package units

import "fmt"

type Kilogram float64
type Pound float64

const (
	LbsPerKg = 2.2046226218
)

func (k Kilogram) String() string {
	return fmt.Sprintf("%.2g Kgs", k)
}

func (p Pound) String() string {
	return fmt.Sprintf("%.2g Lbs", p)
}

// PToK converts a Pound weight to Kilograms
func PToK(p Pound) Kilogram {
	return Kilogram(p * (1 / LbsPerKg))
}

// KToP converts a Kilogram weight to Pounds
func KToP(k Kilogram) Pound {
	return Pound(k * LbsPerKg)
}
