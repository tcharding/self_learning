package units

import "fmt"

type Mile float64
type Kilometre float64

const (
	KmsPerMile = 1.60934
)

func (k Kilometre) String() string {
	return fmt.Sprintf("%.2g Kms", k)
}

func (m Mile) String() string {
	return fmt.Sprintf("%.2g Mls", m)
}

// MToK converts a Mile distance to a Kilometre
func MToK(m Mile) Kilometre {
	return Kilometre(m * KmsPerMile)
}

// KToM converts a Kilometre distance to a Mile
func KToM(k Kilometre) Mile {
	return Mile(k * (1 / KmsPerMile))
}
