// Fun with enumerators
package main

import "fmt"

type Weekday int

const (
	Sunday Weekday = iota
	Monday
	Tuesday
	Wednesday
	Thursday
	Friday
	Saturday
)

type Flags uint

const (
	FlagUp Flags = 1 << iota // is up
	FlagBroadcast
	FlagLoopback
)

const (
	_ = 1 << (10 * iota)
	KiB
	MiB
	GiB
)

const (
	KB = 1000
	MB = KB * 1000
	GB = MB * 1000
)

func main() {
	fmt.Printf("Monday: %d\n", Monday)
	fmt.Printf("KiB: %v MiB: %v\n", KiB, MiB)
	fmt.Printf("KB: %v MB: %v GB: %v\n", KB, MB, GB)
}

func IsUp(v Flags) bool {
	return v&FlagUp == FlagUp
}
