// tracks: store and sort track information (songs)
package tracks

import "time"

type Track struct {
	Title  string
	Artist string
	Album  string
	Year   int
	Length time.Duration
}

func Length(s string) time.Duration {
	d, err := time.ParseDuration(s)
	if err != nil {
		panic(s)
	}
	return d
}
