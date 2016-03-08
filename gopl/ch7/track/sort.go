package track

import "sort"

type byArtist []*Track

func (x byArtist) Len() int           { return len(x) }
func (x byArtist) Less(i, j int) bool { return x[i].Artist < x[j].Artist }
func (x byArtist) Swap(i, j int)      { x[i], x[j] = x[j], x[i] }

type customSort struct {
	t    []*Track
	less func(x, y *Track) bool
}

func (x customSort) Len() int           { return len(x.t) }
func (x customSort) Less(i, j int) bool { return x.less(x.t[i], x.t[j]) }
func (x customSort) Swap(i, j int)      { x.t[i], x.t[j] = x.t[j], x.t[i] }

func SortByArtist(tracks []*Track) {
	sort.Sort(customSort{tracks, func(x, y *Track) bool {
		return x.Artist < y.Artist
	}})
}

func SortByAlbum(tracks []*Track) {
	sort.Sort(customSort{tracks, func(x, y *Track) bool {
		return x.Album < y.Album
	}})
}

func SortByTitle(tracks []*Track) {
	sort.Sort(customSort{tracks, func(x, y *Track) bool {
		return x.Title < y.Title
	}})
}

func SortByYear(tracks []*Track) {
	sort.Sort(customSort{tracks, func(x, y *Track) bool {
		return x.Year < y.Year
	}})
}

func SortByLength(tracks []*Track) {
	sort.Sort(customSort{tracks, func(x, y *Track) bool {
		return x.Length < y.Length
	}})
}
