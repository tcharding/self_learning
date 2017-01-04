package insertionSort

// Sort sorts xs in place
func Sort(xs []int) {
	for j := 1; j < len(xs); j++ {
		key := xs[j]
		// insert xs[j] into sorted sequence xs[0:j-1]
		i := 0
		for i = j - 1; i >= 0 && xs[i] > key; i-- {
			xs[i+1] = xs[i]
		}
		xs[i+1] = key
	}
}
