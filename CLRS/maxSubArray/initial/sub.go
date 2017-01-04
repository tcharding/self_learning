// Find the maximum subarray
//
// Problem requires array to have some negative values
package initial

// bruteForce a solution: O(n^2)
func BruteForce(xs []int) (int, []int) {
	max := 0
	var slice []int

	for i := 0; i < len(xs); i++ {
		for j := i + 1; j < len(xs); j++ {
			total := sum(xs, i, j)
			if total > max {
				max = total
				slice = xs[i:j]
			}
		}
	}
	return max, slice
}

func sum(xs []int, i, j int) int {
	sum := 0
	for _, x := range xs[i:j] {
		sum += x
	}
	return sum
}
