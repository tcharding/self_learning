package limits

import "fmt"

func max(vals ...int) (int, error) {
	if len(vals) == 0 {
		return 0, fmt.Errorf("Error: cannot take maximum without values")
	}
	max := vals[0]

	for _, val := range vals {
		if val > max {
			max = val
		}
	}
	return max, nil
}

func min(vals ...int) (int, error) {
	if len(vals) == 0 {
		return 0, fmt.Errorf("Error: cannot take minimum without values")
	}
	min := vals[0]
	for _, val := range vals {
		if val < min {
			min = val
		}
	}
	return min, nil
}

func maxAtLeastOne(val int, vals ...int) int {
	max := val
	for _, v := range vals {
		if v > max {
			max = v
		}
	}
	return max
}

func minAtLeastOne(val int, vals ...int) int {
	min := val
	for _, v := range vals {
		if v < min {
			min = v
		}
	}
	return min
}
