// Play with packages in this directory
package main

import (
	"fmt"

	"github.com/tcharding/gopl/ch4/treesort"
)

func main() {
	nums := []int{4, 6, 3, 2, 0, 9, 1, 5, 7}
	treesort.Sort(nums)
	fmt.Printf("%v\n", nums)
}
