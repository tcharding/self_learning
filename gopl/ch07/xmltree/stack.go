// stack:  based on https://gist.github.com/bemasher/1777766
package xmltree

import "fmt"

type Stack struct {
	stack []Element
}

// Return the stack's length
func (sp *Stack) Len() int {
	return len(sp.stack)
}

// Push a new element onto the stack
func (sp *Stack) Push(n Element) {
	sp.stack = append(sp.stack, n)
}

// Remove the top element from the stack and return it's value
// If the stack is empty, return nil
func (sp *Stack) Pop() (Element, error) {
	var n Element

	if sp.Len() <= 0 {
		return n, fmt.Errorf("Stack empty")
	}

	lastIndex := sp.Len() - 1
	n = sp.stack[lastIndex]
	sp.stack = sp.stack[:lastIndex]
	return n, nil
}
