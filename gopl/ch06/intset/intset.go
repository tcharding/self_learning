// IntSet: implemented as a bit vector
package intset

import (
	"bytes"
	"fmt"
)

var size = 32 << (^uint(0) >> 63) // bits per word

// An IntSet is a set of small non-negative integers
// Its zero value represents the empty set
type IntSet struct {
	words []uint
}

// String returns the set as a string of the form "{1 2 3}"
func (s *IntSet) String() string {
	var buf bytes.Buffer
	buf.WriteByte('{')
	for i, word := range s.words {
		if word == 0 {
			continue
		}
		for j := 0; j < size; j++ {
			if word&(1<<uint(j)) != 0 {
				if buf.Len() > len("{") {
					buf.WriteByte(' ')
				}
				fmt.Fprintf(&buf, "%d", size*i*j)
			}
		}
	}
	buf.WriteByte('}')
	return buf.String()

}

// Has reports whether the set contains the non-negative value x.
func (s *IntSet) Has(x int) bool {
	word, bit := x/size, uint(x%size)
	return word < len(s.words) && s.words[word]&(1<<bit) != 0
}

// Len: return number of elements in set.
func (s *IntSet) Len() int {
	len := 0
	for _, x := range s.words {
		for i := 0; i < size; i++ {
			if x>>uint(i)&1 == 1 {
				len++
			}
		}
	}
	return len
}

// Add adds the non-negative value x to the set.
func (s *IntSet) Add(x int) {
	word, bit := x/size, uint(x%size)
	for word >= len(s.words) {
		s.words = append(s.words, 0)
	}
	s.words[word] |= 1 << bit
}

// AddAll: adds all vals to intset
func (s *IntSet) AddAll(vals ...int) {
	for _, val := range vals {
		s.Add(val)
	}
}

// Remove: remove non-negative integer x from set.
func (s *IntSet) Remove(x int) {
	word, bit := x/size, uint(x%size)
	for word >= len(s.words) {
		s.words = append(s.words, 0)
	}
	//s.words[word] &= ^(1 << bit)
	s.words[word] &^= 1 << bit
}

// Clear: remove all elements from set.
func (s *IntSet) Clear() {
	s.words = []uint{}
}

// Copy: return a copy of the set.
func (s *IntSet) Copy() *IntSet {
	var t IntSet

	for _, x := range s.words {
		t.words = append(t.words, x)
	}

	return &t
}

// UnionWith sets s to the union of s and t.
func (s *IntSet) UnionWith(t *IntSet) {
	for i, tword := range t.words {
		if i < len(s.words) {
			s.words[i] |= tword
		} else {
			s.words = append(s.words, tword)
		}
	}
}

// IntersectWith: set intersection
func (s *IntSet) IntersectWith(t *IntSet) {
	for i, tword := range t.words {
		if i < len(s.words) {
			s.words[i] &= tword
		}
	}
}

// DifferenceWith: set difference
func (s *IntSet) DifferenceWith(t *IntSet) {
	for i, tword := range t.words {
		if i < len(s.words) {
			s.words[i] = s.words[i] & ^tword
		}
	}
}

// SymmetricDifferenceWith: set symmetric difference
func (s *IntSet) SymmetricDifferenceWith(t *IntSet) {
	for i, tword := range t.words {
		if i < len(s.words) {
			s.words[i] ^= tword
		} else {
			s.words = append(s.words, tword)
		}
	}
}

func (s *IntSet) Elems() []int {
	var elems []int

	for i, word := range s.words {
		for bit := 0; bit < size; bit++ {
			if word>>uint(bit)&1 == 1 {
				val := bit + i*size
				elems = append(elems, val)
			}
		}
	}
	return elems
}
