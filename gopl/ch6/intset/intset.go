// IntSet is a set of small non-negative integers. Implemented using bit vector
package intset

import (
	"bytes"
	"fmt"
)

// const bitsPerWord = 32 << (^uint(0) >> 63)
const bitsPerWord = 64

// Zero value represents empty set
type IntSet struct {
	words []uint64
}

// Has reports whether the set contains the non-negative value x.
func (s *IntSet) Has(x int) bool {
	word, bit := x/bitsPerWord, uint(x%bitsPerWord)
	return word < len(s.words) && s.words[word]&(1<<bit) != 0
}

// Add adds the non-negative value x to the set.
func (s *IntSet) Add(x int) {
	word, bit := x/bitsPerWord, uint(x%bitsPerWord)
	for word >= len(s.words) {
		s.words = append(s.words, 0)
	}
	s.words[word] |= 1 << bit
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

// String returns the set as a string of the form "{5 1 9}".
func (s *IntSet) String() string {
	var buf bytes.Buffer
	buf.WriteByte('{')
	for i, word := range s.words {
		if word == 0 {
			continue
		}
		for j := 0; j < bitsPerWord; j++ {
			if word&(1<<uint(j)) != 0 {
				if buf.Len() > len("{") {
					buf.WriteByte(' ')
				}
				fmt.Fprintf(&buf, "%d", bitsPerWord*i+j)
			}
		}
	}
	buf.WriteByte('}')
	return buf.String()
}

// Len return the number of elements
func (s *IntSet) Len() int {
	len := 0
	for _, word := range s.words {
		if word == 0 {
			continue
		}
		for j := 0; j < bitsPerWord; j++ {
			if word&(1<<uint(j)) != 0 {
				len++
			}
		}
	}
	return len
}

// Remove x from the set
func (s *IntSet) Remove(x int) {
	word, bit := x/bitsPerWord, uint(x%bitsPerWord)
	if word < len(s.words) {
		s.words[word] = s.words[word] & ^(1 << bit)
	}

}

// Clear remove all elements from set
func (s *IntSet) Clear() {
	*s = IntSet{}
}

// Copy returns a copy of the set
func (s *IntSet) Copy() *IntSet {
	var new IntSet
	//	copy(new.words, s.words)
	for _, val := range s.words { // why does copy not work?
		new.words = append(new.words, val)
	}

	return &new
}

func (s *IntSet) Equals(t *IntSet) bool {
	for i, val := range s.words {
		if t.words[i] != val {
			return false
		}
	}
	return true
}

func (s *IntSet) AddAll(vals ...int) {
	for _, val := range vals {
		s.Add(val)
	}
}

// SetWithValues initialises a new IntSet and adds vals
func SetWithValues(vals ...int) *IntSet {
	var s IntSet
	for _, val := range vals {
		s.Add(val)
	}
	return &s
}

func (s *IntSet) IntersectWith(t *IntSet) {
	for i, tword := range t.words {
		if i < len(s.words) {
			s.words[i] &= tword
		}
	}
}

func (s *IntSet) DifferenceWith(t *IntSet) {
	for i, tword := range t.words {
		if i < len(s.words) {
			s.words[i] &= ^tword
		}
	}
}

func (s *IntSet) SymetricDifferenceWith(t *IntSet) {
	for i, tword := range t.words {
		if i < len(s.words) {
			s.words[i] ^= tword
		}
	}
}

// Elems return slice representing the elements of a set
func (s *IntSet) Elems() []int {
	var set []int
	setBit := 0
	for _, word := range s.words {
		var wordBit uint = 0
		for ; wordBit < bitsPerWord; wordBit++ {
			if word&(1<<wordBit) != 0 {
				set = append(set, setBit)
			}
			setBit++
		}
	}
	return set
}
