package intset

import (
	"math/rand"
	"testing"
	"time"
)

// initial simple tests on single intset
func TestSimpleUnary(t *testing.T) {
	var p IntSet
	val := 1
	not := 0

	p.Add(val)
	if got := p.Len(); got != 1 {
		t.Errorf("p.Len() = 1, got: %v\n", got)
	}

	if got := p.Has(val); got == false {
		t.Errorf("p.Has(val) == true, got: %v\n", got)
	}

	if got := p.Has(not); got == true {
		t.Errorf("p.Has(not) == false, got: %v\n", got)
	}

	p.Remove(val)

	if got := p.Len(); got != 0 {
		t.Errorf("p.Len() = 0, got: %v\n", got)
	}

	if got := p.Has(val); got != false {
		t.Errorf("p.Has(val) == false, got: %v\n", got)
	}

	p.Add(val)
	p.Clear()

	if got := p.Len(); got != 0 {
		t.Errorf("p.Len() = 0, got: %v\n", got)
	}

	if got := p.Has(val); got != false {
		t.Errorf("p.Has(val) == false, got: %v\n", got)
	}
}

func TestUnion(t *testing.T) {
	var p, q IntSet

	p.Add(1)
	p.Add(2)

	q.Add(5)
	q.Add(6)

	p.UnionWith(&q)

	vals := []int{1, 2, 5, 6}
	for _, val := range vals {
		if p.Has(val) == false {
			t.Errorf("false negative: %v\n", val)
		}
	}
	vals = []int{0, 3, 4, 7, 8}
	for _, val := range vals {
		if p.Has(val) == true {
			t.Errorf("false positive: %v\n", val)
		}
	}
}

func TestCopy(t *testing.T) {
	var p IntSet
	var qp *IntSet

	p.Add(1)
	p.Add(2)

	qp = p.Copy()
	if qp.Len() != 2 ||
		qp.Has(1) != true ||
		qp.Has(2) != true ||
		qp.Has(3) != false ||
		qp.Has(0) != false {
		t.Errorf("qp does not appear to be a true copy")
	}

	p.Clear()
	if len := qp.Len(); len != 2 {
		t.Errorf("storage appears to be shared between copies")
	}
}

func TestRandomizedInput(t *testing.T) {
	// Initialize a pseudo-random number generator.
	seed := time.Now().UTC().UnixNano()
	t.Logf("Random seed: %d\n", seed)
	rng := rand.New(rand.NewSource(seed))

	var s IntSet
	input := randomInts(rng)
	for _, val := range input {
		s.Add(val)
	}
	for _, val := range input {
		if s.Has(val) == false {
			t.Errorf("s.Has(%d) = true", val)
		}
	}

	var q IntSet
	q.AddAll(input...)
	for _, val := range input {
		if s.Has(val) == false {
			t.Errorf("s.Has(%d) = true", val)
		}
	}
}

// generate pseudo-random list of integers
func randomInts(rng *rand.Rand) []int {
	len := rng.Intn(500)
	var ints []int
	for i := 0; i < len; i++ {
		ints = append(ints, rng.Intn(256))
	}
	return ints
}

func TestIntersectWith(t *testing.T) {
	var p, q IntSet

	p.Add(1)
	p.Add(2)
	p.Add(3)

	q.Add(3)
	q.Add(4)
	q.Add(5)

	// Test union
	union := []int{1, 2, 3, 4, 5}
	nonUnion := []int{0, 6}
	cp := p.Copy()
	hasHasNot(cp, &q, union, nonUnion, cp.UnionWith, t, "UnionWith")

	// Test intersection
	intersection := []int{3}
	nonIntersection := []int{0, 1, 2, 4, 5, 6, 7}
	cp = p.Copy()
	hasHasNot(cp, &q, intersection, nonIntersection, cp.IntersectWith, t, "IntersectWith")

	// Test difference
	difference := []int{1, 2}
	nonDifference := []int{0, 3, 4, 5, 6, 7}
	cp = p.Copy()
	hasHasNot(cp, &q, difference, nonDifference, cp.DifferenceWith, t, "DiffferenceWith")

	// Test symmetricDifferenc
	symmetricDifference := []int{1, 2, 4, 5}
	nonSymmetricDifference := []int{0, 3, 6}
	cp = p.Copy()
	hasHasNot(cp, &q, symmetricDifference, nonSymmetricDifference,
		cp.SymmetricDifferenceWith, t, "SymmetricDifferenceWith")
}

func hasHasNot(cp, q *IntSet, has, hasNot []int, fn func(s *IntSet), t *testing.T, fnName string) {
	fn(q) // cp is modified by call to fn
	vals := has
	for _, val := range vals {
		if cp.Has(val) == false {
			t.Errorf("%s: false negative: %d\n", fnName, val)
		}
	}
	vals = hasNot
	for _, val := range vals {
		if cp.Has(val) == true {
			t.Errorf("%s: false positive: %d\n", fnName, val)
		}
	}

}

// func (s *IntSet) Elems() []int {
func TestElems(t *testing.T) {
	var p IntSet
	data := []int{1, 2, 3}
	p.AddAll(data...)

	want := data
	got := p.Elems()

	for _, val := range got {
		if !ints(want).contains(val) {
			t.Errorf("val not found: %v in 'want'. Got %v want %v\n", val, got, want)
		}
	}

	for _, val := range want {
		if !ints(got).contains(val) {
			t.Errorf("val not found: %v in 'got'. Got %v want %v\n", val, got, want)
		}
	}
}

type ints []int

// we should use binary search for this since int lists will be sorted
func (is ints) contains(i int) bool {
	for _, v := range is {
		if v == i {
			return true
		}
	}
	return false
}
