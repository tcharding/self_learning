package intset

import "testing"

// InitXY initialises two intset's s={1, 2, 3} t={3, 4, 5}
func init2IntSets() (s, t IntSet) {
	s.Add(1)
	s.Add(2)
	s.Add(3)

	t.Add(3)
	t.Add(4)
	t.Add(5)

	return s, t
}

// TestIntSet tests functions provided by text
func TestIntSet(t *testing.T) {
	s, s2 := init2IntSets()

	if s.Has(1) != true {
		t.Errorf("has error: 1")
	}
	if s.Has(4) != false {
		t.Errorf("has not error: 1")
	}

	var z IntSet
	z.UnionWith(&s)
	z.UnionWith(&s2)
	if !z.Has(1) ||
		!z.Has(2) ||
		!z.Has(3) ||
		!z.Has(4) ||
		!z.Has(5) ||
		z.Len() != 5 {
		t.Errorf("UnionWith failed")
	}
}

func TestLen(t *testing.T) {
	s, _ := init2IntSets()
	var got, exp int

	got = s.Len()
	exp = 3
	if got != exp {
		t.Errorf("x.Len error: got: %d exp: %d", got, exp)
	}

	s.Add(10)
	got = s.Len()
	exp = 4
	if got != exp {
		t.Errorf("x.Len error: got: %d exp: %d", got, exp)
	}

	var r IntSet
	got = r.Len()
	exp = 0
	if got != exp {
		t.Errorf("y.Len error: got: %d exp: %d", got, exp)
	}
}

func TestRemove(t *testing.T) {
	s, _ := init2IntSets()
	var got, exp int

	got = s.Len()
	exp = 3
	if exp != got {
		t.Error("Remove: initial Len call failed")
	}

	s.Remove(3)
	if s.Has(3) {
		t.Errorf("Remove: failed to remove 3")
	}

	got = s.Len()
	exp = 2
	if exp != got {
		t.Errorf("Remove: Len after remove failed (got: %d exp: %d)", got, exp)
	}

	s.Remove(3) // remove non-existent
	got = s.Len()
	exp = 2

	if s.Has(3) || exp != got {
		t.Errorf("Remove: failed to remove non-existent")
	}

}

func TestClear(t *testing.T) {
	s, _ := init2IntSets()
	var got, exp int

	got = s.Len()
	exp = 3
	if exp != got {
		t.Errorf("Clear: initial Len call failed")
	}

	s.Clear()
	got = s.Len()
	exp = 0
	if exp != got {
		t.Errorf("Clear: failed to clear")
	}

}

func TestCopy(t *testing.T) {
	s, _ := init2IntSets()
	var sp *IntSet

	sp = s.Copy()

	if !sp.Has(1) ||
		!sp.Has(2) ||
		!sp.Has(3) ||
		sp.Len() != 3 {
		t.Errorf("Copy: failed src: %s dst: %s\n", s.String(), sp.String())
	}
}

func TestEquals(t *testing.T) {
	s, _ := init2IntSets()
	var s2 IntSet
	s2.Add(1)
	s2.Add(2)
	s2.Add(3)

	if !s.Equals(&s2) {
		t.Errorf("Equals failed")
	}
}

func TestAddAll(t *testing.T) {
	s, _ := init2IntSets()
	var s2 IntSet
	s2.AddAll(1, 2, 3)
	if !s.Equals(&s2) {
		t.Errorf("AddAll failed")
	}

}

func TestSetWithValues(t *testing.T) {
	s, _ := init2IntSets()
	sp := SetWithValues(1, 2, 3)
	if !s.Equals(sp) {

	}
}

func TestOperations(t *testing.T) {
	// s={1, 2, 3} s2={3, 4, 5}
	s, s2 := init2IntSets()
	var exp IntSet
	var combined IntSet
	var setUpExp = func(vals ...int) {
		exp.Clear()
		exp.AddAll(vals...)
	}

	combined.UnionWith(&s)
	setUpExp(1, 2, 3)
	if !exp.Equals(&combined) {
		t.Error("UnionWith failed")
	}
	combined.UnionWith(&s2)
	setUpExp(1, 2, 3, 4, 5)
	if !exp.Equals(&combined) {
		t.Error("UnionWith failed")
	}

	// IntersectWith
	combined.Clear()
	combined.UnionWith(&s)
	combined.IntersectWith(&s2)
	setUpExp(3)
	if !exp.Equals(&combined) {
		t.Errorf("Intersect failed: %s\n", combined.String())
	}

	// Difference (complement)
	combined.Clear()
	combined.UnionWith(&s)
	combined.DifferenceWith(&s2)
	setUpExp(1, 2)
	if !exp.Equals(&combined) {
		t.Errorf("Intersect failed: %s\n", combined.String())
	}

	// Symmetric Difference
	combined.Clear()
	combined.UnionWith(&s)
	combined.SymetricDifferenceWith(&s2)
	setUpExp(1, 2, 4, 5)
	if !exp.Equals(&combined) {
		t.Errorf("Intersect failed: %s\n", combined.String())
	}

}

func TestElems(t *testing.T) {
	s, _ := init2IntSets()
	set := s.Elems()
	exp := []int{1, 2, 3}

	for i, val := range exp {
		if set[i] != val {
			t.Errorf("Elems: failed on element index: %d exp: %v got: %v\n",
				i, exp, set)
		}
	}
}
