// benchmarks
package intset

import "testing"

func BenchmarkAnd(b *testing.B) {
	var p IntSet
	for i := 0; i < b.N; i++ {
		p.Add(i)
	}
}

func BenchmarkUnionWith(b *testing.B) {
	var p, q IntSet
	n := 1000
	for i := 0; i < n/2; i++ {
		p.Add(i)
	}
	for i := n / 2; i < n; i++ {
		q.Add(i)
	}

	for i := 0; i < b.N; i++ {
		p.UnionWith(&q)
	}
}
