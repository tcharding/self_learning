package popcount

import (
	"math/rand"
	"testing"
	"time"
)

func Test(t *testing.T) {
	var tests = []struct {
		input int
		want  int
	}{
		{1, 1},
		{3, 2},
	}
	for _, test := range tests {
		if got := PopCount(1); got != 1 {
			t.Errorf("PopCount(%d) = %d want: %d\n", test.input, got, test.want)
		}
	}

}

func TestOther(t *testing.T) {
	// Initialize a pseudo-random number generator.
	seed := time.Now().UTC().UnixNano()
	t.Logf("Random seed: %d", seed)
	rng := rand.New(rand.NewSource(seed))

	for i := 0; i < 1000; i++ {
		n := rng.Intn(1000) // random length up to 24
		un := uint64(n)
		if PopCount(un) != PopCount3(un) {
			t.Errorf("PopCount() and PopCount3 do not aggree: %d", un)
		}
	}
}

func BenchmarkPopCount(b *testing.B) {
	for i := 0; i < b.N; i++ {
		PopCount(uint64(i))
	}
}

func BenchmarkPopCount3(b *testing.B) {
	for i := 0; i < b.N; i++ {
		PopCount3(uint64(i))
	}
}

func BenchmarkPopCount4(b *testing.B) {
	for i := 0; i < b.N; i++ {
		PopCount4(uint64(i))
	}
}

func BenchmarkPopCount5(b *testing.B) {
	for i := 0; i < b.N; i++ {
		PopCount5(uint64(i))
	}
}
