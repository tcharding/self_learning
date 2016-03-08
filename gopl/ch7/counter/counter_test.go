package counter

import (
	"fmt"
	"testing"
)

func TestByteCounter(t *testing.T) {
	var c ByteCounter
	var exp int

	c.Write([]byte("hello"))
	exp = 5
	if int(c) != exp {
		t.Errorf("ByteCounter, got: %d exp: %d", c, exp)
	}

	c = 0 // reset counter
	var name = "Dolly"
	fmt.Fprintf(&c, "hello, %s", name)
	exp = 12
	if int(c) != exp {
		t.Errorf("ByteCounter, got: %d exp: %d", c, exp)
	}
}

func TestWordCounter(t *testing.T) {
	var c WordCounter
	var exp int

	c.Write([]byte("one\ntwo\nthree\n"))
	exp = 3
	if int(c) != exp {
		t.Errorf("WordCounter write error, exp: %d got: %d\n", exp, int(c))
	}

}

func TestLineCounter(t *testing.T) {
	var c LineCounter
	var exp int

	c.Write([]byte("one\ntwo\nthree\n"))
	exp = 3
	if int(c) != exp {
		t.Errorf("LineCounter write error, exp: %d got: %d\n", exp, int(c))
	}
}
