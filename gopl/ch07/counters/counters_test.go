package counters

import (
	"bytes"
	"fmt"
	"testing"
)

func TestByteCounter(t *testing.T) {
	var c ByteCounter
	input := []byte("hello")
	want := 5

	c.Write(input)
	if got := int(c); got != want {
		t.Errorf("c.Write(%s) = %d, want: %d", string(input), got, want)
	}

	c = 0 // reset counter
	var name = "Dolly"
	fmt.Fprintf(&c, "hello, %s", name)
	want = 12
	if got := int(c); got != want {
		t.Errorf("fmt.Fprintf(&c, \"hello, %s\", name) = %d, want: %d", name, got, want)
	}
}

func TestLineCounter(t *testing.T) {
	var c LineCounter
	input := []byte("some \n nice \n words")
	want := 3

	c.Write(input)
	if got := int(c); got != want {
		t.Errorf("c.Write(%s) = %d, want: %d", string(input), got, want)
	}

	c = 0 // reset counter
	var name = "Dolly"
	fmt.Fprintf(&c, "hello, %[1]s \n %[1]s \n %[1]s \n", name)
	want = 3
	if got := int(c); got != want {
		t.Errorf("fmt.Fprintf(&c, \"hello, %[1]s \n %[1]s \n %[1]s \n\", name) = %d, want: %d",
			name, got, want)
	}
}

func TestWordCounter(t *testing.T) {
	var c WordCounter
	input := []byte("hello")
	want := 1

	c.Write(input)
	if got := int(c); got != want {
		t.Errorf("c.Write(%s) = %d, want: %d", string(input), got, want)
	}

	c = 0 // reset counter
	var name = "Dolly"
	fmt.Fprintf(&c, "hello, %s", name)
	want = 2
	if got := int(c); got != want {
		t.Errorf("fmt.Fprintf(&c, \"hello, %s\", name) = %d, want: %d", name, got, want)
	}
}

// func CountingWriter(w io.Writer) (io.Writer, *int64) {
func TestCountingWriter(t *testing.T) {
	var data bytes.Buffer
	data.WriteString("hello")
	out := new(bytes.Buffer)

	w, cp := CountingWriter(out)
	w.Write(data.Bytes())

	if got := out.String(); got != "hello" {
		t.Errorf("fail, got:%v\n", got)
	}

	want := int64(5)
	if got := *cp; got != want {
		t.Errorf("counter fail, got: %d want: %d\n", got, want)
	}

}
