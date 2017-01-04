// counters: fun with interfaces
package counters

import (
	"bufio"
	"bytes"
	"io"
)

type ByteCounter int

func (c *ByteCounter) Write(p []byte) (int, error) {
	*c += ByteCounter(len(p))
	return len(p), nil
}

type LineCounter int

func (c *LineCounter) Write(p []byte) (int, error) {
	scanner := bufio.NewScanner(bytes.NewReader(p))
	for scanner.Scan() {
		*c++
	}
	return int(*c), nil
}

type WordCounter int

func (c *WordCounter) Write(p []byte) (int, error) {
	scanner := bufio.NewScanner(bytes.NewReader(p))
	scanner.Split(bufio.ScanWords)
	for scanner.Scan() {
		*c++
	}
	return int(*c), nil
}

type WriteCounter struct {
	counter *int64
	writer  io.Writer
}

func (w WriteCounter) Write(b []byte) (int, error) {
	n, err := w.writer.Write(b)
	*w.counter += int64(n)

	return n, err
}

func CountingWriter(orig io.Writer) (io.Writer, *int64) {
	var w WriteCounter
	var counter int64

	w.writer = orig
	w.counter = &counter

	return w, &counter
}
