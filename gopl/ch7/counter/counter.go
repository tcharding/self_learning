// Counter provides byte, word, and line counting writers
package counter

import (
	"io"
	"unicode"
)

type ByteCounter int

func (c *ByteCounter) Write(p []byte) (int, error) {
	*c += ByteCounter(len(p)) // convert int to ByteCounter
	return len(p), nil
}

type WordCounter int

func (c *WordCounter) Write(p []byte) (int, error) {
	inWord := isFirstCharInWord(p)

	for _, byte := range p {
		if unicode.IsSpace(rune(byte)) {
			if inWord {
				*c++
				inWord = false
			}
		} else {
			if !inWord {
				inWord = true
			}
		}
	}
	if inWord {
		*c++ // count the last word
	}
	return len(p), nil
}

type LineCounter int

func (c *LineCounter) Write(p []byte) (int, error) {
	for _, byte := range p {
		if byte == '\n' {
			*c++
		}
	}
	return len(p), nil
}

func isFirstCharInWord(p []byte) bool {
	var inWord bool
	if unicode.IsSpace(rune(p[0])) {
		inWord = false
	} else {
		inWord = true
	}
	return inWord
}

type CountingWriter struct {
	count int64
	w     io.Writer
}

func (c *CountingWriter) Write(p []byte) (int, error) {
	writen, err := c.w.Write(p)
	c.count += int64(writen)
	return writen, err
}

func NewCountingWriter(w io.Writer) (io.Writer, *int64) {
	var cw CountingWriter
	return io.Writer(cw), &c.count
}
