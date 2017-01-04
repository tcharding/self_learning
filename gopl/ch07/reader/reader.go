// reader: play with interfaces
package reader

import (
	"io"
	"io/ioutil"
)

// ref: https://medium.com/@mschuett/golangs-reader-interface-bd2917d5ce83#.1dk6v80kp

type Reader struct {
	s string
	i int
}

func NewReader(toRead string) *Reader {
	return &Reader{toRead, 0}
}

func (r *Reader) Read(p []byte) (n int, err error) {
	if r.i >= len(r.s) {
		return 0, io.EOF
	}

	for i, b := range []byte(r.s) {
		p[i] = b
		r.i++
	}
	return len(r.s), nil

}

func LimitReader(r io.Reader, n int64) io.Reader {
	s, _ := ioutil.ReadAll(r)
	return NewReader(string(s[:n]))
}
