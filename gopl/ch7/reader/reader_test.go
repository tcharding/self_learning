package reader

import (
	"fmt"
	"io"
	"testing"
)

func TestReader(t *testing.T) {
	var ts tstring = "violin music is loverly"
	tsp := &ts
	err := utiliseReader(tsp)
	if err != nil {
		t.Error(err)
	}
}

func utiliseReader(r *io.Reader) error {
	var buf []byte
	readn, err = r.Read(buf)
	if err != nil {
		return err
	}
	fmt.Printf("Read %d bytes: %v\n", buf)
}
