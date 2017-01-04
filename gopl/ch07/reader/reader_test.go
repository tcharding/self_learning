package reader

import (
	"io/ioutil"
	"testing"
)

func Test(t *testing.T) {
	s := "test"

	reader := NewReader(s)
	stuff, _ := ioutil.ReadAll(reader)
	if s != string(stuff) {
		t.Errorf("ioutil.RreadAll() failed, got: %s want: %s\n", stuff, s)
	}

	n := 2
	reader = NewReader(s)
	limReader := LimitReader(reader, int64(n))
	stuff, _ = ioutil.ReadAll(limReader)
	if s[:n] != string(stuff) {
		t.Errorf("ioutil.RreadAll() failed, got: %s want: %s\n", stuff, s)
	}

}
