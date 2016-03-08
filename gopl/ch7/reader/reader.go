// Reader implements a simple string reader
package reader

type tstring string

func (t *tstring) Read(p []byte) (n int, err error) {
	for _, r := range *t {
		p = append(p, byte(r))
		n++
	}
	return n, nil
}

func NewReader(s string) tstring {
	return tstring(s)
}
