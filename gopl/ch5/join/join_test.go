package join

import "testing"

func TestJoin(t *testing.T) {
	var exp, got string

	exp = "path/"
	got = Join("/", "path")
	if exp != got {
		t.Errorf("join: exp: %s got: %s\n", exp, got)
	}
	exp = "path/to/dir/"
	got = Join("/", "path", "to", "dir")
	if exp != got {
		t.Errorf("join: exp: %s got: %s\n", exp, got)
	}
	exp = "this"
	got = Join("", "t", "h", "i", "s")
	if exp != got {
		t.Errorf("join: exp: %s got: %s\n", exp, got)
	}

}
