package wget

import "testing"

func TestPathFromURL(t *testing.T) {
	var tests = []struct {
		input string
		want  string
	}{
		{"http://tobin.cc", "tobin.cc/index.html"},
		{"http://tobin.cc/", "tobin.cc/index.html"},
		{"http://tobin.cc/resume.html", "tobin.cc/resume.html"},
		{"http://tobin.cc/dir/file.html", "tobin.cc/dir/file.html"},
	}
	for _, test := range tests {
		if got := pathFromURL(test.input); got != test.want {
			t.Errorf("pathFromURL(%q) = %s", test.input)
		}
	}

}
