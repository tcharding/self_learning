package links

import (
	"fmt"
	"testing"
)

func TestSubdirectory(t *testing.T) {
	var path string

	path = "/"
	urls := []string{
		"http://www.site.com",
		"http://www.site.com/",
		"www.site.com",
		"www.site.com/",
		"site.com",
		"site.com/",
	}
	for _, url := range urls {
		sub := subdirectory(url)
		if sub != path {
			t.Logf("Error: url: %s does not match path: %s sub: %s\n",
				url, path, sub)
		}
	}
}

func TestBaseURL(t *testing.T) {
	var urls []string = []string{
		"github.com",
		"www.github.com",
		"http://github.com",
		"http://www.github.com",
		"github.com/",
		"www.github.com/",
		"http://github.com/",
		"http://www.github.com/",
	}
	for _, url := range urls {
		if baseURL(url) != "github.com/" {
			fmt.Printf("Failed test baseURL: %s\n", url)
		}
	}

}

func TestStartOfBasename(t *testing.T) {
	var tc = map[string]int{
		"github.com":             0,
		"www.github.com":         4,
		"http://github.com":      7,
		"http://www.github.com":  11,
		"github.com/":            0,
		"www.github.com/":        4,
		"http://github.com/":     7,
		"http://www.github.com/": 11,
	}

	for url, exp := range tc {
		got := startOfBasename(url)
		if got != exp {
			fmt.Printf("Fail: startOfBasename url: %s (got: %d exp: %d)\n",
				url, got, exp)
		}
	}
}

func TestFail(t *testing.T) {
	t.Log("test fail")
}
