// mirroredFetch: example usage of canceling http requests
package main

import (
	"flag"
	"fmt"
	"net/http"
	"os"
)

func main() {
	flag.Parse()
	fetch(flag.Args())
}

type success struct {
	id   int
	resp *http.Response
}

type trReq struct {
	tr  *http.Transport
	req *http.Request
}

// fetch connects to multiple URL's,
// returns first to respond, cancels others
func fetch(urls []string) {
	trs := make([]trReq, len(urls))
	for i, url := range urls {
		req, _ := http.NewRequest("GET", url, nil)
		tr := &http.Transport{} // TODO: copy defaults from http.DefaultTransport
		trs[i] = trReq{tr, req}
	}
	done := make(chan success)
	for i, tr := range trs {
		id := i
		go func() {
			resp, _ := doRequest(tr)
			done <- success{
				id:   id,
				resp: resp,
			}
		}()
	}

	suc := <-done
	fmt.Fprintf(os.Stderr, "%v\n", suc.resp.Body)
	for i, tr := range trs {
		if suc.id == i {
			continue
		}
		trReq := tr
		trReq.tr.CancelRequest(trReq.req)
	}
}

func doRequest(trq trReq) (*http.Response, error) {
	client := &http.Client{Transport: trq.tr}
	return client.Do(trq.req)
}
