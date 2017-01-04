package memo

import (
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"sync"
	"testing"
	"time"
)

// function to use for testing
func httpGetBody(url string) (interface{}, error) {
	resp, err := http.Get(url)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()
	return ioutil.ReadAll(resp.Body)
}

func incomingURLs() []string {
	return []string{
		"http://tobin.cc",
		"http://google.com",
		"http://tobin.cc",
		"http://google.com"}
}

func Test(t *testing.T) {
	m := New(Func{f: httpGetBody, cancel: nil})
	var n sync.WaitGroup
	for _, url := range incomingURLs() {
		n.Add(1)
		go func(url string) {
			start := time.Now()
			value, err := m.Get(url)
			if err != nil {
				log.Print(err)
			}
			fmt.Printf("%s, %s, %d bytes\n",
				url, time.Since(start), len(value.([]byte)))
			n.Done()
		}(url)
	}
	n.Wait()
}

func TestCancel(t *testing.T) {
	url := "http://google.com"
	cancel := make(chan string)
	m := New(Func{f: httpGetBody, cancel: cancel})
	timedGet(m, url)
	time.Sleep(1 * time.Second)
	fmt.Printf("Get again after sleep\n")
	timedGet(m, url)
	fmt.Printf("Get again after cancel\n")
	cancel <- url
	timedGet(m, url)
}

func timedGet(m *Memo, url string) {
	start := time.Now()
	value, err := m.Get(url)
	if err != nil {
		log.Print(err)
	}
	fmt.Printf("%s, %s, %d bytes\n",
		url, time.Since(start), len(value.([]byte)))
}
