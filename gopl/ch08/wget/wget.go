package wget

import (
	"fmt"
	"log"
	"net/http"
	"net/url"
	"os"
	"path"
	"strings"

	"golang.org/x/net/html"
)

const crawlLimit = 20 // limit on number of goroutines

// tokens is a counting semaphore used to
// enforce a limit on concurrent requests.
var tokens = make(chan struct{}, crawlLimit)

var baseURL string

func Start(depth int, url string) error {
	baseURL = addProtocol(url)
	if err := createRootDir(baseURL); err != nil {
		return err
	}
	getURL(baseURL, depth)
	return nil
}

// addProtocol: adds http:// if not present
func addProtocol(url string) string {
	if strings.HasPrefix(url, "http") {
		return url
	}
	return "http://" + url
}

// createRootDir: creates root dir for wget download
func createRootDir(url string) error {
	dirName, err := rootDirFromURL(url)
	if err != nil {
		return err
	}
	err = os.Mkdir(dirName, 0755)
	if err != nil {
		return fmt.Errorf("failed to create dir: %s %v", dirName, err)
	}
	return nil
}

type depthList struct {
	depth int
	list  []string
}

// getURL: crawls web starting at url, stopping at specified depth
func getURL(url string, depth int) {
	worklist := make(chan depthList)
	var n int // number of pending sends to worklist

	// Start with the command-line arguments.
	n++
	go func() {
		worklist <- depthList{0, []string{url}}
	}()

	// Crawl the web concurrently.
	seen := make(map[string]bool)
	for ; n > 0; n-- {
		dl := <-worklist
		if depth == 0 || dl.depth <= depth {
			for _, link := range dl.list {
				if !seen[link] {
					seen[link] = true
					n++
					go func(link string) {
						subDl := depthList{dl.depth + 1, crawl(link)}
						worklist <- subDl
					}(link)
				}
			}
		}
	}
}

// rootDirFromURL: returns directory name used as root of download
func rootDirFromURL(url string) (string, error) {
	i := strings.Index(url, "//")
	if i == -1 {
		return "", fmt.Errorf("ill formed url: %s", url)
	}
	sofar := url[i+2:]
	end := strings.Index(sofar, "/")
	if (end) == -1 {
		end = len(sofar)
	}
	return string(sofar[:end]), nil
}

// crawl url calling extracting internal links and making local copy of page
func crawl(url string) []string {
	tokens <- struct{}{} // acquire a token

	n, err := fetch(url)
	if err != nil {
		var empty []string
		return empty
	}
	list := extract(n)
	if err := makeLocalCopy(n, url); err != nil {
		log.Print(err)
	}

	<-tokens // release the token
	return list
}

// fetch: make HTTP GET request to url and parse,
// returns node and filename (i.e index.html)
func fetch(url string) (*html.Node, error) {
	url = addProtocol(url)
	resp, err := http.Get(url)
	if err != nil {
		return nil, err
	}
	if resp.StatusCode != http.StatusOK {
		resp.Body.Close()
		return nil, fmt.Errorf("getting %s: %s", url, resp.Status)
	}
	doc, err := html.Parse(resp.Body)
	resp.Body.Close()
	if err != nil {
		return nil, fmt.Errorf("parsing %s as HTML: %v", url, err)
	}
	return doc, nil
}

// makeLocalCopy: render node and save to disk
func makeLocalCopy(n *html.Node, url string) error {
	p := pathFromURL(url)

	if err := localizeLinks(n); err != nil {
		log.Println(err)
	}

	if err := os.MkdirAll(path.Dir(p), 0755); err != nil {
		return fmt.Errorf("failed to create directories from url: %s %v", url, err)
	}

	f, err := os.Create(p)
	if err != nil {
		return fmt.Errorf("failed to create file from url: %s %v", url, err)
	}

	if err := html.Render(f, n); err != nil {
		f.Close() // ignore error
		return fmt.Errorf("failed to render url: %s %v", url, err)
	}
	return f.Close() // don't defer close so we can return the error
}

// extract list of internal links in an HTML document.
func extract(n *html.Node) []string {
	var links []string
	visitNode := func(n *html.Node) {
		if n.Type == html.ElementNode && n.Data == "a" {
			for _, a := range n.Attr {
				if a.Key != "href" {
					continue
				}
				if isInternalURL(a.Val) == true {
					links = append(links, buildURL(a.Val))
				}
			}
		}
	}
	forEachNode(n, visitNode, nil)
	return links
}

// isInternalURL: true if url is under baseURL or is not fully qualified
func isInternalURL(url string) bool {
	if strings.HasPrefix(url, "http") == true &&
		strings.HasPrefix(url, baseURL) == false {
		return false
	}
	return true
}

// buildURL: builds URL from link address
func buildURL(addr string) string {
	if strings.HasPrefix(addr, "http") == true {
		return addr
	}
	if addr != "" && addr[0] == '#' {
		addr = addr[1:]
	}
	return addPathToDomain(addr)
}

// forEachNode: calls pre(x) and post(x) if non nil,
// functions enable preorder and postorder tree traversal
func forEachNode(n *html.Node, pre, post func(n *html.Node)) {
	if pre != nil {
		pre(n)
	}
	for c := n.FirstChild; c != nil; c = c.NextSibling {
		forEachNode(c, pre, post)
	}
	if post != nil {
		post(n)
	}
}

// addPathToDomain: build URL from path and global baseURL
func addPathToDomain(p string) string {
	u, err := url.Parse(baseURL)
	if err != nil {
		log.Fatal(err)
	}
	u.Path = path.Join(u.Path, p)
	url := u.String()

	if url[len(url)-1] == '/' {
		url = url[:len(url)-1]
	}

	return url
}

// pathFromURL: http://example.com/path/to/index.html -> path/to/index.html
func pathFromURL(url string) string {
	index := "index.html" // FIXME: we need to get this from the server

	root, err := rootDirFromURL(url)
	if err != nil {
		log.Fatalf("ill formed url %s", url)
	}
	i := strings.Index(url, "//")
	if i == -1 {
		log.Fatalf("ill formed url: %s", url)
	}
	sofar := url[i+2:]
	i = strings.Index(string(sofar), "/")
	if i == -1 || i == len(sofar)-1 {
		return path.Join(root, index)
	}

	return path.Join(root, string(sofar[i+1:]))
}

// localizeLinks: convert http://address.com/page.html to page.html
func localizeLinks(n *html.Node) error {
	log.Println("localizeLinks not implemented\n")
	return nil
}
