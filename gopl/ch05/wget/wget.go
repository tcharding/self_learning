// Wget: download web site recursively
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

var baseURL string

func Start(url string) error {
	baseURL = addProtocol(url)
	return breadthFirst(crawlAndSave, baseURL)
}

// addProtocol: adds http:// if not present
func addProtocol(url string) string {
	if strings.HasPrefix(url, "http") {
		return url
	}
	return "http://" + url
}

// breadthFirst calls f for each item in the worklist.
// Any items returned by f are added to the worklist.
// f is called at most once for each item.
func breadthFirst(f func(item string) []string, url string) error {
	err := createRootDir(baseURL)
	if err != nil {
		return err
	}
	seen := make(map[string]bool)
	worklist := []string{url}
	for len(worklist) > 0 {
		items := worklist
		worklist = nil
		for _, item := range items {
			if !seen[item] {
				seen[item] = true
				worklist = append(worklist, f(item)...)
			}
		}
	}
	return nil
}

// crawlAndSave: fetches url, makes local copy, returns list of internal links
func crawlAndSave(url string) []string {
	var list []string

	doc, err := fetch(url)

	if err != nil {
		return list // ignore invalid pages
	}
	savePage(doc, url)
	list, err = Extract(doc)
	if err != nil {
		log.Print(err)
	}
	return list
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

func savePage(n *html.Node, url string) error {
	p := pathFromURL(url)

	err := os.MkdirAll(path.Dir(p), 0755)
	if err != nil {
		return fmt.Errorf("failed to create directories from url: %s %v", url, err)
	}
	f, err := os.Create(p)
	if err != nil {
		return fmt.Errorf("failed to create file from url: %s %v", url, err)
	}
	err = html.Render(f, n)
	if err != nil {
		return fmt.Errorf("failed to render url: %s %v", url, err)
	}
	return f.Close()
}

// Extract list of internal links in an HTML document.
func Extract(n *html.Node) ([]string, error) {
	// fmt.Println(n.Attr.Namespace)
	// os.Exit(0)
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
	return links, nil
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
