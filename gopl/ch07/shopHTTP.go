// shopHTTP: HTTP server example
package main

import (
	"fmt"
	"log"
	"net/http"
	"strings"
	"text/template"
)

func main() {
	db := database{"shoes": 50, "socks": 5}
	http.HandleFunc("/list", db.list)
	http.HandleFunc("/price", db.price)
	http.HandleFunc("/update", db.update)
	http.HandleFunc("/delete", db.delete)
	http.HandleFunc("/create", db.create)
	log.Fatal(http.ListenAndServe("localhost:8000", nil))
}

type dollars float32

func (d dollars) String() string {
	return fmt.Sprintf("$%.2f", d)
}

type database map[string]dollars

// price: get price for existing item
func (db database) price(w http.ResponseWriter, req *http.Request) {
	item := req.URL.Query().Get("item")
	price, ok := db[item]
	if !ok {
		w.WriteHeader(http.StatusNotFound)
		fmt.Fprintf(w, "no such item: %q\n", item)
		return
	}
	fmt.Fprintf(w, "%s\n", price)
}

// update the price of an existing item
func (db database) update(w http.ResponseWriter, req *http.Request) {
	item := req.URL.Query().Get("item")
	_, ok := db[item]
	if !ok {
		w.WriteHeader(http.StatusNotFound)
		fmt.Fprintf(w, "no such item: %q\n", item)
		return
	}
	price := priceOrError(w, req)
	db[item] = price
}

// create: add new item
func (db database) create(w http.ResponseWriter, req *http.Request) {
	item := req.URL.Query().Get("item")
	_, ok := db[item]
	if ok {
		w.WriteHeader(http.StatusBadRequest)
		fmt.Fprintf(w, "item exist (try update): %q\n", item)
		return
	}
	price := priceOrError(w, req)
	db[item] = price
}

// delete: remove an item
func (db database) delete(w http.ResponseWriter, req *http.Request) {
	item := req.URL.Query().Get("item")
	_, ok := db[item]
	if !ok {
		w.WriteHeader(http.StatusBadRequest)
		fmt.Fprintf(w, "no such item: %q\n", item)
		return
	}
	delete(db, item)
}

// priceOrError: parses price from request, writes error to w if encountered
func priceOrError(w http.ResponseWriter, req *http.Request) dollars {
	newPriceString := req.URL.Query().Get("price")
	newPrice, err := stringToDollars(newPriceString)
	if err != nil {
		w.WriteHeader(http.StatusBadRequest)
		fmt.Fprintf(w, "invalid price: %s\n", newPriceString)
		return dollars(-1)
	}
	return newPrice
}

// stringToDollars: parses string
func stringToDollars(price string) (dollars, error) {
	var f float32
	in := strings.NewReader(price)
	_, err := fmt.Fscan(in, &f)
	if err != nil {
		return dollars(0.0), err
	}
	return dollars(f), nil
}

// list: dump database as a table
func (db database) list(w http.ResponseWriter, req *http.Request) {
	if err := table.Execute(w, db); err != nil {
		w.WriteHeader(http.StatusInternalServerError)
	}
}

var table = template.Must(template.New("items").Parse(templ))

const templ = `
<!DOCTYPE html>
<html>
 <head>
  <title>Items</title>
 </head>
 <body>
  <table border="1">
   <tr>
    <th>Item</th>
    <th>Price</th>
   </tr>
{{ range $key, $value := . }}
   <tr>
    <td>{{ $key }}</td>
    <td>{{ $value }}</td>
{{ end }}
   </tr>
  </table>
 </body>
</html>
`
