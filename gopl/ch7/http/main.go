// HTTP Web server showing use of default mutex handler
package main

import (
	"fmt"
	"log"
	"net/http"
	"strconv"
	"text/template"
)

func main() {
	db := database{"shoes": 50, "socks": 10}

	http.HandleFunc("/list", db.list)
	http.HandleFunc("/price", db.price)
	http.HandleFunc("/update", db.update)
	http.HandleFunc("/create", db.create)
	log.Fatal(http.ListenAndServe("localhost:8000", nil))
}

type dollars float64

func (d dollars) String() string {
	return fmt.Sprintf("$%.2f", d)
}

type database map[string]dollars

func (db database) list(w http.ResponseWriter, req *http.Request) {
	const tpl = `
<!DOCTYPE html>
<html>
	<head>
		<meta charset="UTF-8">
		<title>{{.Title}}</title>
	</head>
	<body>

<table style="width:30%">

{{range $key, $value := .}}
  <tr>
    <td>{{$key}}</td>
    <td>{{$value}}</td> 
  </tr>
{{end}}

</table>
	</body>
</html>`

	check := func(err error) {
		if err != nil {
			log.Fatal(err)
		}
	}
	t, err := template.New("webpage").Parse(tpl)
	check(err)

	err = t.Execute(w, db)
	check(err)

}

func (db database) price(w http.ResponseWriter, req *http.Request) {
	item := req.URL.Query().Get("item")
	price, ok := db[item]
	if !ok {
		w.WriteHeader(http.StatusNotFound) // 404
		fmt.Fprintf(w, "no such item: %q\n", item)
		return
	}
	fmt.Fprintf(w, "%s\n", price)
}

func (db database) update(w http.ResponseWriter, req *http.Request) {
	item := req.URL.Query().Get("item")
	_, ok := db[item]
	if !ok {
		w.WriteHeader(http.StatusNotFound) // 404
		fmt.Fprintf(w, "no such item: %q\n", item)
		return
	}
	price := req.URL.Query().Get("price")

	newPrice, err := strconv.Atoi(price)
	if err != nil {
		w.WriteHeader(http.StatusBadRequest)
		fmt.Fprintf(w, "no such item: %q\n", item)
		return
	}
	db[item] = dollars(newPrice)
	fmt.Fprintf(w, "new price: %s\n", dollars(newPrice))
}

func (db database) create(w http.ResponseWriter, req *http.Request) {
	item := req.URL.Query().Get("item")
	_, ok := db[item]
	if ok {
		w.WriteHeader(http.StatusNotFound) // 404
		fmt.Fprintf(w, "item already exists: %q\n", item)
		return
	}
	price := req.URL.Query().Get("price")
	newPrice, err := strconv.Atoi(price)
	if err != nil {
		w.WriteHeader(http.StatusBadRequest)
		fmt.Fprintf(w, "price format error: %q", price)
		return
	}
	db[item] = dollars(newPrice)
	fmt.Fprintf(w, "New item added: %q price: %s\n", item, dollars(newPrice))
}
