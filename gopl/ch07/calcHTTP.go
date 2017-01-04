// HTTP calculator
// serves web page for calculation input and returns evaluated expression
// handles variables and float literals
package main

import (
	"fmt"
	"log"
	"net/http"
	"os"
	"strconv"
	"strings"

	"github.com/tcharding/gopl/ch7/eval"
)

func main() {
	http.HandleFunc("/", outForm)
	http.HandleFunc("/save", evaluate)
	log.Fatal(http.ListenAndServe("localhost:8000", nil))
}

// outForm: print HTML to w for calculator form
func outForm(w http.ResponseWriter, req *http.Request) {
	fmt.Fprintf(w, `
<!DOCTYPE html>
 <html>
  <body>

<p><strong>Calculator</strong></p>

  <form action="/save" method="post">
    Enter Expression: <input type="text" name="expr" value=""><br>
You may enter values for variables in the form 'x=3,y=6.2' <br>
    Enter Variables: <input type="text" name="vars" value=""><br>
    <input type="submit" value="Submit">
   </form>

  </body>
 </html>
`)
}

// evaluate expression and write to w
func evaluate(w http.ResponseWriter, req *http.Request) {
	vars := req.PostFormValue("vars")
	env := buildEnv(vars)

	expr := req.PostFormValue("expr")
	result, err := parseAndEvalWithEnv(expr, env)
	if err != nil {
		w.WriteHeader(http.StatusBadRequest)
		fmt.Fprintf(w, "expression parse error: %s\n", expr)
		return
	}

	fmt.Fprintf(w, `
<!DOCTYPE html>
 <html>
  <body>

<p><strong>Calculator</strong></p>

Expression: %v <br>
Variables: %v <br>
Result: %v 

  </body>
 </html>
`, expr, vars, result)
}

// parseAndEvalWithEnv: parse expression and evaluate in env
func parseAndEvalWithEnv(input string, env eval.Env) (float64, error) {
	expr, err := eval.Parse(input)
	if err != nil {
		return 0, err
	}
	result := expr.Eval(env)
	return result, nil
}

// buildEnv: parse vars string and build environment
// vars form 'x=3,y=6.2'
func buildEnv(vars string) eval.Env {
	env := make(eval.Env)
	sub := strings.Split(vars, ",")
	for _, e := range sub {
		pairs := strings.Split(e, "=")
		if len(pairs) != 2 {
			continue
		}
		name := eval.Var(pairs[0])
		value, err := strconv.ParseFloat(pairs[1], 64)
		if err != nil {
			continue
		}
		env[name] = value

	}
	return env
}

func debug(env eval.Env) {
	for k, v := range env {
		fmt.Fprintf(os.Stderr, "k: %v v: %v\n", k, v)
	}
}
