package eval

import (
	"fmt"
	"math"
	"testing"
)

func TestString(t *testing.T) {
	tests := []struct {
		expr string
		env  Env
		want string
	}{
		{"sqrt(A / pi)", Env{"A": 87616, "pi": math.Pi}, "167"},
		{"pow(x, 3) + pow(y, 3)", Env{"x": 12, "y": 1}, "1729"},
		{"pow(x, 3) + pow(y, 3)", Env{"x": 9, "y": 10}, "1729"},
		{"5 / 9 * (F - 32)", Env{"F": -40}, "-40"},
		{"5 / 9 * (F - 32)", Env{"F": 32}, "0"},
		{"5 / 9 * (F - 32)", Env{"F": 212}, "100"},
		//!-Eval
		// additional tests that don't appear in the book
		{"-1 + -x", Env{"x": 1}, "-2"},
		{"-1 - x", Env{"x": 1}, "-2"},
		//!+Eval
	}
	for _, test := range tests {
		expr, err := Parse(test.expr)
		if err != nil {
			t.Error(err) // parse error
			continue
		}
		got := fmt.Sprintf("%.6g", expr.Eval(test.env))
		if got != test.want {
			t.Errorf("%s.Eval() in %v = %q, want %q\n",
				test.expr, test.env, got, test.want)
		}
		formatted := expr.String()
		reExpr, err := Parse(formatted)
		if err != nil {
			t.Errorf("error parsing formatted expression: %s\n", formatted)
		}
		gotExpr := fmt.Sprintf("%.6g", expr.Eval(test.env))
		gotReExpr := fmt.Sprintf("%.6g", reExpr.Eval(test.env))
		if gotExpr != gotReExpr {
			t.Errorf("%s : %q \n %s : %q \n",
				test.expr, gotExpr, formatted, gotReExpr)
		}

	}

	// tests := []struct {
	// 	expr string
	// 	env  Env
	// }{
	// 	{"sqrt(A / pi)", Env{"A": 87616, "pi": math.Pi}},
	// 	{"pow(x, 3) + pow(y, 3)", Env{"x": 12, "y": 1}},
	// 	{"pow(x, 3) + pow(y, 3)", Env{"x": 9, "y": 10}},
	// 	{"5 / 9 * (F - 32)", Env{"F": -40}},
	// 	{"5 / 9 * (F - 32)", Env{"F": 32}},
	// 	{"5 / 9 * (F - 32)", Env{"F": 212}},
	// 	{"-1 + -x", Env{"x": 1}},
	// 	{"-1 - x", Env{"x": 1}},
	// }

	// for _, test := range tests {
	// 	expr, err := Parse(test.expr)
	// 	if err != nil {
	// 		t.Error(err) // parse error
	// 		continue
	// 	}
	// 	formatted := expr.String()
	// 	if err != nil {
	// 		t.Errorf("error re parsing: %s \n err: %s\n", formatted, err.Error())
	// 	}

	// 	// if formatted != test.expr {
	// 	// 	errMsg := "formatted expression differs to original"
	// 	// 	errMsg += " \n formatted: %v \n original: %s\n"
	// 	// 	t.Errorf(errMsg, formatted, test.expr)
	// 	// }
	// 	exprVal := expr.Eval(test.env)
	// 	formattedExpr, err := Parse(formatted)
	// 	if err != nil {
	// 		t.Errorf("Error parsing: %s\n", formatted)
	// 	}
	// 	formattedVal := formattedExpr.Eval(test.env)
	// 	if exprVal != formattedVal {
	// 		t.Errorf("error evaluating \n formatted %s \n expr: %s\n",
	// 			formattedExpr.String(), expr.String())
	// 		t.Errorf("evaluated results differ \n formatted: %v \n expr: %vn\n",
	// 			formattedVal, exprVal)
	// 	}
	// }
}
