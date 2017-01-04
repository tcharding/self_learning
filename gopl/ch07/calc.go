// calculate: use eval library to evaluate expressions on the command line
package main

import (
	"bufio"
	"fmt"
	"log"
	"os"

	"github.com/tcharding/gopl/ch7/eval"
)

func main() {
	for {
		var env eval.Env
		input, err := readUserInput()
		if err != nil {
			log.Fatal(err)
		}

		expr, err := eval.Parse(input)
		if err != nil {
			log.Fatal(err)
		}

		vars := expr.VarList()
		if len(vars) > 0 {
			env = promptForVars(vars)
		}

		fmt.Printf("result: %v\n", expr.Eval(env))
	}
}

func readUserInput() (string, error) {
	reader := bufio.NewReader(os.Stdin)
	fmt.Printf("Enter expression: ")
	input, err := reader.ReadString('\n')
	if err != nil {
		return "", err
	}
	return input, nil
}

func promptForVars(vars []eval.Var) eval.Env {
	env := make(eval.Env)
	fmt.Printf("Variables found in expression, please supply values\n")
	for _, v := range vars {
		var f float64
		fmt.Printf("%v: ", v)
		fmt.Scanf("%f", &f)
		env[v] = f
	}
	return env
}
