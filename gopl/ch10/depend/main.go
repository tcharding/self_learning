// Command depend lists all installed packages that are dependant on input package[s]
// accepts one or more package names
package main

import (
	"bufio"
	"bytes"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"log"
	"os"
	"os/exec"
	"strings"
)

var usage = `package [package ...]

List all installed packages that are dependant on input package[s]
`

func main() {
	flag.Usage = func() {
		fmt.Printf("Usage: %s %s\n", os.Args[0], usage)
	}
	flag.Parse()
	for _, pkg := range flag.Args() {
		// deps, err := getDeps(pkg)
		// if err != nil {
		// 	log.Fatal(err)
		// }
		// fmt.Fprintf(os.Stderr, "%s\n", deps)

		pkgs, err := packagesThatDependOn(pkg)
		if err != nil {
			log.Fatal(err)
		}
		for _, s := range pkgs {
			fmt.Println(s)
		}
	}
}

func packagesThatDependOn(pkg string) ([]string, error) {
	var deps []string
	allPkgs := installedPkgs()
	for _, installed := range allPkgs {
		if dependsOn(installed, pkg) {
			deps = append(deps, installed)
		}
	}
	return deps, nil
}

var allPkgs []string = nil

func installedPkgs() []string {
	if allPkgs == nil {
		cmd := "go"
		args := []string{"list", "all"}
		out, _ := exec.Command(cmd, args...).Output() // NOTE: ignore errors
		var err error
		allPkgs, err = bytesToStrings(out)
		if err != nil {
			log.Fatal(err)
		}
	}
	return allPkgs
}

// dependsOn true if this depends on that
func dependsOn(this, that string) bool {
	deps, err := getDeps(this)
	//	fmt.Fprintf(os.Stderr, "%s.Desp %v\n", this, deps)
	if err != nil {
		return false
	}
	for _, dep := range deps {
		if dep == that {
			return true
		}
	}
	return false
}

func bytesToStrings(buf []byte) ([]string, error) {
	var out []string
	r := bufio.NewReader(bytes.NewBuffer(buf))
	for {
		line, err := r.ReadString('\n')
		if err != nil && err != io.EOF {
			return nil, err
		}
		if err != nil && err == io.EOF {
			break
		}
		line = strings.TrimSpace(line)
		out = append(out, line)
	}
	return out, nil
}

// getDeps: returns list of packages that pkg depends on
func getDeps(pkg string) ([]string, error) {
	data, err := getPkgMeta(pkg)
	if err != nil {
		return nil, err
	}
	var meta struct{ Deps []string }
	if err := json.Unmarshal(data, &meta); err != nil {
		return nil, err
	}
	return meta.Deps, nil
}

// getPkgMeta returns JSON slice of pkg meta data
func getPkgMeta(pkg string) ([]byte, error) {
	cmd := "go"
	args := []string{"list", "-json", pkg}
	out, err := exec.Command(cmd, args...).Output()
	if err != nil {
		return nil, err
	}
	return out, nil
}
