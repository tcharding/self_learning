package main

import (
	"bufio"
	"flag"
	"fmt"
	"log"
	"net"
	"os"
	"strings"
	"text/tabwriter"
)

// Clock servers
//
// US/Eastern 8010
// US/Central 8020
// London 8030

var clockList = flag.String("clocks", "Local:8000", "Name:port[,Name:port...]")

type clock struct {
	name string
	port string
	ch   chan string
}

func main() {
	flag.Parse()
	clocks := parseClockList(*clockList)

	for _, c := range clocks {
		go connectToClock(c)
	}

	printHeader(clocks)

	for {
		var times string
		for _, c := range clocks {
			t := <-c.ch
			t = strings.TrimSpace(t)
			times += t + "  "
		}
		fmt.Printf("\r%s", times)
	}
}

// parseClockList: parse string of form Name:port[,Name:port...]
func parseClockList(s string) []clock {
	var clocks []clock

	for _, c := range strings.Split(s, ",") {
		a := strings.Split(c, ":")
		clocks = append(clocks, clock{a[0], a[1], make(chan string)})
	}
	return clocks
}

func connectToClock(c clock) error {
	host := "localhost:" + c.port
	conn, err := net.Dial("tcp", host)
	if err != nil {
		log.Fatal(err)
	}
	defer conn.Close()

	reader := bufio.NewReader(conn)
	for {
		text, err := reader.ReadString('\n')
		if err != nil {
			return err
		}
		c.ch <- text
	}
	return nil
}

func printHeader(clocks []clock) {
	tw := new(tabwriter.Writer).Init(os.Stdout, 0, 8, 2, ' ', 0)
	for _, c := range clocks {
		fmt.Fprintf(tw, "%v\t", c.name)
	}
	fmt.Fprintf(tw, "\n")

	for _ = range clocks {
		underline := strings.Repeat("-", 8)
		fmt.Fprintf(tw, "%v\t", underline)
	}
	fmt.Fprintf(tw, "\n")
	tw.Flush() // calculate column widths and print table
}
