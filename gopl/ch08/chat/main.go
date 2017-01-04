// chat server
package main

import (
	"bufio"
	"fmt"
	"log"
	"net"
	"strings"
)

func main() {
	listener, err := net.Listen("tcp", "localhost:8000")
	if err != nil {
		log.Fatal(err)
	}

	go broadcaster()
	for {
		conn, err := listener.Accept()
		if err != nil {
			log.Println(err)
			continue
		}
		go handleConn(conn)
	}
}

type client struct {
	name   string
	active bool
	ch     chan<- string // an outgoing message channel
}

type clientsMap map[string]*client

// idle connection cancellation has bugs!
// const period = 5 * time.Minute // active if message sent within period
const (
	server = "SERVER"
)

var (
	entering = make(chan client)
	leaving  = make(chan client)
	messages = make(chan string) // all incoming client messages
)

func broadcaster() {
	clients := make(clientsMap) // all connected clients
	//	tick := time.Tick(period)
	for {
		select {
		// case <-tick:
		// 	disconnectIdle(clients)
		// 	clearActive(clients)
		case msg := <-messages:
			//			activate(clients, msg)
			// Broadcast incoming message to all
			// clients' outgoing message channels
			log.Println(msg)
			for _, cli := range clients {
				select {
				case cli.ch <- msg:
					// channel write did not block
				default:
					log.Printf("%s channel not accepting writes", cli.name)
				}
			}
		case cli := <-entering:
			_, ok := clients[cli.name]
			if ok {
				log.Printf("rejecting client: %s\n", cli.name)
				rejectClient(cli)
				log.Printf("rejected")
			} else {
				log.Printf("accepting client: %s\n", cli.name)
				acceptClient(clients, cli)
				log.Printf("accepted")
			}
		case cli := <-leaving:
			removeClient(clients, cli.name)
		}
	}
}

func handleConn(conn net.Conn) {
	defer conn.Close()
	input := bufio.NewScanner(conn)

	fmt.Fprintf(conn, "Enter user name: ")
	input.Scan() // read user name
	who := input.Text()
	fmt.Fprintf(conn, "Attempting to connect as: %s...\n", who)

	ch := make(chan string) // outgoing client messages
	cli := client{name: who, ch: ch, active: false}
	entering <- cli
	response := <-ch // read response from server
	m, ok := <-ch
	log.Printf("m: %s", m)

	if !ok { // channel closed
		fmt.Fprintf(conn, response)
		return
	}
	fmt.Fprintf(conn, response)
	send(messages, "INFO", fmt.Sprintf("%s has arrived", who))
	go clientWriter(conn, ch)

	for input.Scan() {
		msg := input.Text()
		strings.TrimSpace(msg)
		postMessage(who, msg)
	}
	// NOTE: ignoring potential errors from input.Err()
	cli.active = false
	leaving <- cli
	postMessage(who, "has left")
}

func rejectClient(cli client) {
	msg := fmt.Sprintf("user name already exists (%s), refusing connection", cli.name)
	send(cli.ch, server, msg)
	close(cli.ch)
}

func acceptClient(clients clientsMap, cli client) {
	cli.ch <- fmt.Sprintf("You are %s\n", cli.name)
	send(cli.ch, server, "staus OK")
	cli.active = true
	clients[cli.name] = &cli
	//	announcePresent(clients, cli)
}

// disconnectIdle: disconnect all idle clients
func disconnectIdle(clients clientsMap) {
	for name, cli := range clients {
		if !cli.active {
			log.Printf("closing idle connection: %s\n", name)
			postMessage(name, "[SERVER] disconnecting idle user")
			cli.ch <- "idle connection closed"
			//			removeClient(clients, name)
		}
	}
}

// clearActive: set each client in clients to inactive
func clearActive(clients clientsMap) {
	for name, cli := range clients {
		cli.active = false
		log.Printf("setting %s to idle (updated status: %v)\n", name, cli.active)
	}
}

func activate(clients clientsMap, msg string) {
	activateName, ok := nameFromMsg(msg)
	if !ok {
		log.Println("ill formed message, could not get sender name: %s\n", msg)
		return
	}
	for name, cli := range clients {
		if name == activateName {
			cli.active = true
			break
		}
	}
}

// announce to client other clients present
func announcePresent(clients clientsMap, to client) {
	for name := range clients {
		if name != to.name {
			to.ch <- name + " is here"
		}
	}
}

func removeClient(clients clientsMap, name string) {
	close(clients[name].ch)
	delete(clients, name)
}

func clientWriter(conn net.Conn, ch <-chan string) {
	for msg := range ch {
		fmt.Fprintln(conn, msg) // NOTE: ignoring network errors
	}
}

// send msg to channel in standard form
func send(to chan<- string, from, msg string) {
	s := fmt.Sprintf("[%s] %s", from, msg)
	to <- s
}

// postMessage: post msg to global channel messages
func postMessage(from, msg string) {
	send(messages, from, msg)
}

// nameFromMsg: get name from msg formed with postMessage()
func nameFromMsg(msg string) (string, bool) {
	start := strings.Index(msg, "[")
	end := strings.Index(msg, "]")
	if start == -1 ||
		end == -1 {
		return "", false
	}
	return msg[start+1 : end], true
}
