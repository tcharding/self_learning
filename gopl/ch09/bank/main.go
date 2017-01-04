// Package bank provides concurrency-safe bank with one account.
package main

var deposits = make(chan int)         // send amount to deposit
var balances = make(chan int)         // receive balance
var withdrawals = make(chan withdraw) // send amount to withdraw

type withdraw struct {
	amount int
	result chan bool
}

func Deposit(amount int) {
	deposits <- amount
}

func Balance() int {
	return <-balances
}

func Withdraw(amount int) bool {
	result := make(chan bool)
	withdrawals <- withdraw{amount, result}
	return <-result
}

func teller() {
	var balance int // balance is confine to teller goroutine
	for {
		select {
		case amount := <-deposits:
			balance += amount
		case balances <- balance:
		case w := <-withdrawals:
			if balance >= w.amount {
				balance -= w.amount
				w.result <- true
			} else {
				w.result <- false
			}
		}
	}
}

func init() {
	go teller() // start the monitor goroutine
}
