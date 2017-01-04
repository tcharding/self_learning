package main

import "testing"

func Test(t *testing.T) {
	if bal := Balance(); bal != 0 {
		t.Errorf("initial value not zero: %d\n", bal)
	}
	Deposit(100)
	if bal := Balance(); bal != 100 {
		t.Errorf("initial value not zero\n")
	}
	if got := Withdraw(1000); got != false {
		t.Errorf("overdraw successful\n")
	}
	if got := Withdraw(50); got != true {
		t.Errorf("withdrawal fail\n")
	}
	if bal := Balance(); bal != 50 {
		t.Errorf("balance error after withdrawal\n")
	}
}
