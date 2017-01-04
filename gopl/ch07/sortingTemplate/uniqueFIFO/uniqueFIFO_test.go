package uniqueFIFO

import "testing"

func TestSimple(t *testing.T) {
	var q Queue
	s := "this"
	q.Enqueue(s)

	if q.head.value != s {
		t.Errorf("Enqueue fail")
	}
}

func TestMore(t *testing.T) {
	var q Queue
	input := []string{"one", "two", "three", "four"}
	want := ""
	for _, s := range input {
		q.Enqueue(s)
		want = "\"" + s + "\"" + want

		if q.head.value != s {
			t.Errorf("Enqueue fail: %s\n", q.String())
		}
		got := q.String()
		if want != got {
			t.Errorf("fail: want: %s got: %s\n", want, got)
		}
	}
}

func TestMulti(t *testing.T) {
	var q Queue
	input := []string{"a", "b", "c", "d"}
	for _, s := range input {
		q.Enqueue(s)
	}
	if q.String() != errorString("a", "b", "c", "d") {
		t.Errorf("re-enqueue fail: %s\n", q.String())
	}

	s := "b"
	q.Enqueue(s)
	if q.String() != errorString("a", "c", "d", "b") {
		t.Errorf("re-enqueue fail: %s\n", q.String())
	}
	s = "d"
	q.Enqueue(s)
	if q.String() != errorString("a", "c", "b", "d") {
		t.Errorf("re-enqueue fail: %s\n", q.String())
	}

}

func TestDequeue(t *testing.T) {
	var q Queue
	input := []string{"one", "two", "three", "four"}
	for _, s := range input {
		q.Enqueue(s)
	}
	for i := len(input) - 1; i >= 0; i-- {
		got := q.Dequeue()
		want := input[i]
		if want != got {
			t.Errorf("Dequeue fail, got: %s want: %s\n", got, want)
		}
	}
}

func errorString(inputs ...string) string {
	var s string
	for _, input := range inputs {
		s = "\"" + input + "\"" + s
	}
	return s
}
