// uniqueFIFO: unique FIFO queue
//
// queue has the special property that if a value that is already present is enqueued
// it is first removed from the queue
package uniqueFIFO

import "bytes"

type node struct {
	value string
	next  *node
	prev  *node
}

type Queue struct {
	LinkedList
}

type LinkedList struct {
	head   *node
	tail   *node
	length int
}

func (q *Queue) Enqueue(s string) {
	q.delete(s)
	q.AddHead(s)
}

func (q *Queue) delete(s string) {
	np := q.GetNode(s)
	q.Delete(np)
}

func (q *Queue) Dequeue() string {
	s := q.head.value
	q.Delete(q.head)
	return s
}

func (q *Queue) String() string {
	var buf bytes.Buffer
	add := func(s string) {
		buf.WriteRune('"')
		buf.WriteString(s)
		buf.WriteRune('"')
	}
	fn := func(n *node) {
		add(n.value)
	}
	q.ForEach(fn)
	return buf.String()
}

// equalFn: returns a function that can be parsed to lk.Delete
func equalFn(s string) func(n *node) bool {
	fn := func(n *node) bool {
		if n.value == s {
			return true
		}
		return false
	}
	return fn
}

//
// Doubly linked list
//

func (p *LinkedList) Length() int {
	return p.length
}

func (p *LinkedList) AddHead(value string) {
	n := node{value, p.head, p.tail}
	if p.head == nil {
		n.prev = &n
		n.next = &n
		p.head = &n
		p.tail = &n

	} else {
		p.head.prev = &n
		p.tail.next = &n
		p.head = &n
	}
	p.length++
}

func (p *LinkedList) AddTail(value string) {
	n := node{value, p.head, p.tail}
	if p.head == nil {
		n.prev = &n
		n.next = &n
		p.head = &n
		p.tail = &n

	} else {
		p.head.prev = &n
		p.tail.next = &n
		p.tail = &n
	}
	p.length++
}

// ForEach: call func once for each element of list
func (p *LinkedList) ForEach(f func(n *node)) {
	np := p.head
	if np == nil {
		return
	}
	for {
		f(np)

		if np == p.tail {
			break
		}
		np = np.next
	}
}

// Delete n from list
func (p *LinkedList) Delete(n *node) {
	if n == nil {
		return
	}
	n.prev.next = n.next
	n.next.prev = n.prev
	if p.head == n {
		p.head = n.next
	}
	if p.tail == n {
		p.tail = n.prev
	}
	p.length--
}

func (p *LinkedList) GetNode(s string) *node {
	np := p.head
	if np == nil {
		return nil
	}
	for np != p.tail {
		if np.value == s {
			return np
		}
		np = np.next
	}
	return nil
}
