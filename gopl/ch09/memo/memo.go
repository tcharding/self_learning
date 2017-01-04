// memo provides memoizing of functions
// this version uses a monitor goroutine to guard the cache (map variable)
package memo

// Func is the type of the function to memoize.
type Func struct {
	f      func(key string) (interface{}, error)
	cancel chan string
}

type entry struct {
	res   result
	ready chan struct{} // closed when res is ready
}

type result struct {
	value interface{}
	err   error
}

// A request is a message requesting that the Func be applied to key.
type request struct {
	key      string
	response chan<- result // the client wants a single result
}

type Memo struct {
	requests chan request
}

// New returns a memoization of f. Clients must subsequently call Close.
func New(f Func) *Memo {
	memo := &Memo{requests: make(chan request)}
	go memo.server(f)
	return memo
}

func (memo *Memo) Get(key string) (value interface{}, err error) {
	response := make(chan result)
	memo.requests <- request{key, response}
	res := <-response
	return res.value, res.err
}

func (memo *Memo) Close() {
	close(memo.requests)
}

func (memo *Memo) server(f Func) {
	cache := make(map[string]*entry)
	for {
		select {
		case req := <-memo.requests:
			e := cache[req.key]
			if e == nil {
				// This is the first request for this key.
				e = &entry{ready: make(chan struct{})}
				cache[req.key] = e
				go e.call(f, req.key) // call f(key)
			}
			go e.deliver(req.response)
		case key := <-f.cancel:
			delete(cache, key)
		}
	}
}

func (e *entry) call(f Func, key string) {
	// Evaluate the function.
	e.res.value, e.res.err = f.f(key)
	// Broadcast the ready condition.
	close(e.ready)
}

func (e *entry) deliver(response chan<- result) {
	// Wait for the ready condition.
	<-e.ready
	// Send the result to the client.
	response <- e.res
}
