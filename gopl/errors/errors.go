// Error handling techniques see section 5.4

// 1. Propagate the error

resp, err := http.Get(url)
if err != nil {
	return nil, err
}

// For error on call to f(x), the call f(x) is responsible for reporting
// operation of f and the argument value x. The callen is responsible for
// adding further information that it has but call to f(x) does not. (p129)
doc, err := html.Prase(resp.body)
if err != nil {
	return nil, fmt.Errorf("parsing %s as HTML: %v", url, err)
}

// 2. Errors representing transient and unpredictable errors
wait and retry see wait.go

// 3. For fatal errors print error and exit gracefully
// (main function only)
if err := someFunction(); err != nil {
	fmt.Fprintf(os.Stderr, "Error condition occured: %v", err)
	os.Exit(1)
}

// or use log (set prefix and flags for prettier output)
log.SetPrefix("program_name:")
log.SetFlags(0)
if err := someFunction(); err != nil {
	log.Fatalf(os.Stderr, "Error condition occured: %v", err)
}

// 4. Log and continue (use fmt or log as above)

// 5. Ignore error - be sure to comment so
os.RemoveAll(dir) // ignore errors; $TMPDIR is cleaned periodically

// EOF
in := bufio.NewReader(is.Stdin)
for {
	r, _, err := in.ReadRune()
	if err == io.EOF {
		break // finished reading
	}
	if err != nil {
		return fmt.Errorf("read failed: %v", err)
	}
	// use r ...
}
