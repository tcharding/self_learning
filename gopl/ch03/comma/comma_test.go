package comma

import "testing"

func TestSignPart(t *testing.T) {
	var tests = []struct {
		input string
		want  string
	}{
		{"123", ""},
		{"+123", "+"},
		{"-123", "-"},
	}
	for _, test := range tests {
		if got := signPart(test.input); got != test.want {
			t.Errorf("sign(%q) = %v", test.input, got)
		}
	}
}

func TestIntPart(t *testing.T) {
	var tests = []struct {
		input string
		want  string
	}{
		{"123", "123"},
		{"+123", "123"},
		{"-123", "123"},
		{"123.45", "123"},
		{"+123.45", "123"},
		{"123.0", "123"},
		{".12", "0"},
		{"0.12", "0"},
	}
	for _, test := range tests {
		if got := intPart(test.input); got != test.want {
			t.Errorf("intPart(%q) = %v", test.input, got)
		}
	}
}

func TestDecPart(t *testing.T) {
	var tests = []struct {
		input string
		want  string
	}{
		{"123", "0"},
		{"+123", "0"},
		{"-123", "0"},
		{"123.45", "45"},
		{"+123.45", "45"},
		{"123.0", "0"},
		{".12", "12"},
		{"0.12", "12"},
	}
	for _, test := range tests {
		if got := decPart(test.input); got != test.want {
			t.Errorf("decPart(%q) = %v", test.input, got)
		}
	}
}
