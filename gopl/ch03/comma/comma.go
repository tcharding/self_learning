// Comma adds commas to number strings
// -1234.5678 -> -1,234.567,8
package comma

import "strings"

// comma: adds commas to string
func Comma(s string) string {
	sign, int, dec := split(s)

	return sign + commaR(int) + "." + commaDecimalR(dec)
}

// split number string into sign, integer part, decimal part
func split(s string) (sign, int, dec string) {
	sign = signPart(s)
	int = intPart(s)
	dec = decPart(s)

	return sign, int, dec
}

// commR: add commas to int, recursive version
func commaR(s string) string {
	if len(s) <= 3 {
		return s
	}
	subl := len(s) - 3
	return commaR(s[:subl]) + "," + s[subl:]
}

// commaDecimalR: add commas to decimal number string, recursive version
func commaDecimalR(s string) string {
	if len(s) <= 3 {
		return s
	}
	return commaR(s[:3]) + "," + commaDecimalR(s[3:])
}

// signPart: sign part of number string
func signPart(s string) string {
	var sign string
	if s[0] == '-' {
		sign = "-"
	} else if s[0] == '+' {
		sign = "+"
	}
	return sign
}

// intPart: integer part of number string
func intPart(s string) string {
	s = stripSign(s)
	s = addInitialZero(s)
	i := strings.Index(s, ".")

	if i != -1 {
		return s[0:i]
	}
	return s
}

// decPart:decimal part of string
func decPart(s string) string {
	s = stripSign(s)
	s = addInitialZero(s)

	i := strings.Index(s, ".")
	if i != -1 {
		return s[i+1:]
	}
	return "0"

}

// stripSign: remove sign if present
func stripSign(s string) string {
	if s[0] == '-' || s[0] == '+' {
		s = s[1:]
	}
	return s
}

// addInitialZero: add zero if number starts with decimal point
func addInitialZero(s string) string {
	if s[0] == '.' {
		s = "0" + s
	}
	return s
}

// // commaI: add commas to number string, iterative version
// func commaI(s string) string {
// 	var b bytes.Buffer

// 	var c float64 = 0.0
// 	for i := len(s) - 1; i >= 0; i-- {
// 		b.WriteByte(s[i])
// 		c++

// 		if math.Mod(c, 3) == 0 && int(c) != len(s) {
// 			b.WriteByte(',')
// 		}

// 	}
// 	return reverse(b.String())
// }

// // reverse string
// func reverse(s string) string {
// 	var b bytes.Buffer

// 	for i := len(s) - 1; i >= 0; i-- {
// 		b.WriteByte(s[i])
// 	}

// 	return b.String()
// }
