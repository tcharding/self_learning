// Join joins argument strings using variadic declaration
package join

func Join(sep, first string, rest ...string) string {
	joined := first + sep
	for _, s := range rest {
		joined += s
		joined += sep
	}
	return joined
}
