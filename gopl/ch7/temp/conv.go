package temp

// CToF converts a Celsius temperature to Fahrenheit
func CToF(c Celsius) Fahrenheit {
	return Fahrenheit(c*9/5 + 32)
}

// FToC converts a Fahrenheit to Celsius
func FToC(f Fahrenheit) Celsius {
	return Celsius((f - 32) * 5 / 9)
}

// KToC converts a Kelvin to a Celsius
func KToC(k Kelvin) Celsius {
	return Celsius(ZeroK + k)
}

// CToK converts a Celsius to Kelvin
func CToK(c Celsius) Kelvin {
	return Kelvin(c - AbsoluteZeroC)
}

// FToK converts a Fahrenheit to Kelvin
func FToK(f Fahrenheit) Kelvin {
	return CToK(FToC(f))
}

// KToF converts a Kelvin to Fahrenheit
func KToF(k Kelvin) Fahrenheit {
	return CToF(KToC(k))
}
