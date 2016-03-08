package main

import (
	"fmt"
	"os"
	"strconv"

	"github.com/tcharding/go-pkg-optarg"
	"github.com/tcharding/gopl/ch7/temp"
)

var t temp.Celsius

func main() {
	optarg.Header("Option")
	optarg.Add("c", "celcius", "convert Celsius", true)
	optarg.Add("f", "fahrenheit", "convert Fahrenheit", false)
	optarg.Header("Help")
	optarg.Add("h", "help", "show this help list", false)

	usageInfo := fmt.Sprintf("Usage: %s option temperature", os.Args[0])
	optarg.UsageInfo = usageInfo

	var cFlag, fFlag, hFlag bool

	for opt := range optarg.Parse() {
		switch opt.ShortName {
		case "c":
			cFlag = opt.Bool()
		case "f":
			fFlag = opt.Bool()
		case "h":
			hFlag = opt.Bool()
		}
	}

	if len(optarg.Remainder) != 1 || hFlag {
		optarg.Usage()
		os.Exit(1)
	}
	tempArg := optarg.Remainder[0]
	raw, err := strconv.ParseFloat(tempArg, 64)
	if err != nil {
		fmt.Printf("Error parsing temperature %s\n", tempArg)
		optarg.Usage()
		os.Exit(1)
	}

	if fFlag {
		converted := temp.FToC(temp.Fahrenheit(raw))
		fmt.Printf("%.2f C\n", converted)
	} else if cFlag {
		converted := temp.CToF(temp.Celsius(raw))
		fmt.Printf("%.2f F\n", converted)
	} else {
		optarg.Usage()
	}
}
