package main

import (
	"fmt"
	"os"
)

func main() {
	name := ""
	if len(os.Args) > 1 {
		name = os.Args[1]
	}
	fmt.Printf("Hello world %s!", name)
}
