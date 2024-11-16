package main

import "base:runtime"
import "core:fmt"

main :: proc() {
	args := runtime.args__
	name := len(args) > 1 ? args[1] : ""
	fmt.printf("Hello world %s!\n", name)
}
