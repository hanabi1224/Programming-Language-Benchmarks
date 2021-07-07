module main

import os
import strconv

fn main() {
	mut n := 100
	if os.args.len > 1 {
		n = strconv.atoi(os.args[1]) or { n }
	}

	mut ch := chan int{cap:1}
	go generate(ch)
	for _ in 0 .. n {
		prime := <-ch
		println(prime)
		ch_next := chan int{cap:1}
		go filter(ch, ch_next, prime)
		ch = ch_next
	}
}

fn generate(ch chan int) {
	mut i := 2
	for {
		ch <- i++
	}
}

fn filter(chin chan int, chout chan int, prime int) {
	for {
		i := <-chin
		if i % prime != 0 {
			chout <- i
		}
	}
}
