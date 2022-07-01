module main

import os
import strconv
import json
import vweb
import net.http
import rand

fn main() {
	mut n := 10
	if os.args.len == 2 {
		n = strconv.atoi(os.args[1]) or { 10 }
	}
	port := int(rand.u32_in_range(20000, 50000) or { 23333 })
	go vweb.run(&App{}, port)
	url := 'http://localhost:$port/api'
	mut ch := chan int{cap: n}
	for i in 1 .. (n + 1) {
		go send(url, i, ch)
	}
	mut sum := 0
	for _ in 0 .. n {
		sum += <-ch
	}
	println(sum)
}

fn send(url string, v int, ch chan int) {
	for true {
		response := http.post_json(url, json.encode(Payload{ value: v })) or { continue }
		ch <- strconv.atoi(response.body) or { 0 }
		return
	}
}

struct App {
	vweb.Context
}

['/api'; post]
pub fn (mut app App) api() vweb.Result {
	data := json.decode(Payload, app.req.data) or {
		Payload{
			value: 0
		}
	}
	return app.text('$data.value')
}

struct Payload {
	value int [json: 'value']
}
