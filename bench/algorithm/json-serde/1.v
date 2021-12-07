module main

import os
import strconv
import json
import crypto.md5

fn main() {
	file_name := if os.args.len > 1 { os.args[1] } else { 'sample' }
	mut n := 3
	if os.args.len > 2 {
		n = strconv.atoi(os.args[2]) or { 3 }
	}
	json_str := os.read_file('${file_name}.json') ?
	data := json.decode(GeoData, json_str) ?
	println(md5.hexhash(json.encode(data)))
	array := []GeoData{len: n, cap: n, init: json.decode(GeoData, json_str) ?}
	println(md5.hexhash(json.encode(array)))
}

struct GeoData {
	t        string    [json: 'type']
	features []Feature
}

struct Feature {
	t          string     [json: 'type']
	properties Properties
	geometry   Geometry
}

struct Properties {
	name string
}

struct Geometry {
	t           string    [json: 'type']
	coordinates [][][]f64
}
