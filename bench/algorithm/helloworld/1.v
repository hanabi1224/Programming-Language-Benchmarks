import os

fn main() {
	name := if os.args.len > 1 { os.args[1] } else { '' }
	println('Hello world ${name}!')
}
