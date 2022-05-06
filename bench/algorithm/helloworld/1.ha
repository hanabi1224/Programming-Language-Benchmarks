use fmt;
use os;

export fn main() void = {
	const n = if (len(os::args) > 1) os::args[1] else "";
	fmt::printfln("Hello world {}!", n)!;
};
