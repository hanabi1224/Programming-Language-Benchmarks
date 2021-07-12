import haxe.crypto.Md5;
import sys.io.File;

class App {
	static function main() {
		final args = Sys.args();
		final fileName = args.length > 0 ? args[0] : 'sample';
		final n = args.length > 1 ? Std.parseInt(args[1]) : 3;
		final jsonStr = File.getContent('${fileName}.json');
		var indent = " ";
		for (i in 0...n) {
			final data = haxe.Json.parse(jsonStr);
			final prettified = haxe.Json.stringify(data, indent);
			Sys.println(prettified);
			indent += " ";
			final hash = Md5.encode(prettified);
			Sys.println(hash);
		}
	}
}
