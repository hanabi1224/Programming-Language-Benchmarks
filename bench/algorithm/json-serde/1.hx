import haxe.crypto.Md5;
import sys.io.File;

class App {
	static function main() {
		final args = Sys.args();
		final fileName = args.length > 0 ? args[0] : 'sample';
		final n = args.length > 1 ? Std.parseInt(args[1]) : 10;
		final jsonStr = File.getContent('${fileName}.json');
		final data = haxe.Json.parse(jsonStr);
		printHash(haxe.Json.stringify(data));
		final array = new List<Dynamic>();
		for (i in 0...n) {
			array.add(haxe.Json.parse(jsonStr));
		}
		printHash(haxe.Json.stringify(array));
	}

	static function printHash(s:String) {
		// Sys.println(s);
		final hash = Md5.encode(s);
		Sys.println(hash);
	}
}
