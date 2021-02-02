class App {
	static function main() {
		var args = Sys.args();
		var name = args.length > 0 ? args[0] : "";
		Sys.println('Hello world ${name}!');
	}
}
