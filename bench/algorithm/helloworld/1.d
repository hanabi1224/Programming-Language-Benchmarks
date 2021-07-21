@safe:
import std;

void main(string[] args) {
    auto n = args.length > 1 ? args[1] : "";
    writeln(format("Hello world %s!", n));
}
