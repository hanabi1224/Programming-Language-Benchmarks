@safe:
import std;
import std.digest.md;

void printHash(ref JSONValue json)
{
    auto md5 = new MD5Digest();
    auto hash = md5.digest(json.toString);
    // No way to keep key order?
    writeln(toHexString!(LetterCase.lower)(hash));
}

void main(string[] args)
{
    auto fileName = args.length > 1 ? args[1] : "sample";
    auto n = args.length > 2 ? args[2].to!int() : 10;
    auto jsonText = std.file.readText(fileName ~ ".json");
    JSONValue json = parseJSON(jsonText);
    printHash(json);
}
