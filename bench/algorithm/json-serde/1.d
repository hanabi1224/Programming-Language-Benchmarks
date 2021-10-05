import std;
import std.digest.md;

void main(string[] args)
{
    auto fileName = args.length > 1 ? args[1] : "sample";
    auto n = args.length > 2 ? args[2].to!int() : 10;
    auto jsonStr = std.file.readText(fileName ~ ".json");
    auto json = std.json.parseJSON(jsonStr, std.json.JSONOptions.strictParsing);
    printHash(json);
    // for (auto i = 1; i <= n; i++)
    // {
    //     auto json = std.json.parseJSON(jsonStr);
    //     // Indent support?
    //     auto prettified = json.toPrettyString();
    //     // writeln(prettified);
    //     auto md5 = new MD5Digest();
    //     auto hash = md5.digest(prettified);
    //     writeln(toHexString!(LetterCase.lower)(hash));
    // }
}

void printHash(JSONValue json)
{
    auto md5 = new MD5Digest();
    writeln(json.toString());
    auto hash = md5.digest(json.toString());
    writeln(toHexString!(LetterCase.lower)(hash));
}
