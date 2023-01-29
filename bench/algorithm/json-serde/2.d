import asdf.asdf;
import std;

struct Properties {
    string name;
}

static struct Numeric {
    import mir.algebraic: Variant;
    import asdf: SerdeException;

    static union types {
        double floating;
        int integer;
    }
    alias Union = Variant!(
        types
    );

    Union data;

    alias data this;

    static foreach (T; Union.AllowedTypes)
        this(T v) @safe pure nothrow @nogc { data = v; }

    void serialize(S)(ref S serializer) const
    {
        import asdf: serializeValue;
        import mir.algebraic: visit;

        data.visit!(
            (int v) => serializer.serializeValue(v),
            (double v) => serializer.serializeValue(v),
        );
    }

    SerdeException deserializeFromAsdf(Asdf asdfData)
    {
        import asdf : deserialize, deserializeValue;

        double tmp;
        if (auto exc = deserializeValue(asdfData, tmp))
            return exc;
        if (cast(int) tmp == tmp)
            data = cast(int) tmp;
        else
            data = tmp;
        return null;
    }
}

struct Geometry {
    alias Coord = Numeric[2][];

    string type;
    Coord[] coordinates;
}

struct Feature {
    string type;
    Properties properties;
    Geometry geometry;
}

struct GeoData {
    string type;
    Feature[] features;
}

void printHash(string data)
{
    import std.digest.md;

    auto md5 = new MD5Digest();
    auto hash = md5.digest(data);
    writeln(toHexString!(LetterCase.lower)(hash));
}

void main(string[] args)
{
    import asdf.serialization: deserialize, serializeToJson;
    
    auto fileName = args.length > 1 ? args[1] : "sample";
    auto n = args.length > 2 ? args[2].to!int() : 10;
    auto jsonText = readText(fileName ~ ".json");
    auto json = deserialize!GeoData(jsonText);
    printHash(serializeToJson(json));

    auto array = Array!GeoData(json);
    array.reserve(n);
    foreach(_; 1..n)
       array.insertBack(deserialize!GeoData(jsonText));
    printHash(serializeToJson(array[]));
}
