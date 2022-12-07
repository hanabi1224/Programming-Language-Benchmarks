import std;
import std.outbuffer: OutBuffer;

static immutable s1 = [
    tuple(ctRegex!("agggtaaa|tttaccct"), "agggtaaa|tttaccct"),
    tuple(ctRegex!("[cgt]gggtaaa|tttaccc[acg]"),"[cgt]gggtaaa|tttaccc[acg]"),
    tuple(ctRegex!("a[act]ggtaaa|tttacc[agt]t"),"a[act]ggtaaa|tttacc[agt]t"),
    tuple(ctRegex!("ag[act]gtaaa|tttac[agt]ct"),"ag[act]gtaaa|tttac[agt]ct"),
    tuple(ctRegex!("agg[act]taaa|ttta[agt]cct"),"agg[act]taaa|ttta[agt]cct"),
    tuple(ctRegex!("aggg[acg]aaa|ttt[cgt]ccct"),"aggg[acg]aaa|ttt[cgt]ccct"),
    tuple(ctRegex!("agggt[cgt]aa|tt[acg]accct"),"agggt[cgt]aa|tt[acg]accct"),
    tuple(ctRegex!("agggta[cgt]a|t[acg]taccct"),"agggta[cgt]a|t[acg]taccct"),
    tuple(ctRegex!("agggtaa[cgt]|[acg]ttaccct"),"agggtaa[cgt]|[acg]ttaccct")
];

static immutable s2 = [
    ctRegex!("tHa[Nt]"),
    ctRegex!("aND|caN|Ha[DS]|WaS"),
    ctRegex!("a[NSt]|BY"), 
    ctRegex!("<[^>]*>"),
    ctRegex!("\\|[^|][^|]*\\|"),
];

static immutable s3 = ["<4>","<3>","<2>","|","-"];

void f1(string content) {
    foreach(re; s1) {
        auto m = matchAll(content, re[0]);
        writeln(re[1], " ", m.array.length);
    }
}

long f2(string content) {
    auto sink = appender!(char [])();
    sink.reserve(content.length);
    string current;
    bool firstRun = true;
    foreach(i; 0..s3.length) {
        if (firstRun) {
            replaceAllInto(sink, content, s2[i], s3[i]);
            firstRun = false;
        }
        else
            replaceAllInto(sink, current, s2[i], s3[i]);
        current = to!string(sink.data);
        if (sink.data.length)
            sink.clear();
    }
    return current.length;
}

void main(string[] args) {
    immutable string fileName = args.length > 1 ? args[1] : "25000_in";
    auto content = readText(fileName);
    auto ilen = content.length;
    auto r = ctRegex!(">.*\n|\n");
    content = replaceAll(content, r, "");
    auto clen = content.length;
    auto task2 = task!f2(content);
    task2.executeInNewThread();
    f1(content);
    auto lengthResult = task2.yieldForce;
    writefln("\n%d\n%d\n%d",ilen,clen,lengthResult);
}
