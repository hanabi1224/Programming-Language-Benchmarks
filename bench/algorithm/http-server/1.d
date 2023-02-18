import std;
import arsd.cgi;

static void startServer(RequestServer s) {
    s.serve!(postFunc, Cgi, defaultMaxContentLength);
}

void postFunc(Cgi cgi) {
    cgi.setResponseContentType("text/plain");
    cgi.write(cgi.post["value"]);

}

int reqSend(Tuple!(string, int) val) {
    while (true) {
        auto content = std.net.curl.post(val[0], ["value" : val[1].to!string]);
        return content.to!int;
    }
}

int main(string[] args)
{
    int n = args.length > 1 ? args[1].to!int : 10;
    auto rnd = Random(unpredictableSeed);
    auto port = uniform(30000, 40000, rnd);
    string api = "127.0.0.1:"~to!string(port)~"/";
    RequestServer server = RequestServer("127.0.0.1", cast(ushort) port);
    server.useFork = false;
    auto tid = spawn(&startServer, server);
    auto init = iota(1,n + 1,1).map!(a => tuple(api, a));
    auto res = taskPool.amap!reqSend(init, n);
    writeln(res.sum);
    server.stop();
    return 0;
}
