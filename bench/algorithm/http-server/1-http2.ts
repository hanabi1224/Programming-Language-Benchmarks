async function readBody(body: ReadableStream<Uint8Array>): Promise<string> {
    const reader = body.getReader();
    const result = await reader.read();
    return new TextDecoder("utf-8").decode(result.value);
}

async function runServerAsync(server: any) {
    for await (const conn of server) {
        const httpConn = Deno.serveHttp(conn);
        for await (const { request, respondWith } of httpConn) {
            const content = request.body ? await readBody(request.body!) : '';
            const obj = JSON.parse(content);
            const body = `${obj.value}`;
            respondWith(
                new Response(body, {
                    status: 200,
                }),
            );
        }
    }
}

async function sendAsync(api: string, value: number): Promise<number> {
    while (true) {
        try {
            const resp = await fetch(api, {
                method: 'POST',
                body: JSON.stringify({ "value": value }),
                headers: {
                    'content-type': 'application/json',
                },
            });
            const text = await resp.text();
            return +text;
        } catch { }
    }
}

async function main() {
    const n = +Deno.args[0] || 10;
    const hostname = "localhost";
    var port = 20000 + Math.floor(Math.random() * 20000);
    const server = Deno.listenTls({
        hostname: hostname,
        port: port,
        cert: Deno.readTextFileSync("testcert.pem"),
        key: Deno.readTextFileSync("testkey.pem"),
        alpnProtocols: ["h2"],
    });
    runServerAsync(server);
    const api = `https://${hostname}:${port}/`;
    var sum = 0;
    const tasks: Promise<number>[] = [];
    for (var i = 1; i <= n; i++) {
        tasks.push(sendAsync(api, i));
    }
    for (var i = 0; i < n; i++) {
        sum += await tasks[i];
    }
    console.log(sum.toString());
    Deno.exit(0);
    // await server.close();
}

main();
