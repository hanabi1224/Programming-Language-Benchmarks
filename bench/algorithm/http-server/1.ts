async function readBody(body: ReadableStream<Uint8Array>): Promise<string> {
    const reader = body.getReader();
    const result = await reader.read();
    return new TextDecoder("utf-8").decode(result.value);
}

async function runServerAsync(server: any) {
    for await (const conn of server) {
        // In order to not be blocking, we need to handle each connection individually
        // in its own async function.
        (async () => {
            // This "upgrades" a network connection into an HTTP connection.
            const httpConn = Deno.serveHttp(conn);
            // Each request sent over the HTTP connection will be yielded as an async
            // iterator from the HTTP connection.
            for await (const requestEvent of httpConn) {
                // The native HTTP server uses the web standard `Request` and `Response`
                // objects.
                const content = requestEvent.request.body ? await readBody(requestEvent.request.body!) : '';
                const obj = JSON.parse(content);
                const body = `${obj.value}`;
                // The requestEvent's `.respondWith()` method is how we send the response
                // back to the client.
                requestEvent.respondWith(
                    new Response(body, {
                        status: 200,
                    }),
                );
            }
        })();
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
    var port = 30000 + Math.floor(Math.random() * 10000);
    const server = Deno.listen({ hostname: hostname, port: port });
    runServerAsync(server);
    const api = `http://${hostname}:${port}/`;
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
