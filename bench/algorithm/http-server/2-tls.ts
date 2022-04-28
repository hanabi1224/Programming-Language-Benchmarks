import { serveTls } from "https://deno.land/std/http/server.ts";

async function readBody(body: ReadableStream<Uint8Array>): Promise<string> {
    const reader = body.getReader();
    const result = await reader.read();
    return new TextDecoder("utf-8").decode(result.value);
}

async function runServerAsync(port: number) {
    await serveTls(async (req: Request) => {
        const content = req.body ? await readBody(req.body!) : '';
        const obj = JSON.parse(content);
        const body = `${obj.value}`;
        return new Response(body, {
            status: 200,
        })
    }, {
        hostname: 'localhost',
        port: port,
        certFile: 'testcert.pem',
        keyFile: 'testkey.pem',
        // h2 is not yet supported
        // alpnProtocols: ['h2'],
    });
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
    runServerAsync(port);
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
}

main();
