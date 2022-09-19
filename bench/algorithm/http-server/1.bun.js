const host = "localhost";

async function send(api, value) {
	let r = await fetch(api, {
		method: "POST",
		body: JSON.stringify({ value: value }),
	});
	let text = await r.text();
	// console.log(`response: ${text}`);
	return parseInt(text);
}

async function sendAsync(api, value) {
	while (true) {
		try {
			let v = await send(api, value);
			return v;
		} catch (e) {}
	}
}

async function calculateSum(port, n) {
	const api = `http://${host}:${port}/`;
	var sum = +0;
	var tasks = [];
	for (var i = 1; i <= n; i++) {
		tasks.push(sendAsync(api, i));
	}
	for (var i = 0; i < n; i++) {
		sum += await tasks[i];
	}
	console.log(sum);
}

function runServer(port) {
	return Bun.serve({
		async fetch(req) {
			if (req.method === "POST") {
				const payload = await req.json();
				// console.log(`request: ${payload.value}`);
				return new Response(payload.value.toString());
			} else {
				return new Response(`${req.method} not supported`);
			}
		},
		error(err) {
			return new Response("uh oh! :(\n" + err.toString(), { status: 500 });
		},
		development: false,
		port: port,
	});
}

async function main() {
	const args = process.argv.slice(2);
	const n = +args[0] || 10;
	const port = 20000 + Math.floor(Math.random() * 30000);
	// const server = runServer(port);
	runServer(port);
	await calculateSum(port, n);
	process.exit(0);
	// server.stop();
}

main();
