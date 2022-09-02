const http = require("http");

const host = "localhost";

function requestListener(req, res) {
	var body = "";
	req.on("data", function (chunk) {
		body += chunk;
	});
	req.on("end", function () {
		const payload = JSON.parse(body);
		res.writeHead(200);
		res.end(payload.value.toString());
	});
}

function send(api, value) {
	return new Promise(function (onFulfilled, onRejected) {
		const req = http.request(
			api,
			{
				method: "POST",
				headers: {
					"content-type": "application/json",
				},
			},
			(res) => {
				var content = "";
				res.on("data", (chunk) => {
					content += chunk;
				});
				res.on("end", () => {
					onFulfilled(+content);
				});
			},
		);
		req.on("error", (err) => {
			onRejected(err);
		});
		req.write(JSON.stringify({ value: value }));
		req.end();
	});
}

async function sendAsync(api, value) {
	while (true) {
		try {
			return await send(api, value);
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

function main() {
	const args = process.argv.slice(2);
	const n = +args[0] || 10;
	const port = 20000 + Math.floor(Math.random() * 30000);
	const server = http.createServer(requestListener);
	server.listen(port, host, async () => {
		await calculateSum(port, n);
		server.close();
	});
}

main();
