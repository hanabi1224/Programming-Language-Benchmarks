import sys
import random
import json
import concurrent.futures
import urllib.request
import uvicorn
from http.server import BaseHTTPRequestHandler
from threading import Thread
from io import BytesIO
from datetime import datetime


async def read_body(receive):
    """
    Read and return the entire body from an incoming ASGI message.
    """
    body = b''
    more_body = True
    while more_body:
        message = await receive()
        body += message.get('body', b'')
        more_body = message.get('more_body', False)
    return body


async def app(scope, receive, send):
    assert scope['type'] == 'http'
    body = await read_body(receive)
    obj = json.loads(body)
    await send({
        'type': 'http.response.start',
        'status': 200,
        'headers': [],
    })
    await send({
        'type': 'http.response.body',
        'body': str(obj['value']).encode('utf-8'),
    })


class SimpleHTTPRequestHandler(BaseHTTPRequestHandler):
    def do_POST(self):
        body = self.rfile.read1(-1)
        obj = json.loads(body)
        self.send_response(200)
        self.end_headers()
        response = BytesIO()
        response.write(str(obj['value']).encode('utf-8'))
        self.wfile.write(response.getvalue())


def run_server(port: int):
    uvicorn.run("app:app", host="localhost", port=port, log_level="critical")


def send(api: str, value: int):
    while True:
        try:
            with urllib.request.urlopen(api, json.dumps({'value': value}).encode('utf-8')) as fp:
                return int(fp.read().decode('utf-8'))
        except:
            pass


def main():
    n = 10 if len(sys.argv) < 2 else int(sys.argv[1])
    port = 20000 + int(30000*random.random())
    t = Thread(target=run_server, args=(port,), daemon=True)
    t.start()
    api = f'http://localhost:{port}'
    with concurrent.futures.ThreadPoolExecutor() as executor:
        futures = [executor.submit(send, api, i) for i in range(1, n + 1)]
        sum = 0
        for future in futures:
            sum += future.result()
        print(sum)


if __name__ == '__main__':
    with open("ready", "w") as f:
        f.write(str(round(datetime.utcnow().timestamp() * 1000)))

    main()
    # uvicorn.run("app:app", host="localhost", port=5000, log_level="info")
