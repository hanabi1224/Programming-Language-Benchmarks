import sys
import random
import json
import concurrent.futures
import urllib.request
from http.server import HTTPServer, ThreadingHTTPServer, BaseHTTPRequestHandler
from threading import Thread
from io import BytesIO


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
    httpd = ThreadingHTTPServer(('localhost', port), SimpleHTTPRequestHandler)
    httpd.serve_forever()


def send(api: str, value: int):
    while True:
        try:
            with urllib.request.urlopen(api, json.dumps({'value': value}).encode('utf-8')) as fp:
                return int(fp.read().decode('utf-8'))
        except:
            pass


def main():
    n = 10 if len(sys.argv) < 2 else int(sys.argv[1])
    random.seed(0)
    port = 30000 + int(10000*random.random())
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
    main()
