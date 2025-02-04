import FlyingFox
import Foundation

struct PayloadContent: Codable {
	let value: Int
	init(value: Int){
		self.value = value;
	}
}

class DummyServer {
	let server : HTTPServer

	private func handleRequest(request: HTTPRequest) async -> HTTPResponse{
		do {
			let bodyStr = try await request.bodyData
			let body = try JSONDecoder().decode(PayloadContent.self, from: bodyStr)
			let response = "\(body.value)"
			if let responseData = response.data(using: .utf8) {
			    return HTTPResponse(statusCode: .ok, body: responseData)
			}
		} catch {
			print("An unexpected error occurred: \(error)")
		}
		return HTTPResponse(statusCode: .internalServerError, body: Data("Internal Server Error".utf8))
	}

	init(_ port: UInt16) async {
		server = HTTPServer(port: port)
		await server.appendRoute("POST /api") {request in
			return await self.handleRequest(request: request)
		}
	}

	func start() async throws {
		try await server.start()
	}

	func stop() async throws{
		await server.stop(timeout: 3)
	}
}

func send(url: URL, value: Int) async -> Int {
    let payload = PayloadContent(value: value)
    guard let jsonData = try? JSONEncoder().encode(payload) else { return 0 }

    var request = URLRequest(url: url)
    request.httpMethod = "POST"
    request.setValue("application/json", forHTTPHeaderField: "Content-Type")
    request.httpBody = jsonData
    do {
	    let (data, _) = try await URLSession.shared.data(for: request)
	    if let responseStr = String(data: data, encoding: .utf8), let intValue = Int(responseStr) {
	        return intValue
	    }
    } catch {
    	print(error)
    }
    return 0
}

func main()async throws{
	print("Hello world!")
	// Command line arguments
	let args = CommandLine.arguments
	var n = 10

	if args.count == 2, let inputNumber = Int(args[1]) {
	    n = inputNumber
	}
	print(n)

	// Generate a random port number in the range 20000 to 50000
	let port = UInt16.random(in: 20000...50000)

	let task = Task {
		let server = await DummyServer(port)
		try await server.start()
	}
	let urlString = "http://localhost:\(port)/api"

	guard let url = URL(string: urlString) else {
	    print("Invalid URL")
	    exit(1)
	}

	var sum = 0

    await withTaskGroup(of: Int.self) { group in
        for i in 1...n {
            group.addTask {
                await send(url: url, value: i)
            }
        }

        for await result in group {
            sum += result
        }
    }

    print(sum)
    task.cancel()

}

try await main()
