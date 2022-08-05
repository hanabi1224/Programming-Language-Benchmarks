
let arguments = CommandLine.arguments.dropFirst()

if let name = arguments.first {
    print("Hello world \(name)!")
} else {
    print("Hello world!")
}
