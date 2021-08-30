import "os" for Process

var n = ""
if (Process.allArguments.count > 2) {
    n = Process.allArguments[2]
}

System.print("Hello world %(n)!")
