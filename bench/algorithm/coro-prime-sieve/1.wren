import "os" for Process

var generate = Fn.new { 
    return Fiber.new {
        var i = 2
        while(true){
            Fiber.yield(i)
            i = i + 1
        }
    }
}

var filter = Fn.new { |last, prime|
    return Fiber.new {
        while (true) {
            var n = last.call()
            if (n % prime != 0) {
                Fiber.yield (n)
            }
        }
    }
}

var n = 10
if (Process.allArguments.count > 2) {
    n = Num.fromString(Process.allArguments[2])
}

var coro = generate.call()
for(i in 0...n) {
    var prime = coro.call()
    System.print(prime)
    coro = filter.call(coro, prime)
}
