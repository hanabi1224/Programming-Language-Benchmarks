import "os" for Process

var evala = Fn.new { |i, j|
    return ((i + j) * (i + j + 1) / 2 + i + 1)
}

var times = Fn.new { |v, u|
    for (i in 0...v.count) {
		var a = 0
		for (j in 0...u.count) {
			a = a + u[j] / evala.call(i, j)
		}
		v[i] = a
	}
}

var times_trans = Fn.new { |v, u|
    for (i in 0...v.count) {
		var a = 0
		for (j in 0...u.count) {
			a = a + u[j] / evala.call(j, i)
		}
		v[i] = a
	}
}

var a_times_transp = Fn.new { |v, u|
    var x = List.filled(u.count, 0)
    times.call(x, u)
    times_trans.call(v, x)
}

var numToString = Fn.new() { |n|
    var d = n.round
    var f = (n.fraction * 1e9).round.abs
    var fStr = f.toString
    var nPadding = 9 - fStr.count
    (0...nPadding).each {
        fStr = "0%(fStr)"
    }
    return "%(d).%(fStr)"
} 

var n = 10
if (Process.allArguments.count > 2) {
    n = Num.fromString(Process.allArguments[2])
}

var u = List.filled(n, 1)
var v = List.filled(n, 1)
(0...10).each {
    a_times_transp.call(v, u)
    a_times_transp.call(u, v)
}

var vbv = 0
var vv = 0
for (i in 0...n) {
    vbv = vbv + u[i] * v[i]
    vv = vv + v[i] * v[i]
}
var ans = (vbv / vv).sqrt
System.print(numToString.call(ans))
