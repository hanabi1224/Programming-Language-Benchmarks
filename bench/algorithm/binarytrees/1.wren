import "os" for Process

class Node {
    construct new(left, right) {
        _left = left
        _right = right
    }

    construct bottomUp(depth) {
        if (depth <= 0) {
            _left = null
            _right = null
        } else {
            _left = Node.bottomUp(depth - 1)
            _right = Node.bottomUp(depth - 1)
        }
    }

    check {
        if (_left == null) {
            return 1
        }
        return 1 + _left.check + _right.check
    }
}

var minDepth = 4

var n = 5
if (Process.allArguments.count > 2) {
    n = Num.fromString(Process.allArguments[2])
}

if (n < minDepth + 2) {
    n = minDepth + 2
}
var stretchDepth = n + 1

var check = Node.bottomUp(stretchDepth).check
System.print("stretch tree of depth %(stretchDepth)\t check: %(check)")

var longLivedTree = Node.bottomUp(n)

var depth = minDepth
while (depth <= n) {
    var iterations = 1 << (n - depth + minDepth)
    var check = 0
    for (i in 0...iterations) {
        check = check + Node.bottomUp(depth).check
    }
    System.print("%(iterations)\t trees of depth %(depth)\t check: %(check)")
    depth = depth + 2
}
System.print("long lived tree of depth %(n)\t check: %(longLivedTree.check)")
