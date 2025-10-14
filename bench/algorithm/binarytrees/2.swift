import Foundation

let minDepth = 4
let maxDepth: Int = {
    if CommandLine.argc <= 1 { return 10 }
    return max(minDepth + 2, Int(CommandLine.arguments[1]) ?? 10)
}()

let stretchDepth = maxDepth + 1
print("stretch tree of depth \(stretchDepth)\t check: \(Tree(depth: stretchDepth).check())")

let longLivedTree = Tree(depth: maxDepth)
let nResults = (maxDepth - minDepth) / 2 + 1

for i in 0..<nResults {
    let depth = i * 2 + minDepth
    let n = 1 << (maxDepth - depth + minDepth)

    var chk = 0
    for _ in 0..<n {
        chk += Tree(depth: depth).check()
    }
    print("\(n)\t trees of depth \(depth)\t check: \(chk)")
}

print("long lived tree of depth \(maxDepth)\t check: \(longLivedTree.check())")

struct Tree {
    struct Node {
        var left: Int
        var right: Int
    }

    private static let none: Int = -1
    private var nodes: [Node] = []
    private var root: Int = none

    init(depth: Int) {
        let total = (1 << (depth + 1)) - 1
        guard total > 0 else { return }

        nodes = Array(repeating: Node(left: Self.none, right: Self.none), count: total)
        root = 0

        for i in 0..<total {
            let l = 2 * i + 1
            let r = l + 1
            if l < total { nodes[i].left = l }
            if r < total { nodes[i].right = r }
        }
    }

    func check() -> Int {
        let span = nodes.span
        return _check(index: root, span: span)
    }

    private func _check(index: Int, span: Span<Node>) -> Int {
        if index == Self.none { return 0 }
        let node = span[Int(index)]
        return 1 + _check(index: node.left, span: span)
            + _check(index: node.right, span: span)
    }
}
