import Dispatch
import Foundation

let n = UInt32(10)

if CommandLine.argc > 1 {
    n = UInt32(CommandLine.arguments[1]) ?? UInt32(10)
}

let minDepth = UInt32(4)
let maxDepth = (n > minDepth + 2) ? n : minDepth + 2
let depth = maxDepth + 1
let check = Tree(depth: depth).check
print("stretch tree of depth \(maxDepth+1)\t check: \(check)")

var messages = [(UInt32, String)]()
let longLivingTree = Tree(depth: maxDepth)
await withTaskGroup(of: (UInt32, String).self) { group in
    for d in stride(from: minDepth, to: maxDepth, by: 2) {
        group.addTask {
            let iterations = UInt32(1 << (maxDepth - d + minDepth))
            var chk: UInt32 = 0
            for _ in 1...iterations {
                chk += Tree(depth: d).check
            }
            return (d, "\(iterations)\t trees of depth \(d)\t check: \(chk)")
        }
    }

    for await msg in group {
        messages.append(msg)
    }

}
for msg in messages.sorted(by: { $0.0 < $1.0 }) {
    print(msg.1)
}
print("long lived tree of depth \(maxDepth)\t check: \(longLivingTree.check)")


indirect enum Tree {
    case Empty
    case Node(left: Tree, right: Tree)

    init(depth: UInt32) {
        if depth > 0 {
            self = .Node(left: Tree(depth: depth - 1), right: Tree(depth: depth - 1))
        } else {
            self = .Node(left: .Empty, right: .Empty)
        }
    }

    var check: UInt32 {
        switch self {
        case .Node(let left, let right):
            switch (left, right) {
            case (.Empty, .Empty):
                return 1
            default:
                return 1 + left.check + right.check
            }
        case .Empty:
            return 1
        }
    }
}
