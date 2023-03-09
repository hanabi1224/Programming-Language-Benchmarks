import Foundation

let minDepth = UInt32(4)
let maxDepth = CommandLine.argc > 1 ? max(UInt32(CommandLine.arguments[1]) ?? 0, minDepth + 2) : 10
let stretchDepth = maxDepth + 1
let check = Tree(depth: stretchDepth).check
print("stretch tree of depth \(stretchDepth)\t check: \(check)")

let longLivingTree = Tree(depth: maxDepth)

for halfDepth in (minDepth / 2)..<(maxDepth / 2 + 1) {
    let depth = halfDepth * 2
    let iterations = UInt32(1 << (maxDepth - depth + minDepth))
    var chk: UInt32 = 0
    for _ in 1...iterations {
        chk += Tree(depth: depth).check
    }
    print("\(iterations)\t trees of depth \(depth)\t check: \(chk)")
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
