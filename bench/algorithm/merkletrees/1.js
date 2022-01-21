class TreeNode {
    hash;
    value;
    left;
    right;

    constructor(value, left, right) {
        this.value = value;
        this.left = left;
        this.right = right;
    }

    static create(depth) {
        if (depth > 0) {
            const d = depth - 1
            return new TreeNode(undefined, TreeNode.create(d), TreeNode.create(d))
        }
        return new TreeNode(1, undefined, undefined)
    }

    check() {
        if (this.hash != undefined) {
            if (this.value != undefined) {
                return true
            } else if (this.left != undefined && this.right != undefined) {
                return this.left.check() && this.right.check();
            }
        }
        return false
    }

    calHash() {
        if (this.hash == undefined) {
            if (this.value != undefined) {
                this.hash = this.value
            } else if (this.left != undefined && this.right != undefined) {
                this.left.calHash();
                this.right.calHash();
                this.hash = this.left.getHash() + this.right.getHash();
            }
        }
    }

    getHash() {
        return this.hash == undefined ? -1 : this.hash;
    }
}

function main() {
    const maxDepth = Math.max(6, +process.argv[2] || 6);
    const stretchDepth = maxDepth + 1;
    const stretchTree = TreeNode.create(stretchDepth)
    stretchTree.calHash()
    console.log(`stretch tree of depth ${stretchDepth}\t root hash: ${stretchTree.getHash()} check: ${stretchTree.check()}`)
    const longLivedTree = TreeNode.create(maxDepth);
    for (let depth = 4; depth <= maxDepth; depth += 2) {
        const iterations = 1 << maxDepth - depth + 4;
        let sum = 0;
        for (var i = 0; i < iterations; i++) {
            const tree = TreeNode.create(depth)
            tree.calHash()
            sum += tree.getHash()
        }
        console.log(`${iterations}\t trees of depth ${depth}\t root hash sum: ${sum}`)
    }
    longLivedTree.calHash();
    console.log(
        `long lived tree of depth ${maxDepth}\t root hash: ${longLivedTree.getHash()} check: ${longLivedTree.check()}`
    );
}

main()
