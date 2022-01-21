/* The Computer Language Benchmarks Game
 * https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
 *
 * contributed by François Pirsch
 * Based on the node.js program from Léo Sarrazin and Andrey Filatkin
 * Remove worker_threads usage by hanabi1224
*/

function main() {
    const maxDepth = Math.max(6, +process.argv[2] || 0);
    const stretchDepth = maxDepth + 1;
    const stretchTree = createTree(stretchDepth)
    console.log(`stretch tree of depth ${stretchDepth}\t check: ${checksum(stretchTree)}`)
    const longLivedTree = createTree(maxDepth);

    for (let depth = 4; depth <= maxDepth; depth += 2) {
        const iterations = 1 << maxDepth - depth + 4;
        let sum = 0;
        for (var i = 0; i < iterations; i++) {
            const tree = createTree(depth)
            sum += checksum(tree)
        }
        console.log(`${iterations}\t trees of depth ${depth}\t check: ${sum}`)
    }

    console.log(
        `long lived tree of depth ${maxDepth}\t check: ${checksum(longLivedTree)}`
    );
}

function checksum(node) {
    if (!node.left) {
        return 1;
    }
    return 1 + checksum(node.left) + checksum(node.right);
}

function createTree(depth) {
    return depth-- > 0
        ? { left: createTree(depth), right: createTree(depth) }
        : { left: null, right: null };
}

main()
