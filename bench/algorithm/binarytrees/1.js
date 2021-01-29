/* The Computer Language Benchmarks Game
 * https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
 *
 * contributed by François Pirsch
 * Based on the node.js program from Léo Sarrazin and Andrey Filatkin
*/

const { Worker, isMainThread, parentPort, workerData } = require('worker_threads');

if (isMainThread) {
    mainThread();
} else {
    workerThread();
}

async function mainThread () {
    const maxDepth = Math.max(6, +process.argv[2] || 0);
    const stretchDepth = maxDepth + 1;
    const tasks = [{ iterations: 1, depth: stretchDepth }];

    const longLivedTree = createTree(maxDepth);

    for (let depth = 4; depth <= maxDepth; depth += 2) {
        const iterations = 1 << maxDepth - depth + 4;
        tasks.push({ iterations, depth });
    }

    (await runTasks(tasks)).forEach(result => process.stdout.write(result));

    process.stdout.write(
        `long lived tree of depth ${maxDepth}\t check: ${checksum(longLivedTree)}\n`
    );
}

function runTasks (tasks) {
    return new Promise(resolve => {
        const results = [];
        let tasksRemaining = tasks.length;
        for (let i = 0; i < tasks.length; i++) {
            new Worker(__filename, { workerData: tasks[i] })
            .on('message', message => {
                results[i] = message;
                tasksRemaining--;
                if (tasksRemaining === 0) {
                    resolve(results);
                }
            });
        }
    });
}

function workerThread () {
    const { iterations, depth } = workerData;
    let check = 0;
    for (let i = 0; i < iterations; i++) {
        check += checksum(createTree(depth));
    }
    parentPort.postMessage(
        iterations === 1
        ? `stretch tree of depth ${depth}\t check: ${check}\n`
        : `${iterations}\t trees of depth ${depth}\t check: ${check}\n`
    );
}

function checksum (node) {
    if (!node.left) {
        return 1;
    }
    return 1 + checksum(node.left) + checksum(node.right);
}

function createTree (depth) {
    return depth-- > 0
        ? { left: createTree(depth), right: createTree(depth) }
        : { left: null, right: null };
}
