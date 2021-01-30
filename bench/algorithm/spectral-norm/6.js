// The Computer Language Benchmarks Game
// https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
//
// contributed by Ian Osgood
// Optimized by Roy Williams
// modified for Node.js by Isaac Gouy
// multi thread by Andrey Filatkin

const { Worker, isMainThread, parentPort, workerData } = require('worker_threads');
const os = require('os');

const bytesPerFloat = Float64Array.BYTES_PER_ELEMENT;

if (isMainThread) {
    mainThread(+process.argv[2]);
} else {
    workerThread(workerData);
}

async function mainThread(n) {
    const sab = new SharedArrayBuffer(3 * bytesPerFloat * n);
    const u = new Float64Array(sab, 0, n).fill(1);
    const v = new Float64Array(sab, bytesPerFloat * n, n);

    const workers = new Set();
    startWorkers();

    for (let i = 0; i < 10; i++) {
        await atAu('u', 'v', 'w');
        await atAu('v', 'u', 'w');
    }

    stopWorkers();

    let vBv = 0;
    let vv = 0;
    for (let i = 0; i < n; i++) {
        vBv += u[i] * v[i];
        vv += v[i] * v[i];
    }

    const result = Math.sqrt(vBv / vv);

    console.log(result.toFixed(9));

    async function atAu(u, v, w) {
        await work('au', { vec1: u, vec2: w });
        await work('atu', { vec1: w, vec2: v });
    }

    function startWorkers() {
        const cpus = os.cpus().length;
        const chunk = Math.ceil(n / cpus);

        for (let i = 0; i < cpus; i++) {
            const start = i * chunk;
            let end = start + chunk;
            if (end > n) {
                end = n;
            }
            const worker = new Worker(__filename, { workerData: { n, start, end } });

            worker.postMessage({ name: 'sab', data: sab });
            workers.add(worker);
        }
    }

    function work(name, data) {
        return new Promise(resolve => {
            let wait = 0;
            workers.forEach(worker => {
                worker.postMessage({ name, data });
                worker.once('message', () => {
                    wait--;
                    if (wait === 0) {
                        resolve();
                    }
                });
                wait++;
            });
        });
    }

    function stopWorkers() {
        workers.forEach(worker => worker.postMessage({ name: 'exit' }));
    }
}

function workerThread({ n, start, end }) {
    const data = {
        u: null,
        v: null,
        w: null,
    };

    parentPort.on('message', message => {
        const name = message.name;

        if (name === 'sab') {
            const sab = message.data;
            data.u = new Float64Array(sab, 0, n);
            data.v = new Float64Array(sab, bytesPerFloat * n, n);
            data.w = new Float64Array(sab, 2 * bytesPerFloat * n, n);
        } else if (name === 'au') {
            au(data[message.data.vec1], data[message.data.vec2]);
            parentPort.postMessage({ name: 'end' });
        } else if (name === 'atu') {
            atu(data[message.data.vec1], data[message.data.vec2]);
            parentPort.postMessage({ name: 'end' });
        } else if (name === 'exit') {
            process.exit();
        }
    });

    function au(u, v) {
        for (let i = start; i < end; i++) {
            let t = 0;
            for (let j = 0; j < n; j++) {
                t += u[j] / a(i, j);
            }
            v[i] = t;
        }
    }

    function atu(u, v) {
        for (let i = start; i < end; i++) {
            let t = 0;
            for (let j = 0; j < n; j++) {
                t += u[j] / a(j, i);
            }
            v[i] = t;
        }
    }

    function a(i, j) {
        return ((i + j) * (i + j + 1) >>> 1) + i + 1;
    }
}