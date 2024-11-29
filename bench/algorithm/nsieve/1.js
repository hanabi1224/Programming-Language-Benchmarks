function nsieve(_m) {
    const m = _m | 0;
    let count = 0;
    const flags = new Int8Array(m);

    for (let i = 2 | 0; i < m; ++i) {
        if (flags[i] === 0) {
            ++count;
            for (let j = i << 1; j < m; j += i) {
                flags[j] = 1;
            }
        }
    }
    
    const mOut = m.toString().padStart(8);
    const countOut = count.toString().padStart(8);
    process.stdout.write(`Primes up to ${mOut} ${countOut}\n`);
}

function main() {
    const m = process.argv[2] | 0;
    
    for (let i = 0; i < 3; i++) {
        nsieve(10000 << (m - i));
    }
}

main();
