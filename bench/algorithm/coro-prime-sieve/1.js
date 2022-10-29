function* generate() {
    for (let i = 2; ; i++) {
        yield i;
    }
}

function* filter(ch, prime) {
    while (true) {
        const i = ch.next().value;
        if (i % prime != 0) {
            yield i;
        }
    }
}

function findPrimes(n) {
    let ch = generate();
    for (let i = 0; i < n; i++) {
        const prime = ch.next().value;
        console.log(prime);
        ch = filter(ch, prime);
    }
}

function main() {
    const n = +process.argv[2] || 100;
    findPrimes(n);
}

main();
