const utf8Encoder = new TextEncoder();

function write(str: string) {
    Deno.stdout.write(utf8Encoder.encode(str));
}

async function* generate() {
    for (var i = 2; ; i++) {
        yield i;
    }
}

async function* filter(ch: AsyncGenerator, prime: number) {
    while (true) {
        var i = (await ch.next()).value;
        if (i % prime != 0) {
            yield i;
        }
    }
}

async function findPrimes(n: number) {
    var ch = generate();
    for (var i = 0; i < n; i++) {
        const prime = (await ch.next()).value as number;
        console.log(prime.toString());
        ch = filter(ch, prime);
    }
}

function main() {
    const n = +Deno.args[0] || 100;
    findPrimes(n);
}

main();
