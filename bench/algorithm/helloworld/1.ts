function main() {
    const args = Deno.args;
    const name = args[0];
    console.log(`Hello world ${name}!`);
}

main();
