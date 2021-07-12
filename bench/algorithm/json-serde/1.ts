import { createHash } from "https://deno.land/std/hash/mod.ts";

async function main() {
    let fileName = Deno.args[0] || "sample";
    let n = +Deno.args[1] || 3;
    const jsonStr = await Deno.readTextFile(`${fileName}.json`);
    for (var i = 1; i <= n; i++) {
        const data = JSON.parse(jsonStr);
        const prettified = JSON.stringify(data, null, i);
        const hasher = createHash('md5');
        hasher.update(prettified);
        console.log(hasher.toString());
    }
}

main();
