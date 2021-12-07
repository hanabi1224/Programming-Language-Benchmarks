import { createHash } from "https://deno.land/std/hash/mod.ts";

function printHash(data: any) {
    const str = JSON.stringify(data);
    const hasher = createHash('md5');
    hasher.update(str);
    console.log(hasher.toString());
}

async function main() {
    let fileName = Deno.args[0] || "sample";
    let n = +Deno.args[1] || 3;
    const jsonStr = await Deno.readTextFile(`${fileName}.json`);
    printHash(JSON.parse(jsonStr));
    const array = [];
    for (var i = 0; i < n; i++) {
        array.push(JSON.parse(jsonStr));
    }
    printHash(array);
}

main();
