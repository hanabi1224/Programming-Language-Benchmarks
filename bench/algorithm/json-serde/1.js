const fs = require('fs');
const crypto = require('crypto');

function printHash(data) {
    const str = JSON.stringify(data);
    const hasher = crypto.createHash('md5');
    const hash = hasher.update(str, 'utf-8').digest('hex');
    console.log(hash);
}

function main() {
    let fileName = process.argv[2] || "sample";
    let n = +process.argv[3] || 3;
    const jsonStr = fs.readFileSync(`${fileName}.json`, 'utf8');
    printHash(JSON.parse(jsonStr));
    const array = [];
    for (var i = 0; i < n; i++) {
        array.push(JSON.parse(jsonStr));
    }
    printHash(array);
}

main();
