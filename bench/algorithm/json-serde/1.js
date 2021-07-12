const fs = require('fs');
const crypto = require('crypto');

function main() {
    let fileName = process.argv[2] || "sample";
    let n = +process.argv[3] || 3;
    const jsonStr = fs.readFileSync(`${fileName}.json`, 'utf8');
    for (var i = 1; i <= n; i++) {
        const data = JSON.parse(jsonStr);
        const prettified = JSON.stringify(data, null, i);
        const hasher = crypto.createHash('md5');
        const hash = hasher.update(prettified, 'utf-8').digest('hex');
        console.log(hash);
    }
}

main();
