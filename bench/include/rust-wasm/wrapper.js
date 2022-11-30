'use strict';
const fs = require('fs');
const { WASI } = require('wasi');

const wasi = new WASI({
    args: process.argv.slice(1),
    env: process.env,
});

const importObject = { wasi_snapshot_preview1: wasi.wasiImport };

(async () => {
    const wasm = await WebAssembly.compile(fs.readFileSync('./_app.wasm'));
    const instance = await WebAssembly.instantiate(wasm, importObject);

    wasi.start(instance);
})();
