import Context from "https://deno.land/std/wasi/snapshot_preview1.ts";

const context = new Context({
    args: Deno.args,
    env: Deno.env.toObject(),
});

const binary = await Deno.readFile("_app.wasm");
const module = await WebAssembly.compile(binary);
const instance = await WebAssembly.instantiate(module, {
    "wasi_snapshot_preview1": context.exports,
});

context.start(instance);
