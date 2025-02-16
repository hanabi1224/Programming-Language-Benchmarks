import * as esbuild from "npm:esbuild";
import { denoPlugins } from "jsr:@luca/esbuild-deno-loader";

const result = await esbuild.build({
  plugins: [...denoPlugins()],
  entryPoints: ["c.ts"],
  outfile: "app.ts",
  bundle: true,
  format: "esm",
});

esbuild.stop();
