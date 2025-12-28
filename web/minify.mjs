import * as swc from "@swc/core";
import { readFileSync, writeFileSync } from "fs";

const pureFuncs = ["F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9"];

const inputFile = process.argv[2];
const outputFile = process.argv[3];

if (!inputFile || !outputFile) {
  console.error("Usage: node minify.mjs <input.js> <output.js>");
  process.exit(1);
}

const code = readFileSync(inputFile, "utf8");

const result = await swc.minify(code, {
  compress: {
    pure_funcs: pureFuncs,
    pure_getters: true,
    unsafe_comps: true,
    unsafe: true,
  },
  mangle: {
    reserved: pureFuncs,
  },
});

writeFileSync(outputFile, result.code);
