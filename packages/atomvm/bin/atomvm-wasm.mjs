#!/usr/bin/env node
/**
 * atomvm-wasm - AtomVM executable backed by the WebAssembly build shipped in
 * public/. Command line and exit codes match the native generic-UNIX binary:
 *
 *   atomvm-wasm -v                 print version banner, exit 0
 *   atomvm-wasm app.avm [lib.avm]  run app; exit code is AtomVM's
 *
 * This lets AtomVMNodeRuntime / AtomVMProcessBroker execute real AtomVM
 * bytecode on machines without a native AtomVM installation (CI, fog/edge
 * nodes, containers). It never uses a shell.
 *
 * Exit latency: the shipped build is pthread-enabled, so Node runs it with
 * Worker isolates. V8 tiers the (large) wasm module up with TurboFan in
 * background threads, and Node cannot tear a Worker isolate down until those
 * compile jobs finish, so process exit stalled ~0.7-2 s after the program's
 * output was complete (measured: `node --liftoff-only` 0.3 s vs 1.1-2.3 s; a
 * 60M-iteration Erlang loop runs at the same speed with either). We therefore
 * enable Liftoff-only wasm compilation before the module is compiled. Exit
 * codes and output are untouched (no exit-path hacks).
 *
 * NOTE: spawn/1,3 (and other estdlib functions) are Erlang wrappers in
 * estdlib's erlang.beam, so an app that uses them must ship that module in its
 * .avm. The AVM must also end with a full 12-byte zero entry (see
 * src/avm-packer.mjs); a shorter trailer makes AtomVM spin forever on any
 * module-lookup miss instead of reporting `undef`.
 */
import vm from 'node:vm';
import v8 from 'node:v8';
import { createRequire } from 'node:module';
import { readFileSync } from 'node:fs';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { parseAvm } from '../src/avm-packer.mjs';
import { atomvmAssetName } from '../src/assets.mjs';

const REFUSED_EXIT_CODE = 2;
const publicDir = join(dirname(fileURLToPath(import.meta.url)), '..', 'public');
const jsPath = join(publicDir, atomvmAssetName('node', 'js'));
const wasmPath = join(publicDir, atomvmAssetName('node', 'wasm'));

function refuse(message) {
  process.stderr.write(`atomvm-wasm: ${message}\n`);
  process.exit(REFUSED_EXIT_CODE);
}

const args = process.argv.slice(2);
if (args.length === 1 && (args[0] === '-v' || args[0] === '--version')) {
  process.stdout.write('AtomVM (wasm-node)\n');
  process.exit(0);
}
if (args.length === 0) refuse('usage: atomvm-wasm <app.avm> [lib.avm ...]');

const avmPaths = args.map(path => resolve(path));
for (const path of avmPaths) {
  let bytes;
  try {
    bytes = readFileSync(path);
  } catch (error) {
    refuse(`cannot read ${path}: ${error.message}`);
  }
  try {
    // The wasm build aborts opaquely on malformed archives; refuse them clearly.
    // Library archives (estdlib) have no start module, so only the app is checked.
    if (path === avmPaths[0]) parseAvm(new Uint8Array(bytes));
  } catch (error) {
    refuse(`${path}: ${error.message}`);
  }
}

// Must run before the wasm module is compiled (see header comment).
v8.setFlagsFromString('--liftoff-only');

// The Emscripten prologue is `var Module = typeof Module != "undefined" ? ...`,
// which only honours a pre-set Module when evaluated as a global script.
globalThis.Module = {
  arguments: avmPaths,
  locateFile: () => wasmPath, // Emscripten hardcodes AtomVM.wasm
  onExit: code => process.exit(code),
};

globalThis.require = createRequire(jsPath);
globalThis.__filename = jsPath;
globalThis.__dirname = publicDir;
vm.runInThisContext(readFileSync(jsPath, 'utf8'), { filename: jsPath });
