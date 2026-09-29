/**
 * Build an Erlang module into a runnable AtomVM AVM application.
 */
import { existsSync, mkdirSync, readFileSync, statSync, writeFileSync } from 'node:fs';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { execFileSync } from 'node:child_process';
import { packBeamFile } from './pack-beam.mjs';

const MODULE_NAME = /^[a-zA-Z][a-zA-Z0-9_]*$/;
const __dirname = dirname(fileURLToPath(import.meta.url));
const rootDir = resolve(__dirname, '..');
const srcDir = join(rootDir, 'src/erlang');
const publicDir = join(rootDir, 'public');

function validateModuleName(value) {
  if (typeof value !== 'string' || !MODULE_NAME.test(value)) {
    throw new TypeError('Module name must start with a letter and contain only letters, numbers, and underscores');
  }
  return value;
}

function runTool(binary, args, label) {
  try {
    execFileSync(binary, args, { stdio: 'inherit' });
  } catch (error) {
    throw new Error(`${label} failed: ${error.message}`, { cause: error });
  }
}

/**
 * Build an Erlang module to a runnable .avm file.
 *
 * Tool overrides:
 * - ERLC_BIN=/path/to/erlc
 * - PACKBEAM_BIN=/path/to/PackBEAM (default: PackBEAM on PATH; if absent, the
 *   built-in JS packer src/avm-packer.mjs is used)
 *
 * @param {string} moduleName
 * @param {{srcDir?: string, publicDir?: string}} [options] - override output dirs
 */
export async function buildModule(moduleName, options = {}) {
  const validatedModuleName = validateModuleName(moduleName);
  const erlc = process.env.ERLC_BIN || 'erlc';

  const erlDir = options.srcDir ?? srcDir;
  const outDir = options.publicDir ?? publicDir;
  mkdirSync(erlDir, { recursive: true });
  mkdirSync(outDir, { recursive: true });

  const erlFile = join(erlDir, `${validatedModuleName}.erl`);
  const beamFile = join(erlDir, `${validatedModuleName}.beam`);
  const avmFile = join(outDir, `${validatedModuleName}.avm`);

  if (!existsSync(erlFile)) {
    writeFileSync(erlFile, generateErlangModule(validatedModuleName), 'utf8');
    console.log(`Created ${erlFile}`);
  }

  runTool(erlc, ['-o', erlDir, erlFile], 'erlc');
  if (!existsSync(beamFile)) {
    throw new Error(`Compilation failed: ${beamFile} was not created`);
  }
  const beamHeader = readFileSync(beamFile).subarray(0, 4);
  if (!beamHeader.equals(Buffer.from('FOR1'))) {
    throw new Error(`Invalid BEAM file: ${beamFile} does not have a FOR1 header`);
  }

  // Native PackBEAM when available, otherwise the JS packer.
  const { kind } = packBeamFile(avmFile, beamFile);
  if (!existsSync(avmFile) || statSync(avmFile).size === 0) {
    throw new Error(`Packaging failed: ${avmFile} was not created or is empty`);
  }

  console.log(`Built runnable AtomVM application: ${avmFile} (packer: ${kind})`);
  return Object.freeze({ moduleName: validatedModuleName, erlFile, beamFile, avmFile, packer: kind });
}

function generateErlangModule(moduleName) {
  return `-module(${moduleName}).
-export([start/0]).

start() ->
    erlang:display({atomvm_module_alive, ${moduleName}}),
    ok.
`;
}
