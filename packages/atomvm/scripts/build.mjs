/**
 * Build an Erlang module into a runnable AtomVM AVM application.
 */
import { existsSync, mkdirSync, readFileSync, statSync, writeFileSync } from 'node:fs';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { execFileSync } from 'node:child_process';
import { z } from 'zod';

const SafeModuleNameSchema = z.string()
  .regex(/^[a-zA-Z][a-zA-Z0-9_]*$/, 'Module name must start with a letter and contain only letters, numbers, and underscores');

const __dirname = dirname(fileURLToPath(import.meta.url));
const rootDir = resolve(__dirname, '..');
const srcDir = join(rootDir, 'src/erlang');
const publicDir = join(rootDir, 'public');

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
 * - PACKBEAM_BIN=/path/to/PackBEAM
 */
export async function buildModule(moduleName) {
  const validatedModuleName = SafeModuleNameSchema.parse(moduleName);
  const erlc = process.env.ERLC_BIN || 'erlc';
  const packbeam = process.env.PACKBEAM_BIN || 'PackBEAM';

  mkdirSync(srcDir, { recursive: true });
  mkdirSync(publicDir, { recursive: true });

  const erlFile = join(srcDir, `${validatedModuleName}.erl`);
  const beamFile = join(srcDir, `${validatedModuleName}.beam`);
  const avmFile = join(publicDir, `${validatedModuleName}.avm`);

  if (!existsSync(erlFile)) {
    writeFileSync(erlFile, generateErlangModule(validatedModuleName), 'utf8');
    console.log(`Created ${erlFile}`);
  }

  runTool(erlc, ['-o', srcDir, erlFile], 'erlc');
  if (!existsSync(beamFile)) {
    throw new Error(`Compilation failed: ${beamFile} was not created`);
  }
  const beamHeader = readFileSync(beamFile).subarray(0, 4);
  if (!beamHeader.equals(Buffer.from('FOR1'))) {
    throw new Error(`Invalid BEAM file: ${beamFile} does not have a FOR1 header`);
  }

  // PackBEAM uses positional output/input arguments. It has no -o option.
  runTool(packbeam, [avmFile, beamFile], 'PackBEAM');
  if (!existsSync(avmFile) || statSync(avmFile).size === 0) {
    throw new Error(`Packaging failed: ${avmFile} was not created or is empty`);
  }

  console.log(`Built runnable AtomVM application: ${avmFile}`);
  return Object.freeze({ moduleName: validatedModuleName, erlFile, beamFile, avmFile });
}

function generateErlangModule(moduleName) {
  return `-module(${moduleName}).
-export([start/0]).

start() ->
    erlang:display({atomvm_module_alive, ${moduleName}}),
    0.
`;
}
