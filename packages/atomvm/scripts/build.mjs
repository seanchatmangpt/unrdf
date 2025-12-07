/**
 * Build Script for AtomVM
 *
 * Creates Erlang module, compiles to BEAM, and packages to .avm file.
 */

import { writeFileSync, readFileSync, existsSync, mkdirSync, statSync } from 'fs';
import { join, resolve, dirname } from 'path';
import { fileURLToPath } from 'url';
import { execSync } from 'child_process';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const rootDir = resolve(__dirname, '../');
const srcDir = join(rootDir, 'src/erlang');
const publicDir = join(rootDir, 'public');

/**
 * Build an Erlang module to .avm file
 * @param {string} moduleName - Name of the module (without .erl extension, required)
 */
export async function buildModule(moduleName) {
  if (!moduleName) {
    throw new Error('moduleName is required');
  }
  console.log(`Building Erlang module: ${moduleName}`);

  // Ensure directories exist
  if (!existsSync(srcDir)) {
    mkdirSync(srcDir, { recursive: true });
  }
  if (!existsSync(publicDir)) {
    mkdirSync(publicDir, { recursive: true });
  }

  // Step 1: Create Erlang source file if it doesn't exist
  const erlFile = join(srcDir, `${moduleName}.erl`);
  if (!existsSync(erlFile)) {
    console.log(`Creating ${moduleName}.erl...`);
    const erlContent = generateErlangModule(moduleName);
    writeFileSync(erlFile, erlContent, 'utf8');
    console.log(`✓ Created ${moduleName}.erl`);
  } else {
    console.log(`✓ ${moduleName}.erl already exists`);
  }

  // Step 2: Compile to BEAM
  const beamFile = join(srcDir, `${moduleName}.beam`);
  console.log(`Compiling ${moduleName}.erl to ${moduleName}.beam...`);
  
  try {
    execSync(`erlc -o ${srcDir} ${erlFile}`, { stdio: 'inherit' });
  } catch (error) {
    throw new Error(`erlc command failed: ${error.message}`);
  }
  
  if (!existsSync(beamFile)) {
    throw new Error(`Compilation failed: ${beamFile} not created`);
  }
  
  // Verify BEAM file is valid (starts with "FOR1" magic)
  const beamHeader = readFileSync(beamFile, { encoding: null }).slice(0, 4);
  const beamMagic = Buffer.from([0x46, 0x4F, 0x52, 0x31]); // "FOR1"
  if (!beamHeader.equals(beamMagic)) {
    throw new Error(`Invalid BEAM file: ${beamFile} does not have BEAM magic header`);
  }
  
  console.log(`✓ Compiled to ${moduleName}.beam`);

  // Step 3: Package to .avm using packbeam
  const avmFile = join(publicDir, `${moduleName}.avm`);
  console.log(`Packaging ${moduleName}.beam to ${moduleName}.avm...`);

  try {
    execSync(`packbeam -o ${avmFile} ${beamFile}`, { stdio: 'inherit' });
  } catch (error) {
    throw new Error(`packbeam command failed: ${error.message}`);
  }
  
  if (!existsSync(avmFile)) {
    throw new Error(`Packaging failed: ${avmFile} not created`);
  }
  
  // Verify AVM file is valid (non-empty, has content)
  const avmStats = statSync(avmFile);
  if (avmStats.size === 0) {
    throw new Error(`Invalid AVM file: ${avmFile} is empty`);
  }
  
  console.log(`✓ Packaged to ${moduleName}.avm`);
  console.log(`✓ .avm file available at: ${avmFile}`);

  console.log(`\n✓ Build complete! ${moduleName}.avm is ready in public/ directory.`);
}

/**
 * Generate a simple Erlang module
 * @param {string} moduleName - Name of the module
 * @returns {string} Erlang source code
 */
function generateErlangModule(moduleName) {
  const capitalized = moduleName.charAt(0).toUpperCase() + moduleName.slice(1);
  
  return `-module(${moduleName}).
-export([start/0, world/0]).

%% Main entry point
start() ->
    io:format("Hello from AtomVM!~n"),
    io:format("Module: ${moduleName}~n"),
    world(),
    {ok, ${moduleName}}.

%% Example function
world() ->
    io:format("Hello, World from ${capitalized}!~n"),
    io:format("Running in browser via WebAssembly~n"),
    {ok, browser, ready}.
`;
}

