/**
 * Playground Build Script
 * 
 * Builds Erlang modules from erlang/validation-modules/ to public/
 * This is what a developer would do when using @unrdf/atomvm
 */

import { readFileSync, existsSync, mkdirSync, statSync } from 'fs';
import { join, resolve, dirname } from 'path';
import { fileURLToPath } from 'url';
import { execSync } from 'child_process';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const rootDir = resolve(__dirname, '../');
const erlangDir = join(rootDir, 'erlang/validation-modules');
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
  if (!existsSync(erlangDir)) {
    throw new Error(`Erlang directory not found: ${erlangDir}`);
  }
  if (!existsSync(publicDir)) {
    mkdirSync(publicDir, { recursive: true });
  }

  // POKA-YOKE: Module must exist (don't auto-generate)
  const erlFile = join(erlangDir, `${moduleName}.erl`);
  if (!existsSync(erlFile)) {
    throw new Error(`Module not found: ${erlFile}. Create the .erl file first.`);
  }

  console.log(`✓ Found ${moduleName}.erl`);

  // Step 1: Compile to BEAM
  const beamFile = join(erlangDir, `${moduleName}.beam`);
  console.log(`Compiling ${moduleName}.erl to ${moduleName}.beam...`);
  
  try {
    execSync(`erlc -o ${erlangDir} ${erlFile}`, { stdio: 'inherit' });
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

  // Step 2: Package to .avm using packbeam
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

// CLI entry point
if (import.meta.url === `file://${process.argv[1]}`) {
  const moduleName = process.argv[2];
  if (!moduleName) {
    console.error('Usage: node scripts/build.mjs <moduleName>');
    process.exit(1);
  }
  buildModule(moduleName).catch(error => {
    console.error(`Error: ${error.message}`);
    process.exit(1);
  });
}

