/**
 * Validate Build Script
 * 
 * POKA-YOKE: Ensures all .erl files have corresponding .avm files
 */

import { readdirSync, existsSync, statSync } from 'fs';
import { join, resolve, dirname } from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const rootDir = resolve(__dirname, '../');
const erlangDir = join(rootDir, 'erlang/validation-modules');
const publicDir = join(rootDir, 'public');

function validateBuild() {
  if (!existsSync(erlangDir)) {
    throw new Error(`Erlang directory not found: ${erlangDir}`);
  }

  if (!existsSync(publicDir)) {
    console.error(`Public directory not found: ${publicDir}`);
    console.error('Run: pnpm run build:all');
    process.exit(1);
  }

  const files = readdirSync(erlangDir);
  const erlFiles = files.filter(f => f.endsWith('.erl'));

  if (erlFiles.length === 0) {
    console.log('No .erl files found in erlang/validation-modules/');
    return;
  }

  console.log(`Validating ${erlFiles.length} Erlang modules...\n`);

  const missing = [];
  const invalid = [];

  for (const erlFile of erlFiles) {
    const moduleName = erlFile.replace('.erl', '');
    const avmFile = join(publicDir, `${moduleName}.avm`);

    if (!existsSync(avmFile)) {
      missing.push(moduleName);
      continue;
    }

    // Verify AVM file is valid (non-empty)
    const stats = statSync(avmFile);
    if (stats.size === 0) {
      invalid.push(moduleName);
    }
  }

  if (missing.length === 0 && invalid.length === 0) {
    console.log('✅ All modules are built and valid!\n');
    erlFiles.forEach(erlFile => {
      const moduleName = erlFile.replace('.erl', '');
      console.log(`   ✓ ${moduleName}.avm`);
    });
    return;
  }

  if (missing.length > 0) {
    console.error('❌ Missing .avm files:\n');
    missing.forEach(module => {
      console.error(`   - ${module}.avm (source: ${module}.erl)`);
    });
    console.error('\nRun: pnpm run build:erlang <module>');
  }

  if (invalid.length > 0) {
    console.error('\n❌ Invalid .avm files (empty):\n');
    invalid.forEach(module => {
      console.error(`   - ${module}.avm`);
    });
    console.error('\nRebuild these modules: pnpm run build:erlang <module>');
  }

  process.exit(1);
}

validateBuild();

