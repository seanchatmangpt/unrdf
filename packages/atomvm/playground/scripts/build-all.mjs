/**
 * Build All Modules Script
 * 
 * Builds all Erlang modules in erlang/validation-modules/
 */

import { readdirSync, existsSync } from 'fs';
import { join, resolve, dirname } from 'path';
import { fileURLToPath } from 'url';
import { buildModule } from './build.mjs';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const rootDir = resolve(__dirname, '../');
const erlangDir = join(rootDir, 'erlang/validation-modules');

async function buildAll() {
  if (!existsSync(erlangDir)) {
    throw new Error(`Erlang directory not found: ${erlangDir}`);
  }

  const files = readdirSync(erlangDir);
  const erlFiles = files.filter(f => f.endsWith('.erl'));

  if (erlFiles.length === 0) {
    console.log('No .erl files found in erlang/validation-modules/');
    return;
  }

  console.log(`Found ${erlFiles.length} Erlang modules to build:\n`);

  const results = [];
  for (const erlFile of erlFiles) {
    const moduleName = erlFile.replace('.erl', '');
    console.log(`\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━`);
    console.log(`Building: ${moduleName}`);
    console.log(`━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n`);
    
    try {
      await buildModule(moduleName);
      results.push({ module: moduleName, status: 'success' });
    } catch (error) {
      console.error(`\n❌ Failed to build ${moduleName}: ${error.message}\n`);
      results.push({ module: moduleName, status: 'failed', error: error.message });
    }
  }

  console.log(`\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━`);
  console.log('Build Summary:');
  console.log(`━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n`);

  const successful = results.filter(r => r.status === 'success');
  const failed = results.filter(r => r.status === 'failed');

  console.log(`✅ Successful: ${successful.length}`);
  successful.forEach(r => console.log(`   - ${r.module}`));

  if (failed.length > 0) {
    console.log(`\n❌ Failed: ${failed.length}`);
    failed.forEach(r => console.log(`   - ${r.module}: ${r.error}`));
  }

  if (failed.length > 0) {
    process.exit(1);
  }
}

buildAll().catch(error => {
  console.error(`Error: ${error.message}`);
  process.exit(1);
});

