/**
 * Complete Workflow Script
 *
 * Builds Erlang module and provides next steps for execution.
 */

import { buildModule } from './build.mjs';
import { existsSync } from 'fs';
import { join, resolve, dirname } from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const rootDir = resolve(__dirname, '../');
const publicDir = join(rootDir, 'public');

/**
 * Complete workflow: build and provide execution instructions
 * @param {string} moduleName - Name of the module (required)
 */
export async function workflow(moduleName) {
  if (!moduleName) {
    throw new Error('moduleName is required');
  }
  console.log('🚀 AtomVM Complete Workflow\n');
  
  try {
    // Step 1: Build the module
    console.log('📦 Step 1: Building Erlang module...\n');
    await buildModule(moduleName);
    
    const avmFile = join(publicDir, `${moduleName}.avm`);
    
    if (!existsSync(avmFile)) {
      throw new Error(`Build failed: ${avmFile} not created`);
    }
    
    console.log('\n✅ Build successful!');
    console.log(`\n📁 Generated file: ${avmFile}`);
  } catch (error) {
    console.error(`\n❌ Workflow failed: ${error.message}\n`);
    throw error;
  }
}

