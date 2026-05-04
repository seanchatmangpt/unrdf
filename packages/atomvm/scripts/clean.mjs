/**
 * Clean Script for AtomVM
 *
 * Removes build artifacts (.beam files, .avm files, etc.)
 */

import { rmSync, existsSync, readdirSync } from 'fs';
import { join, resolve, dirname } from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const rootDir = resolve(__dirname, '../');
const srcDir = join(rootDir, 'src/erlang');

/**
 * Clean build artifacts
 */
export async function clean() {
  console.log('Cleaning build artifacts...');

  // Clean .beam files from src/erlang
  if (existsSync(srcDir)) {
    const files = readdirSync(srcDir);
    let cleaned = 0;

    for (const file of files) {
      if (file.endsWith('.beam')) {
        const filePath = join(srcDir, file);
        rmSync(filePath);
        console.log(`  Removed ${file}`);
        cleaned++;
      }
    }

    if (cleaned > 0) {
      console.log(`✓ Cleaned ${cleaned} .beam file(s)`);
    } else {
      console.log('✓ No .beam files to clean');
    }
  } else {
    console.log('✓ No src/erlang directory to clean');
  }

  // Optionally clean .avm files from public (commented out by default)
  // Uncomment if you want to remove .avm files too
  /*
  if (existsSync(publicDir)) {
    const files = readdirSync(publicDir);
    let cleaned = 0;
    
    for (const file of files) {
      if (file.endsWith('.avm') && file !== 'hello_world.avm') {
        const filePath = join(publicDir, file);
        rmSync(filePath);
        console.log(`  Removed ${file}`);
        cleaned++;
      }
    }
    
    if (cleaned > 0) {
      console.log(`✓ Cleaned ${cleaned} .avm file(s)`);
    }
  }
  */

  console.log('✓ Clean complete');
}
