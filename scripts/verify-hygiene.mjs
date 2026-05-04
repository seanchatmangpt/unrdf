/**
 * @file verify-hygiene.mjs
 * @description CI/CD tool to enforce zero unregulated technical debt (TODO/FIXME/HACK)
 */

import { readFile, readdir, stat } from 'fs/promises';
import { join, extname } from 'path';

const FORBIDDEN_MARKERS = [/^\s*\/\/.*(?:TODO|FIXME|HACK):/i, /^\s*\/\*.*(?:TODO|FIXME|HACK):/m];
const DEFERRED_MARKER = /^\s*\/\/ DEFERRED_ACTION\(#[a-z0-9-]+\):/i;

const TARGET_DIRS = ['packages', 'sidecar', 'hive', 'src'];
const EXCLUDE_DIRS = ['node_modules', 'dist', 'test', 'examples', 'playground', '.next', '.pnpm-store', 'coverage', 'docs'];
const INCLUDE_EXTENSIONS = ['.mjs', '.js', '.ts', '.mts', '.cts'];

let totalErrors = 0;
let totalManagedDebt = 0;

async function scan(dir) {
  const entries = await readdir(dir);
  
  for (const entry of entries) {
    if (EXCLUDE_DIRS.includes(entry)) continue;
    
    const fullPath = join(dir, entry);
    const s = await stat(fullPath);
    
    if (s.isDirectory()) {
      await scan(fullPath);
    } else if (s.isFile()) {
      if (!INCLUDE_EXTENSIONS.includes(extname(entry))) continue;
      await checkFile(fullPath);
    }
  }
}

async function checkFile(filePath) {
  const content = await readFile(filePath, 'utf-8');
  const lines = content.split('\n');
  
  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    
    // Check for managed debt
    if (DEFERRED_MARKER.test(line)) {
      totalManagedDebt++;
      continue;
    }
    
    // Check for forbidden unregulated debt
    for (const marker of FORBIDDEN_MARKERS) {
      if (marker.test(line)) {
        console.error(`❌ Unregulated technical debt found in ${filePath}:${i + 1}`);
        console.error(`   Line: ${line.trim()}`);
        totalErrors++;
      }
    }
  }
}

async function main() {
  console.log('🚀 Starting UNRDF Hygiene Verification...');
  
  for (const dir of TARGET_DIRS) {
    try {
      await scan(dir);
    } catch (e) {
      // Ignore if directory doesn't exist
    }
  }
  
  console.log('\n--- Hygiene Summary ---');
  console.log(`✅ Managed Debt (DEFERRED_ACTION): ${totalManagedDebt}`);
  console.log(`❌ Unregulated Debt (TODO/FIXME/HACK): ${totalErrors}`);
  
  if (totalErrors > 0) {
    console.error(`\n🚨 Hygiene verification FAILED. Total errors: ${totalErrors}`);
    process.exit(1);
  } else {
    console.log('\n🌟 Hygiene verification PASSED.');
  }
}

main().catch(console.error);
