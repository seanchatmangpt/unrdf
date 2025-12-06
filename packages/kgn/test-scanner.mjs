#!/usr/bin/env node
import { scanWorkspace } from './src/doc-generator/scanner.mjs';
import { join } from 'path';

const rootDir = join(process.cwd(), '../..');
console.log('Scanning from:', rootDir);

const packages = await scanWorkspace(rootDir);
console.log('Found packages:', packages.length);

packages.forEach(pkg => {
  console.log(`\n${pkg.name}:`);
  console.log(`  Files: ${pkg.sourceFiles.length}`);
  pkg.sourceFiles.slice(0, 3).forEach(f => console.log(`    - ${f}`));
});
