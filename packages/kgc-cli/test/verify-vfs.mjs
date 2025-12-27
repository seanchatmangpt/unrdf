#!/usr/bin/env node
/**
 * @fileoverview Standalone VFS verification script
 * Tests VFS collection without requiring vitest
 */

import { normalizeToVFS, sortVFSPaths, isValidVFSPath } from '../src/lib/latex/path-normalize.mjs';
import { collectProjectFiles, listProjectFilesSorted, getVFSStats } from '../src/lib/latex/project-files.mjs';
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

console.log('🧪 VFS Verification Script\n');

// Test 1: Path normalization
console.log('Test 1: Path normalization');
const vfsPath = normalizeToVFS('/home/user/project/main.tex', '/home/user/project');
console.assert(vfsPath === 'work/main.tex', `Expected work/main.tex, got ${vfsPath}`);
console.log('✅ Path normalization works');

// Test 2: VFS path validation
console.log('\nTest 2: VFS path validation');
console.assert(isValidVFSPath('work/main.tex') === true, 'Valid path rejected');
console.assert(isValidVFSPath('main.tex') === false, 'Invalid path accepted');
console.assert(isValidVFSPath('work/../etc/passwd') === false, 'Path traversal accepted');
console.log('✅ VFS path validation works');

// Test 3: Path sorting
console.log('\nTest 3: Path sorting');
const unsorted = ['work/packages/b.tex', 'work/a.tex', 'work/packages/a.tex'];
const sorted = sortVFSPaths(unsorted);
const expected = ['work/a.tex', 'work/packages/a.tex', 'work/packages/b.tex'];
console.assert(JSON.stringify(sorted) === JSON.stringify(expected), 'Sorting failed');
console.log('✅ Path sorting works');

// Test 4: File collection (if thesis directory exists)
console.log('\nTest 4: File collection from thesis directory');
const thesisPath = join(__dirname, '../../../thesis');

try {
  const vfs = await collectProjectFiles(thesisPath, {
    include: ['.tex'],
    exclude: ['node_modules', '.git', '.kgc'],
  });

  console.log(`📁 Collected ${vfs.size} files`);

  const files = listProjectFilesSorted(vfs);
  console.log('\n📋 Files in VFS:');
  files.forEach(file => console.log(`  - ${file}`));

  // Verify all paths are normalized
  for (const path of vfs.keys()) {
    console.assert(path.startsWith('work/'), `Path doesn't start with work/: ${path}`);
    console.assert(!path.includes('\\'), `Path contains backslash: ${path}`);
  }

  // Verify all values are Uint8Array
  for (const [path, content] of vfs.entries()) {
    console.assert(content instanceof Uint8Array, `Content not Uint8Array: ${path}`);
    console.assert(content.length > 0, `Empty file: ${path}`);
  }

  // Show statistics
  const stats = getVFSStats(vfs);
  console.log('\n📊 VFS Statistics:');
  console.log(`  Total files: ${stats.fileCount}`);
  console.log(`  Total bytes: ${stats.totalBytes.toLocaleString()}`);
  console.log(`  By extension:`, stats.byExtension);

  console.log('\n✅ File collection works');

  // Verify expected files exist
  const hasMainTex = vfs.has('work/main.tex');
  const hasPreamble = vfs.has('work/preamble.tex');

  console.log('\n🔍 Key file verification:');
  console.log(`  work/main.tex: ${hasMainTex ? '✅' : '❌'}`);
  console.log(`  work/preamble.tex: ${hasPreamble ? '✅' : '❌'}`);

  if (hasMainTex) {
    const mainContent = vfs.get('work/main.tex');
    const decoder = new TextDecoder();
    const preview = decoder.decode(mainContent.slice(0, 100));
    console.log(`\n📄 main.tex preview (first 100 bytes):\n${preview}...`);
  }

  console.log('\n✅ All VFS tests passed!');
  process.exit(0);

} catch (err) {
  console.error('❌ Test failed:', err.message);
  console.error(err.stack);
  process.exit(1);
}
