#!/usr/bin/env node
/**
 * @file Usage Example - Enhanced Resolver
 * @module kgc-cli/lib/latex/cache/usage-example
 *
 * @description
 * Demonstrates how compile.mjs should call the enhanced resolver
 * with retry logic and local fixture support.
 */

import { resolveMissingInputs, augmentVfsWithResolvedPackages } from './resolve.mjs';
import { buildCtanUrls, extractPackageName as _extractPackageName, getPackageMetadata } from './ctan-map.mjs';

// ============================================================================
// EXAMPLE 1: Production Usage (CTAN with retry)
// ============================================================================

async function productionExample() {
  console.log('\n=== Example 1: Production Usage ===\n');

  const missingInputs = ['algorithm2e.sty', 'tikz.sty', 'beamer.cls'];
  const cacheDir = '/home/user/.cache/kgc-latex';

  try {
    // Resolve with retry logic
    const resolved = await resolveMissingInputs({
      missingInputs,
      cacheDir,
      registry: 'https://mirrors.ctan.org', // Default CTAN
      maxRetries: 3,                        // Exponential backoff
      initialDelay: 100,                    // Starting at 100ms
    });

    console.log(`✅ Resolved ${resolved.size} packages:`);
    for (const [vfsPath, content] of resolved.entries()) {
      console.log(`   - ${vfsPath} (${content.length} bytes)`);
    }

    // Merge into VFS
    const vfs = { 'work/main.tex': new Uint8Array([/* ... */]) };
    const augmentedVfs = augmentVfsWithResolvedPackages(vfs, resolved);

    console.log(`\n✅ VFS now has ${Object.keys(augmentedVfs).length} files`);
    return augmentedVfs;

  } catch (error) {
    console.error(`❌ Resolution failed: ${error.message}`);
    throw error;
  }
}

// ============================================================================
// EXAMPLE 2: Testing with Local Fixture Server
// ============================================================================

async function testingExample() {
  console.log('\n=== Example 2: Testing with Local Fixtures ===\n');

  const missingInputs = ['test-package.sty'];
  const cacheDir = '/tmp/test-cache';

  try {
    // Use local HTTP server for testing
    const resolved = await resolveMissingInputs({
      missingInputs,
      cacheDir,
      registry: 'http://localhost:3000/fixtures',
      maxRetries: 0, // No retry for local fixtures
    });

    console.log(`✅ Resolved from local fixture: ${resolved.size} packages`);
    return resolved;

  } catch (error) {
    console.error(`❌ Local fixture failed: ${error.message}`);
    throw error;
  }
}

// ============================================================================
// EXAMPLE 3: Package Metadata Inspection
// ============================================================================

function metadataExample() {
  console.log('\n=== Example 3: Package Metadata ===\n');

  const packages = ['algorithm2e.sty', 'tikz.sty', 'beamer.cls'];

  for (const pkg of packages) {
    const metadata = getPackageMetadata(pkg);
    const urls = buildCtanUrls(pkg);

    console.log(`\nPackage: ${pkg}`);
    console.log(`  → CTAN package: ${metadata.package}`);
    console.log(`  → VFS path: ${metadata.vfsPath}`);
    console.log(`  → Extension: ${metadata.extension}`);
    console.log(`  → Candidate URLs (${urls.length}):`);
    urls.forEach((url, i) => {
      console.log(`    ${i + 1}. ${url}`);
    });
  }
}

// ============================================================================
// EXAMPLE 4: Integration with compile.mjs
// ============================================================================

/**
 * This is how compile.mjs should integrate the resolver
 */
async function compileIntegrationExample(
  vfs,
  missingInputs,
  cacheDir,
  lockfile,
  registry = 'https://mirrors.ctan.org'
) {
  console.log('\n=== Example 4: compile.mjs Integration ===\n');

  // Step 1: Resolve missing inputs
  console.log(`Resolving ${missingInputs.length} missing inputs...`);
  const resolved = await resolveMissingInputs({
    missingInputs,
    cacheDir,
    registry,
    lockfile,        // Optional: for version pinning
    maxRetries: 3,
    initialDelay: 100,
  });

  console.log(`✅ Resolved ${resolved.size} packages`);

  // Step 2: Augment VFS
  const augmentedVfs = augmentVfsWithResolvedPackages(vfs, resolved);
  console.log(`✅ VFS augmented: ${Object.keys(vfs).length} → ${Object.keys(augmentedVfs).length} files`);

  // Step 3: Update lockfile (Agent 5's job)
  if (lockfile) {
    for (const [vfsPath, _content] of resolved.entries()) {
      const inputName = vfsPath.split('/').pop();
      // recordResolvedInput(lockfile, { inputName, hash, ... });
      console.log(`  → Lockfile updated: ${inputName}`);
    }
  }

  return augmentedVfs;
}

// ============================================================================
// EXAMPLE 5: Error Handling
// ============================================================================

async function errorHandlingExample() {
  console.log('\n=== Example 5: Error Handling ===\n');

  try {
    await resolveMissingInputs({
      missingInputs: ['nonexistent-package-xyz.sty'],
      cacheDir: '/tmp/cache',
      registry: 'https://invalid-mirror.example.com',
      maxRetries: 1,
      initialDelay: 50,
    });
  } catch (error) {
    console.log('Expected error caught:');
    console.log(`  → Message includes: ${error.message.includes('Failed to resolve') ? '✅' : '❌'} "Failed to resolve"`);
    console.log(`  → Message includes: ${error.message.includes('URLs attempted') ? '✅' : '❌'} "URLs attempted"`);
    console.log(`  → Message includes: ${error.message.includes('tlmgr install') ? '✅' : '❌'} "tlmgr install"`);
  }
}

// ============================================================================
// RUN EXAMPLES
// ============================================================================

async function main() {
  console.log('╔════════════════════════════════════════════════════════════╗');
  console.log('║  Enhanced Resolver - Usage Examples                       ║');
  console.log('╚════════════════════════════════════════════════════════════╝');

  // Example 3: Metadata (no network calls)
  metadataExample();

  // Example 5: Error handling
  await errorHandlingExample();

  // Examples 1, 2, 4 require network/fixtures
  console.log('\n\n💡 To run network examples:');
  console.log('   - Example 1: Requires internet connection');
  console.log('   - Example 2: Requires local server at http://localhost:3000/fixtures');
  console.log('   - Example 4: Requires actual VFS and missing inputs');

  console.log('\n✅ Examples completed\n');
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch(console.error);
}

// Export for testing
export {
  productionExample,
  testingExample,
  metadataExample,
  compileIntegrationExample,
  errorHandlingExample,
};
