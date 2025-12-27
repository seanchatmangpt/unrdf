#!/usr/bin/env node
/**
 * @file CTAN Resolver Integration Example
 * @module kgc-cli/lib/latex/integration-example
 *
 * @description
 * Demonstrates how Agent 3 (engine runner) and Agent 10 (synthesis editor)
 * integrate with Agent 4 (package resolver) for LaTeX compilation.
 *
 * **Flow**:
 * 1. Agent 10: Initiates LaTeX compilation
 * 2. Engine detects missing inputs
 * 3. Agent 3: Calls resolver to fetch from CTAN
 * 4. Agent 3: Augments VFS with resolved files
 * 5. Agent 3: Re-runs compilation with complete VFS
 * 6. Agent 10: Returns PDF to user
 */

import { resolveMissingInputs, augmentVfsWithResolvedPackages, getCacheStats } from './ctan-resolver.mjs';

// ============================================================================
// Mock Engine Runner (Agent 3)
// ============================================================================

/**
 * Simulated LaTeX engine that detects missing inputs
 * @param {Record<string, Uint8Array>} vfs - Virtual file system
 * @returns {Promise<{success: boolean, missingInputs: string[], log: string, pdf?: Uint8Array}>}
 */
async function mockLatexEngine(vfs) {
  // Simulate checking VFS for required packages
  const mainTex = vfs['work/main.tex'];
  if (!mainTex) {
    return {
      success: false,
      missingInputs: [],
      log: 'Error: No main.tex found',
    };
  }

  // Parse \usepackage commands to detect missing .sty files
  const texSource = new TextDecoder().decode(mainTex);
  const packageMatches = texSource.matchAll(/\\usepackage(?:\[.*?\])?\{([^}]+)\}/g);

  const requiredPackages = [];
  for (const match of packageMatches) {
    const packages = match[1].split(',').map(p => p.trim());
    requiredPackages.push(...packages);
  }

  // Check which packages are missing from VFS
  const missingInputs = [];
  for (const pkg of requiredPackages) {
    const styPath = `texmf/tex/latex/${pkg}/${pkg}.sty`;
    if (!vfs[styPath]) {
      missingInputs.push(`${pkg}.sty`);
    }
  }

  if (missingInputs.length > 0) {
    return {
      success: false,
      missingInputs,
      log: `LaTeX Error: Missing packages: ${missingInputs.join(', ')}`,
    };
  }

  // All packages present - simulate successful compilation
  return {
    success: true,
    missingInputs: [],
    log: 'LaTeX compilation successful',
    pdf: new Uint8Array([0x25, 0x50, 0x44, 0x46]), // Mock PDF header
  };
}

// ============================================================================
// Integration: Agent 3 + Agent 4
// ============================================================================

/**
 * Compile LaTeX with automatic CTAN package resolution
 * @param {string} texSource - LaTeX source code
 * @param {string} cacheDir - Cache directory for CTAN packages
 * @returns {Promise<{success: boolean, pdf?: Uint8Array, log: string}>}
 */
export async function compileWithAutoResolve(texSource, cacheDir) {
  console.log('┌─────────────────────────────────────────────┐');
  console.log('│ LaTeX Compilation with CTAN Auto-Resolve    │');
  console.log('└─────────────────────────────────────────────┘\n');

  // Step 1: Initial VFS setup
  console.log('[Agent 10] Setting up initial VFS...');
  let vfs = {
    'work/main.tex': new TextEncoder().encode(texSource),
  };
  console.log(`  ✓ VFS has ${Object.keys(vfs).length} file(s)\n`);

  // Step 2: First compilation attempt
  console.log('[Agent 3] Running LaTeX engine (attempt 1)...');
  let result = await mockLatexEngine(vfs);

  if (!result.success && result.missingInputs.length > 0) {
    console.log(`  ⚠ Missing ${result.missingInputs.length} package(s): ${result.missingInputs.join(', ')}\n`);

    // Step 3: Resolve missing inputs via CTAN
    console.log('[Agent 4] Resolving missing packages from CTAN...');
    try {
      const resolved = await resolveMissingInputs({
        missingInputs: result.missingInputs,
        cacheDir,
      });

      console.log(`  ✓ Resolved ${resolved.size} package(s):`);
      for (const [path, content] of resolved) {
        console.log(`    - ${path} (${content.length} bytes)`);
      }
      console.log('');

      // Step 4: Augment VFS
      console.log('[Agent 3] Augmenting VFS with resolved packages...');
      vfs = augmentVfsWithResolvedPackages(vfs, resolved);
      console.log(`  ✓ VFS now has ${Object.keys(vfs).length} file(s)\n`);

      // Step 5: Re-run compilation
      console.log('[Agent 3] Running LaTeX engine (attempt 2)...');
      result = await mockLatexEngine(vfs);
    } catch (error) {
      console.error(`  ✗ Resolution failed: ${error.message}\n`);
      return {
        success: false,
        log: `CTAN resolution failed: ${error.message}`,
      };
    }
  }

  // Step 6: Final result
  if (result.success) {
    console.log('  ✓ Compilation successful!\n');
    console.log('[Agent 10] Returning PDF to user...');
    console.log(`  ✓ PDF size: ${result.pdf.length} bytes\n`);

    // Cache stats
    const stats = getCacheStats(cacheDir);
    console.log('[Cache Stats]');
    console.log(`  Total entries: ${stats.totalEntries}`);
    console.log(`  Total size: ${stats.totalSize} bytes`);
    console.log(`  Files: ${stats.files.join(', ') || 'none'}\n`);
  } else {
    console.log(`  ✗ Compilation failed: ${result.log}\n`);
  }

  console.log('└─────────────────────────────────────────────┘\n');

  return {
    success: result.success,
    pdf: result.pdf,
    log: result.log,
  };
}

// ============================================================================
// Example Usage
// ============================================================================

if (import.meta.url === `file://${process.argv[1]}`) {
  // Example LaTeX document
  const exampleTex = `\\documentclass{article}
\\usepackage{algorithm2e}
\\usepackage{amsmath}

\\title{Example Document}
\\author{Agent 4}

\\begin{document}
\\maketitle

\\section{Introduction}
This document demonstrates CTAN package auto-resolution.

\\begin{algorithm}[H]
  \\SetAlgoLined
  \\KwData{Missing LaTeX packages}
  \\KwResult{Resolved packages from CTAN}
  Check cache\\;
  \\eIf{cached}{
    Return from cache\\;
  }{
    Fetch from CTAN\\;
    Cache for future use\\;
  }
  \\caption{Package Resolution Algorithm}
\\end{algorithm}

\\end{document}`;

  const cacheDir = '/tmp/kgc-latex-integration-example';

  console.log('Running integration example...\n');
  console.log('LaTeX Source:');
  console.log('─────────────');
  console.log(exampleTex);
  console.log('─────────────\n');

  compileWithAutoResolve(exampleTex, cacheDir)
    .then(result => {
      if (result.success) {
        console.log('✅ Integration example completed successfully!');
        console.log(`   PDF would be ${result.pdf.length} bytes`);
      } else {
        console.error('❌ Integration example failed:');
        console.error(`   ${result.log}`);
        console.error('\nNote: This is expected when running offline or without CTAN access.');
        console.error('      The mock engine demonstrates the flow, not actual compilation.');
      }
    })
    .catch(error => {
      console.error('❌ Unexpected error:', error.message);
      console.error('\nNote: Network errors are expected when offline.');
      console.error('      This example demonstrates the integration flow.');
    });
}

// Export for use in other modules
export default compileWithAutoResolve;
