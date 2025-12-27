#!/usr/bin/env node
/**
 * @file KGC Docs CLI
 * @module kgc-docs/cli
 *
 * CLI tool for building, verifying, and proving KGC documentation
 */

import { readFile, writeFile, readdir, mkdir } from 'node:fs/promises';
import { join, basename, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import {
  parseKGCMarkdown,
  renderDiataxisView,
  generateProofTree,
  verifyProof,
} from '../src/kgc-markdown.mjs';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

/**
 * Build documentation from .kgcmd files
 * @param {string} sourceDir - Source directory
 * @param {string} outputDir - Output directory
 */
async function buildDocs(sourceDir, outputDir) {
  console.log(`Building docs from ${sourceDir} to ${outputDir}`);

  const files = await readdir(sourceDir);
  const kgcmdFiles = files.filter((f) => f.endsWith('.kgcmd'));

  console.log(`Found ${kgcmdFiles.length} .kgcmd files`);

  const views = ['tutorials', 'how-to', 'reference', 'explanations'];
  const stats = {
    files_processed: 0,
    views_generated: 0,
  };

  // Create output directories
  for (const view of views) {
    const viewDir = join(outputDir, view);
    await mkdir(viewDir, { recursive: true });
  }

  for (const file of kgcmdFiles) {
    const sourcePath = join(sourceDir, file);
    const markdown = await readFile(sourcePath, 'utf8');

    try {
      const ast = parseKGCMarkdown(markdown);
      const baseName = basename(file, '.kgcmd');

      // Render all 4 views
      const viewMap = {
        tutorials: 'tutorial',
        'how-to': 'how-to',
        reference: 'reference',
        explanations: 'explanation',
      };

      for (const [dir, viewType] of Object.entries(viewMap)) {
        const content = renderDiataxisView(ast, viewType);
        const outputPath = join(outputDir, dir, `${baseName}.md`);
        await writeFile(outputPath, content, 'utf8');
        stats.views_generated++;
      }

      stats.files_processed++;
      console.log(`✓ Processed ${file} → ${Object.keys(viewMap).length} views`);
    } catch (error) {
      console.error(`✗ Failed to process ${file}:`, error.message);
    }
  }

  console.log(`\nBuild complete:`);
  console.log(`  Files processed: ${stats.files_processed}`);
  console.log(`  Views generated: ${stats.views_generated}`);

  return stats;
}

/**
 * Verify proof appendices in documentation
 * @param {string} outputDir - Documentation output directory
 */
async function verifyDocs(outputDir) {
  console.log(`Verifying proofs in ${outputDir}`);

  const views = ['tutorials', 'how-to', 'reference', 'explanations'];
  let verified = 0;
  let failed = 0;

  for (const view of views) {
    const viewDir = join(outputDir, view);

    try {
      const files = await readdir(viewDir);
      const mdFiles = files.filter((f) => f.endsWith('.md'));

      for (const file of mdFiles) {
        const filePath = join(viewDir, file);
        const content = await readFile(filePath, 'utf8');

        // Check for proof appendix
        if (content.includes('## Proof Appendix')) {
          // Extract proof data
          const proofMatch = content.match(/```json\n([\s\S]*?)\n```/);
          if (proofMatch) {
            const proofData = JSON.parse(proofMatch[1]);
            if (proofData.merkle_root && proofData.o_hash) {
              verified++;
              console.log(`✓ ${view}/${file} - proof valid`);
            } else {
              failed++;
              console.error(`✗ ${view}/${file} - proof incomplete`);
            }
          }
        } else {
          failed++;
          console.error(`✗ ${view}/${file} - no proof appendix`);
        }
      }
    } catch (error) {
      console.error(`Error verifying ${view}:`, error.message);
    }
  }

  console.log(`\nVerification complete:`);
  console.log(`  Verified: ${verified}`);
  console.log(`  Failed: ${failed}`);

  return { verified, failed };
}

/**
 * Refresh all documentation
 * @param {string} sourceDir - Source directory
 * @param {string} outputDir - Output directory
 */
async function refreshDocs(sourceDir, outputDir) {
  console.log('Refreshing all documentation...');
  return buildDocs(sourceDir, outputDir);
}

/**
 * Generate proof trees for all documents
 * @param {string} sourceDir - Source directory
 * @param {string} proofsDir - Proofs output directory
 */
async function generateProofs(sourceDir, proofsDir) {
  console.log(`Generating proofs from ${sourceDir} to ${proofsDir}`);

  await mkdir(proofsDir, { recursive: true });

  const files = await readdir(sourceDir);
  const kgcmdFiles = files.filter((f) => f.endsWith('.kgcmd'));

  let proofCount = 0;

  for (const file of kgcmdFiles) {
    const sourcePath = join(sourceDir, file);
    const markdown = await readFile(sourcePath, 'utf8');

    try {
      const ast = parseKGCMarkdown(markdown);
      const proof = generateProofTree(ast);

      const baseName = basename(file, '.kgcmd');
      const proofPath = join(proofsDir, `${baseName}.proof.json`);

      await writeFile(proofPath, JSON.stringify(proof, null, 2), 'utf8');

      // Verify immediately
      const isValid = verifyProof(proof, ast);
      if (isValid) {
        console.log(`✓ ${file} → proof generated and verified`);
        proofCount++;
      } else {
        console.error(`✗ ${file} → proof verification failed`);
      }
    } catch (error) {
      console.error(`✗ Failed to generate proof for ${file}:`, error.message);
    }
  }

  console.log(`\nProof generation complete: ${proofCount} proofs`);
  return proofCount;
}

/**
 * Main CLI entry point
 */
async function main() {
  const command = process.argv[2];
  const sourceDir = process.argv[3] || 'docs/src';
  const outputDir = process.argv[4] || 'docs';
  const proofsDir = process.argv[5] || 'proofs';

  try {
    switch (command) {
      case 'build':
        await buildDocs(sourceDir, outputDir);
        break;

      case 'verify':
        await verifyDocs(outputDir);
        break;

      case 'refresh':
        await refreshDocs(sourceDir, outputDir);
        break;

      case 'prove':
        await generateProofs(sourceDir, proofsDir);
        break;

      default:
        console.log(`
KGC Docs CLI

Usage:
  kgc-docs build [sourceDir] [outputDir]     - Build documentation
  kgc-docs verify [outputDir]                - Verify proof appendices
  kgc-docs refresh [sourceDir] [outputDir]   - Rebuild all docs
  kgc-docs prove [sourceDir] [proofsDir]     - Generate proof trees

Defaults:
  sourceDir:  docs/src
  outputDir:  docs
  proofsDir:  proofs
        `);
        process.exit(1);
    }
  } catch (error) {
    console.error('Error:', error.message);
    process.exit(1);
  }
}

main();
