#!/usr/bin/env node
/**
 * @fileoverview Simple docs generator for @unrdf/kgn package
 * Generates MDX documentation for the filters module
 */

import { parseFile } from '../src/doc-generator/parser.mjs';
import { generateModuleMDX } from '../src/doc-generator/mdx-generator.mjs';
import { writeFile, mkdir } from 'fs/promises';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

async function main() {
  console.log('📚 Generating Documentation for @unrdf/kgn\n');

  const rootDir = join(__dirname, '../../..');
  const sourceFile = join(__dirname, '../src/filters/rdf.js');
  const outputDir = join(rootDir, 'packages/nextra/app/reference/api/kgn');
  const outputFile = join(outputDir, 'page.mdx');

  try {
    // Parse source file
    console.log('📖 Parsing:', sourceFile);
    const parsed = parseFile(sourceFile, rootDir);
    console.log(`   Found ${parsed.exports.length} exports\n`);

    // Generate MDX
    console.log('📝 Generating MDX...');
    const mdx = generateModuleMDX(parsed);
    console.log(`   Generated ${mdx.length} characters\n`);

    // Write output
    console.log('💾 Writing to:', outputFile);
    await mkdir(outputDir, { recursive: true });
    await writeFile(outputFile, mdx, 'utf-8');

    console.log('\n✅ Documentation generated successfully!');
    console.log(`\nView at: packages/nextra/app/reference/api/kgn/page.mdx`);

  } catch (error) {
    console.error('\n❌ Generation failed:', error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

main();
