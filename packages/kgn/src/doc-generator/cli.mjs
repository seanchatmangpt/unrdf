#!/usr/bin/env node
/**
 * @fileoverview CLI tool for documentation generation
 * Usage: node cli.mjs [options]
 */

import { parseFiles } from './parser.mjs';
import { generateModulesMDX } from './mdx-generator.mjs';
import { scanWorkspace, groupFilesByModule } from './scanner.mjs';
import { generateNavigation, generatePackageIndex, generateAPIIndex } from './nav-generator.mjs';
import { mkdir, writeFile } from 'fs/promises';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

/**
 * Main CLI function
 */
async function main() {
  console.log('📚 @unrdf/kgn Documentation Generator\n');

  // Default options
  const rootDir = join(__dirname, '../../../..');
  const outputDir = join(rootDir, 'packages/nextra/pages/api');

  console.log(`Root: ${rootDir}`);
  console.log(`Output: ${outputDir}\n`);

  try {
    // Phase 1: Scan workspace
    console.log('🔍 Phase 1: Scanning workspace...');
    const packages = await scanWorkspace(rootDir);
    console.log(`   Found ${packages.length} packages\n`);

    if (packages.length === 0) {
      console.log('⚠️  No packages found with source files');
      return;
    }

    // Group files by module
    const groupedFiles = groupFilesByModule(packages);

    // Phase 2: Parse all files
    console.log('📖 Phase 2: Parsing JSDoc...');
    let totalFiles = 0;
    const parsedByPackage = {};

    for (const [pkgName, pkgData] of Object.entries(groupedFiles)) {
      console.log(`   Parsing @unrdf/${pkgName}...`);
      const parsed = parseFiles(pkgData.sourceFiles, rootDir);
      parsedByPackage[pkgName] = parsed;
      totalFiles += parsed.length;
    }
    console.log(`   Parsed ${totalFiles} files\n`);

    // Phase 3: Generate MDX
    console.log('📝 Phase 3: Generating MDX...');
    let generatedCount = 0;

    for (const [pkgName, parsed] of Object.entries(parsedByPackage)) {
      const mdxMap = generateModulesMDX(parsed);

      // Write MDX files
      for (const [relativePath, mdx] of mdxMap) {
        const fileName = relativePath.split('/').pop().replace(/\.(m?js)$/, '.mdx');
        const pkgOutputDir = join(outputDir, pkgName);

        // Create directory if needed
        await mkdir(pkgOutputDir, { recursive: true });

        // Write MDX file
        const mdxPath = join(pkgOutputDir, fileName);
        await writeFile(mdxPath, mdx, 'utf-8');

        generatedCount++;
      }

      console.log(`   Generated ${mdxMap.size} files for @unrdf/${pkgName}`);
    }
    console.log(`   Total: ${generatedCount} MDX files\n`);

    // Phase 4: Generate navigation
    console.log('🗂️  Phase 4: Generating navigation...');

    // Generate _meta.json
    const navigation = generateNavigation(groupedFiles);
    const metaPath = join(outputDir, '_meta.json');
    await writeFile(metaPath, JSON.stringify(navigation, null, 2), 'utf-8');
    console.log(`   Created _meta.json`);

    // Generate index pages
    const apiIndex = generateAPIIndex(groupedFiles);
    const apiIndexPath = join(outputDir, 'index.mdx');
    await writeFile(apiIndexPath, apiIndex, 'utf-8');
    console.log(`   Created API index`);

    for (const [pkgName, pkgData] of Object.entries(groupedFiles)) {
      const pkgIndex = generatePackageIndex(pkgName, pkgData);
      const pkgIndexPath = join(outputDir, pkgName, 'index.mdx');
      await writeFile(pkgIndexPath, pkgIndex, 'utf-8');
      console.log(`   Created @unrdf/${pkgName} index`);
    }

    console.log('\n✅ Documentation generation complete!');
    console.log(`\n📊 Summary:`);
    console.log(`   Packages: ${packages.length}`);
    console.log(`   Source files: ${totalFiles}`);
    console.log(`   MDX files: ${generatedCount}`);
    console.log(`   Output: ${outputDir}`);

  } catch (error) {
    console.error('\n❌ Documentation generation failed:');
    console.error(error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

// Run CLI
main();
