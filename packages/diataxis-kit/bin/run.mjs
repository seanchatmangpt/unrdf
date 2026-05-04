#!/usr/bin/env node
/**
 * @file Diátaxis Kit orchestrator - full pipeline execution
 * @description Discovers packages, gathers evidence, classifies, and scaffolds documentation
 */

import { mkdir, writeFile, readFile, rm } from 'node:fs/promises';
import { join, resolve } from 'node:path';
import { existsSync } from 'node:fs';
import { discoverPackages } from '../src/inventory.mjs';
import { collectEvidence } from '../src/evidence.mjs';
import { classifyPackage } from '../src/classify.mjs';
import { generateScaffold } from '../src/scaffold.mjs';
import { stableStringify } from '../src/stable-json.mjs';

/**
 * Main orchestration function
 * @returns {Promise<void>}
 */
async function main() {
  const isDeterministic = process.env.DETERMINISTIC === '1';
  const workspaceRoot = resolve(process.cwd(), '../..');
  const artifactsDir = resolve(process.cwd(), 'ARTIFACTS', 'diataxis');
  const outDir = resolve(process.cwd(), 'OUT');

  console.log('🚀 Diátaxis Kit - Full Pipeline');
  console.log(`   Workspace: ${workspaceRoot}`);
  console.log(`   Deterministic: ${isDeterministic ? 'YES' : 'NO'}`);
  console.log('');

  // Step 1: Discover packages
  console.log('📦 Discovering packages...');
  const packages = await discoverPackages(workspaceRoot);
  console.log(`   Found ${packages.length} packages`);

  // Step 2: Create ARTIFACTS directory structure
  await mkdir(artifactsDir, { recursive: true });
  await mkdir(outDir, { recursive: true });

  // Step 3: Gather evidence and classify each package
  console.log('');
  console.log('🔍 Gathering evidence and classifying...');

  const diataxisEntries = [];
  for (const pkg of packages) {
    // Read package.json for evidence collection
    const pkgJsonPath = join(pkg.dir, 'package.json');
    const pkgJsonContent = await readFile(pkgJsonPath, 'utf8');
    const pkgJson = JSON.parse(pkgJsonContent);

    const evidence = await collectEvidence(pkg.dir, pkgJson);
    const diataxisEntry = await classifyPackage(pkg, evidence);

    // Override timestamp if deterministic mode
    if (isDeterministic) {
      diataxisEntry.generatedAt = '2000-01-01T00:00:00.000Z';
    }

    diataxisEntries.push(diataxisEntry);

    // Save per-package diataxis.json
    const packageDir = join(artifactsDir, pkg.name);
    await mkdir(packageDir, { recursive: true });
    await writeFile(
      join(packageDir, 'diataxis.json'),
      stableStringify(diataxisEntry)
    );

    console.log(`   ✓ ${pkg.name}`);
  }

  // Step 4: Save inventory.json
  const inventory = {
    generatedAt: isDeterministic ? '2000-01-01T00:00:00.000Z' : new Date().toISOString(),
    packageCount: packages.length,
    packages: packages.map(pkg => ({
      name: pkg.name,
      version: pkg.version,
      dir: pkg.dir
    }))
  };

  await writeFile(
    join(artifactsDir, 'inventory.json'),
    stableStringify(inventory)
  );

  // Step 5: Generate scaffolds
  console.log('');
  console.log('📝 Generating scaffolds...');

  for (const diataxisEntry of diataxisEntries) {
    const packageOutDir = join(outDir, diataxisEntry.packageName);

    try {
      await generateScaffold(diataxisEntry, packageOutDir);
      console.log(`   ✓ ${diataxisEntry.packageName}`);
    } catch (error) {
      console.error(`   ✗ ${diataxisEntry.packageName}: ${error.message}`);
    }
  }

  console.log('');
  console.log('✅ Pipeline complete');
  console.log(`   Artifacts: ${artifactsDir}`);
  console.log(`   Output: ${outDir}`);
}

// Execute main
main().catch(error => {
  console.error('❌ Pipeline failed:', error.message);
  console.error(error.stack);
  process.exit(1);
});
