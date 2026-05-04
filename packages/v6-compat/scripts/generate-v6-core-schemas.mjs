#!/usr/bin/env node
/**
 * PHASE 3.1: Generate Zod schemas for ALL v6-core exports
 *
 * Target: 194 exports → 194 schemas (100% coverage)
 * Current: 8 schemas (Batch 1)
 * Gap: 186 remaining
 *
 * Usage: node scripts/generate-v6-core-schemas.mjs
 */

import { generateSchemasForFiles } from '../src/schema-generator.mjs';
import { glob } from 'glob';
import { readFile } from 'fs/promises';

/**
 * Find all v6-core modules excluding tests and existing schemas
 */
async function findV6CoreModules() {
  const pattern = '/home/user/unrdf/packages/v6-core/src/**/*.mjs';
  const allFiles = await glob(pattern, { absolute: true });

  // Exclude: tests, existing schemas
  const moduleFiles = allFiles.filter((f) => {
    return !f.includes('.test.mjs') && !f.includes('.schema.mjs');
  });

  return moduleFiles;
}

/**
 * Count exported functions in a file
 */
async function countExportedFunctions(file) {
  const source = await readFile(file, 'utf-8');
  const exportMatches = source.match(/^export\s+(async\s+)?function\s+\w+/gm);
  return exportMatches ? exportMatches.length : 0;
}

async function main() {
  console.log('🚀 PHASE 3.1: v6-core Zod Schema Generation\n');
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');

  // Find all v6-core modules
  const modules = await findV6CoreModules();
  console.log(`📦 Found ${modules.length} v6-core modules\n`);

  // Count baseline exports
  let totalExports = 0;
  const moduleStats = [];

  for (const file of modules) {
    const exportCount = await countExportedFunctions(file);
    if (exportCount > 0) {
      totalExports += exportCount;
      moduleStats.push({ file, exports: exportCount });
    }
  }

  console.log(`📊 Baseline: ${totalExports} exported functions across ${moduleStats.length} modules\n`);

  // Generate schemas
  console.log('🔧 Generating schemas...\n');

  const results = await generateSchemasForFiles(modules, {
    outputSuffix: '.schema.mjs',
    dryRun: false, // WRITE FILES
  });

  // Analyze results
  let totalFunctions = 0;
  let totalSchemas = 0;

  const byModule = {};

  for (const result of results) {
    const moduleName = result.file.split('/').slice(-3).join('/');
    const functionCount = result.functions.length;
    const schemaCount = functionCount * 2; // params + returns

    totalFunctions += functionCount;
    totalSchemas += schemaCount;

    byModule[moduleName] = {
      functions: functionCount,
      schemas: schemaCount,
      outputPath: result.outputPath,
    };

    console.log(`  ✅ ${moduleName}`);
    console.log(`     Functions: ${functionCount}, Schemas: ${schemaCount}`);
  }

  console.log('\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');
  console.log('📊 PHASE 3.1 RESULTS:\n');
  console.log(`   Modules processed: ${results.length}`);
  console.log(`   Functions with schemas: ${totalFunctions}`);
  console.log(`   Total schema exports: ${totalSchemas}`);
  console.log(`   Coverage: ${totalFunctions}/${totalExports} (${((totalFunctions / totalExports) * 100).toFixed(1)}%)`);
  console.log('\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');

  // Sample schemas
  console.log('📝 Sample Schemas (First 3):\n');

  for (let i = 0; i < Math.min(3, results.length); i++) {
    const result = results[i];
    const fn = result.functions[0];
    if (!fn) continue;

    console.log(`${i + 1}. ${fn.name} (${result.file.split('/').pop()})`);
    console.log(`   Params: ${fn.params.map((p) => `${p.name}: ${p.type}`).join(', ')}`);
    console.log(`   Returns: ${fn.returnType}`);
    console.log('');
  }

  console.log('✅ PHASE 3.1 Complete\n');

  // Return stats for reporting
  return {
    modulesProcessed: results.length,
    functionsWithSchemas: totalFunctions,
    totalSchemas,
    coverage: totalFunctions / totalExports,
    byModule,
  };
}

main()
  .then((stats) => {
    console.log(`\n🎯 Ready signal: PHASE 3.1 complete, ${stats.totalSchemas} schemas generated for v6-core\n`);
    process.exit(0);
  })
  .catch((error) => {
    console.error('❌ FAILED:', error);
    process.exit(1);
  });
