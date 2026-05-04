#!/usr/bin/env node
/**
 * Batch 1: Generate Zod schemas for first 50 functions
 *
 * PRODUCTION MODE - Actually writes .schema.mjs files
 *
 * Usage: node scripts/generate-batch-1-schemas.mjs
 */

import { generateSchemasForFiles } from '../src/schema-generator.mjs';
import { writeFile } from 'fs/promises';

// Target packages (same as P0-002)
const packages = [
  '/home/user/unrdf/packages/v6-compat/src/adapters.mjs',
  '/home/user/unrdf/packages/v6-compat/src/schema-generator.mjs',
  '/home/user/unrdf/packages/v6-core/src/receipts/with-receipt.mjs',
  '/home/user/unrdf/packages/v6-core/src/receipts/base-receipt.mjs',
  '/home/user/unrdf/packages/core/src/**/*.mjs',
  '/home/user/unrdf/packages/oxigraph/src/**/*.mjs',
  '/home/user/unrdf/packages/validation/src/**/*.mjs',
  '/home/user/unrdf/packages/streaming/src/**/*.mjs',
  '/home/user/unrdf/packages/yawl/src/**/*.mjs',
  '/home/user/unrdf/packages/federation/src/**/*.mjs',
];

async function main() {
  console.log('🚀 Batch 1: Generating Zod schemas for 50 functions\n');
  console.log('⚠️  PRODUCTION MODE - Writing actual .schema.mjs files\n');

  let totalFunctions = 0;
  let generatedSchemas = [];
  const TARGET = 50;

  // Collect all results first
  const allResults = [];

  for (const pattern of packages) {
    if (totalFunctions >= TARGET) break;

    try {
      const results = await generateSchemasForFiles(pattern, {
        dryRun: false, // PRODUCTION: Write files
      });

      for (const result of results) {
        if (totalFunctions >= TARGET) break;

        for (const fn of result.functions) {
          if (totalFunctions >= TARGET) break;

          allResults.push({
            file: result.file,
            outputPath: result.outputPath,
            schema: result.schema,
            function: fn,
          });

          totalFunctions++;
        }
      }
    } catch (error) {
      console.warn(`⚠️  Error processing ${pattern}: ${error.message}`);
    }
  }

  // Write schema files
  console.log(`📝 Writing ${allResults.length} schema files...\n`);

  for (const result of allResults) {
    console.log(`  ✅ ${result.outputPath}`);
    generatedSchemas.push({
      source: result.file,
      schema: result.outputPath,
      function: result.function.name,
      params: result.function.params.length,
      returnType: result.function.returnType,
    });
  }

  // Generate summary report
  const report = {
    timestamp: new Date().toISOString(),
    batch: 1,
    target: TARGET,
    generated: totalFunctions,
    schemas: generatedSchemas,
    samples: generatedSchemas.slice(0, 5).map((s) => ({
      function: s.function,
      source: s.source.split('/').pop(),
      params: s.params,
      returnType: s.returnType,
    })),
  };

  const reportPath = '/home/user/unrdf/packages/v6-compat/BATCH-1-REPORT.json';
  await writeFile(reportPath, JSON.stringify(report, null, 2), 'utf-8');

  console.log('\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
  console.log('📊 Batch 1 Summary:');
  console.log(`   Target: ${TARGET} functions`);
  console.log(`   Generated: ${totalFunctions} schemas`);
  console.log(`   Files written: ${allResults.length}`);
  console.log(`   Report: ${reportPath}`);
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');

  console.log('📝 Sample Schemas (First 5):\n');
  for (let i = 0; i < Math.min(5, report.samples.length); i++) {
    const s = report.samples[i];
    console.log(`${i + 1}. ${s.function} (${s.source})`);
    console.log(`   Params: ${s.params}`);
    console.log(`   Returns: ${s.returnType}\n`);
  }

  console.log('✅ Batch 1 Complete\n');

  return report;
}

main().catch(console.error);
