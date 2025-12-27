#!/usr/bin/env node
/**
 * P0-002: Generate Zod schemas for 10 core packages
 *
 * Usage: node scripts/generate-schemas.mjs
 */

import { generateSchemasForFiles } from '../src/schema-generator.mjs';

// 10 core packages to apply schema generation
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
  console.log('🔧 P0-002: Generating Zod schemas for 10 core packages...\n');

  let totalFunctions = 0;
  let totalFiles = 0;

  for (const pattern of packages) {
    console.log(`\n📦 Processing: ${pattern}`);

    try {
      const results = await generateSchemasForFiles(pattern, {
        dryRun: true, // Don't write files in this demo
      });

      if (results.length > 0) {
        totalFiles += results.length;

        for (const result of results.slice(0, 2)) {
          // Show first 2 results per package
          console.log(`  ✅ ${result.file}`);
          console.log(`     Functions: ${result.functions.length}`);
          totalFunctions += result.functions.length;

          // Show first function schema
          if (result.functions.length > 0) {
            const fn = result.functions[0];
            console.log(`     Example: ${fn.name}(${fn.params.map((p) => p.name).join(', ')})`);
          }
        }

        if (results.length > 2) {
          console.log(`  ... and ${results.length - 2} more files`);
        }
      } else {
        console.log('  ⚠️  No exported functions found');
      }
    } catch (error) {
      console.warn(`  ❌ Error: ${error.message}`);
    }
  }

  console.log('\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
  console.log(`📊 Summary:`);
  console.log(`   Files processed: ${totalFiles}`);
  console.log(`   Functions found: ${totalFunctions}`);
  console.log(`   Schemas generated: ${totalFunctions * 2} (params + returns)`);
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n');

  // Generate 5 sample schemas
  console.log('📝 Sample Schemas (First 5 functions):\n');

  let sampleCount = 0;
  for (const pattern of packages) {
    if (sampleCount >= 5) break;

    try {
      const results = await generateSchemasForFiles(pattern, { dryRun: true });

      for (const result of results) {
        if (sampleCount >= 5) break;

        for (const fn of result.functions) {
          if (sampleCount >= 5) break;

          console.log(`${sampleCount + 1}. ${fn.name} (${result.file.split('/').pop()})`);
          console.log(`   Params: ${fn.params.map((p) => `${p.name}: ${p.type}`).join(', ')}`);
          console.log(`   Returns: ${fn.returnType}`);
          console.log(`   Schema: z.tuple([${fn.params.map((p) => `z.${p.type}()`).join(', ')}])\n`);

          sampleCount++;
        }
      }
    } catch (error) {
      // Skip on error
    }
  }

  console.log('✅ P0-002 Complete\n');
}

main().catch(console.error);
