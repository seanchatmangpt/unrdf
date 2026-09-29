#!/usr/bin/env node
/**
 * Batch 3 - Final: Generate Zod schemas for remaining 94 exports in src/
 *
 * Target: 194/194 schemas = 100% coverage
 * Focus: CLI, Delta, Validation, Grammar, Project-engine, Utils
 *
 * @module scripts/generate-src-schemas-batch3
 */

import { generateSchemasForFiles } from '../packages/v6-compat/src/schema-generator.mjs';
import { writeFile, mkdir } from 'fs/promises';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));
const ROOT = join(__dirname, '..');

/**
 * Batch 3 target modules - organized by priority
 */
const BATCH_3_PATTERNS = [
  // CLI exports (highest priority)
  join(ROOT, 'src/cli/**/*.mjs'),
  join(ROOT, 'src/commands/**/*.mjs'),

  // Delta/Admission exports
  join(ROOT, 'src/admission/**/*.mjs'),

  // Validation exports
  join(ROOT, 'src/validation/**/*.mjs'),

  // Project engine exports
  join(ROOT, 'src/project-engine/**/*.mjs'),

  // Knowledge engine exports
  join(ROOT, 'src/knowledge-engine/**/*.mjs'),

  // Composables exports
  join(ROOT, 'src/composables/**/*.mjs'),

  // Utils exports
  join(ROOT, 'src/utils/**/*.mjs'),

  // Measurement exports
  join(ROOT, 'src/measurement/**/*.mjs'),

  // Browser exports
  join(ROOT, 'src/browser/**/*.mjs'),

  // Context exports
  join(ROOT, 'src/context/**/*.mjs'),

  // Dependency analyzer exports
  join(ROOT, 'src/dependency-analyzer/**/*.mjs'),

  // Receipts exports
  join(ROOT, 'src/receipts/**/*.mjs'),

  // Security exports
  join(ROOT, 'src/security/**/*.mjs'),

  // Universe exports
  join(ROOT, 'src/universe/**/*.mjs'),

  // Projection exports
  join(ROOT, 'src/projection/**/*.mjs'),

  // Profiling exports
  join(ROOT, 'src/profiling/**/*.mjs'),

  // Monorepo admission exports
  join(ROOT, 'src/monorepo-admission/**/*.mjs'),

  // React hooks exports
  join(ROOT, 'src/react-hooks/**/*.mjs'),

  // Top-level exports
  join(ROOT, 'src/diff.mjs'),
  join(ROOT, 'src/ken.mjs'),
  join(ROOT, 'src/ken-parliment.mjs'),
  join(ROOT, 'src/knowledge-engine.mjs'),
  join(ROOT, 'src/verification.mjs'),
];

/**
 * Main execution
 */
async function main() {
  console.log('🚀 Batch 3 - Final: Zod Schema Generation\n');
  console.log('Target: 194/194 schemas (100% coverage)\n');
  console.log('━'.repeat(70) + '\n');

  const allResults = [];
  const moduleStats = {};
  let totalFunctions = 0;
  let totalFiles = 0;

  // Process each pattern
  for (const pattern of BATCH_3_PATTERNS) {
    const moduleName = extractModuleName(pattern);
    console.log(`📦 Processing: ${moduleName}`);

    try {
      const results = await generateSchemasForFiles(pattern, {
        outputSuffix: '.schema.mjs',
        dryRun: false, // Actually write the files
      });

      if (results.length > 0) {
        totalFiles += results.length;
        const functionCount = results.reduce((sum, r) => sum + r.functions.length, 0);
        totalFunctions += functionCount;

        moduleStats[moduleName] = {
          files: results.length,
          functions: functionCount,
          schemas: functionCount * 2, // params + returns
        };

        allResults.push(...results);

        console.log(`  ✅ ${results.length} files, ${functionCount} functions`);
      } else {
        console.log(`  ⚠️  No exports found`);
      }
    } catch (error) {
      console.log(`  ❌ Error: ${error.message}`);
    }
  }

  console.log('\n' + '━'.repeat(70));
  console.log('📊 Batch 3 Summary:\n');
  console.log(`  Files processed: ${totalFiles}`);
  console.log(`  Functions found: ${totalFunctions}`);
  console.log(`  Schemas generated: ${totalFunctions * 2} (params + returns)`);
  console.log(`  Schema files written: ${totalFiles}`);
  console.log('━'.repeat(70) + '\n');

  // Module breakdown
  console.log('📂 Module Breakdown:\n');
  for (const [module, stats] of Object.entries(moduleStats)) {
    console.log(`  ${module}:`);
    console.log(`    Files: ${stats.files}, Functions: ${stats.functions}, Schemas: ${stats.schemas}`);
  }

  // Generate sample definitions
  console.log('\n' + '━'.repeat(70));
  console.log('📝 Sample Schema Definitions (5 examples):\n');

  const samples = allResults
    .filter(r => r.functions.length > 0)
    .slice(0, 5);

  samples.forEach((sample, idx) => {
    const fn = sample.functions[0];
    console.log(`${idx + 1}. ${fn.name}() - ${sample.file.split('/').pop()}`);
    console.log(`   Module: ${extractModuleName(sample.file)}`);
    console.log(`   Params: ${fn.params.map(p => `${p.name}: ${p.type}`).join(', ') || 'none'}`);
    console.log(`   Returns: ${fn.returnType}`);
    console.log(`   Schema file: ${sample.outputPath}`);
    console.log();
  });

  // Generate integration manifest
  const manifest = {
    version: '6.0.0-batch3',
    timestamp: new Date().toISOString(),
    coverage: {
      target: 194,
      achieved: totalFunctions,
      percentage: ((totalFunctions / 194) * 100).toFixed(2) + '%',
    },
    modules: moduleStats,
    files: allResults.map(r => ({
      source: r.file.replace(ROOT, ''),
      schema: r.outputPath.replace(ROOT, ''),
      functions: r.functions.length,
    })),
  };

  const manifestPath = join(ROOT, 'src/schemas/batch3-manifest.json');
  await mkdir(dirname(manifestPath), { recursive: true });
  await writeFile(manifestPath, JSON.stringify(manifest, null, 2), 'utf-8');

  console.log('━'.repeat(70));
  console.log(`✅ Manifest written to: ${manifestPath}`);
  console.log('━'.repeat(70) + '\n');

  // Final status
  if (totalFunctions >= 94) {
    console.log('🎉 Batch 3 Complete! Target achieved or exceeded.\n');
  } else {
    console.log(`⚠️  Batch 3 Incomplete: ${totalFunctions}/94 schemas generated.\n`);
  }

  return manifest;
}

/**
 * Extract module name from file pattern
 */
function extractModuleName(pattern) {
  const parts = pattern.split('/');
  const srcIdx = parts.indexOf('src');
  if (srcIdx !== -1 && srcIdx + 1 < parts.length) {
    return parts[srcIdx + 1].replace(/\*\*.*/, '').replace('.mjs', '') || 'root';
  }
  return 'unknown';
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main()
    .then((manifest) => {
      console.log('✨ Schema generation complete!\n');
      process.exit(0);
    })
    .catch((error) => {
      console.error('❌ Schema generation failed:', error);
      process.exit(1);
    });
}

export { main, BATCH_3_PATTERNS };
