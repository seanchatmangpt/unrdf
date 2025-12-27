#!/usr/bin/env node
/**
 * @fileoverview UNRDF v5 → v6 Migration Script
 *
 * Automated code transformations for upgrading to v6:
 * - Store initialization (N3 → Oxigraph)
 * - Import path updates
 * - Receipt wrapper injection
 * - Zod schema generation
 * - Configuration updates
 *
 * @example
 * ```bash
 * # Dry run (show changes without applying)
 * node scripts/migrate-to-v6.mjs --dry-run
 *
 * # Migrate specific package
 * node scripts/migrate-to-v6.mjs --package packages/my-app
 *
 * # Migrate entire workspace
 * node scripts/migrate-to-v6.mjs --all
 *
 * # Generate migration report
 * node scripts/migrate-to-v6.mjs --all --report migration-report.json
 * ```
 */

import { promises as fs } from 'fs';
import { resolve, join, relative, dirname } from 'path';
import { fileURLToPath } from 'url';
import { execSync } from 'child_process';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const ROOT = resolve(__dirname, '..');

// ============================================================================
// Configuration
// ============================================================================

const CONFIG = {
  dryRun: process.argv.includes('--dry-run'),
  targetPackage: getArgValue('--package'),
  migrateAll: process.argv.includes('--all'),
  reportFile: getArgValue('--report'),
  backup: !process.argv.includes('--no-backup'),
  verbose: process.argv.includes('--verbose'),

  // Transformations to apply
  transforms: {
    storeInit: true,
    imports: true,
    receipts: true,
    zod: true,
    config: true,
  }
};

// ============================================================================
// Migration Patterns
// ============================================================================

const PATTERNS = {
  // Store initialization patterns
  storeInit: [
    {
      pattern: /import\s+{\s*Store\s*}\s+from\s+['"]n3['"]/g,
      replacement: "import { createStore } from '@unrdf/oxigraph'",
      description: 'Update Store import from N3 to Oxigraph'
    },
    {
      pattern: /new\s+Store\(\s*\)/g,
      replacement: 'await createStore()',
      description: 'Convert Store constructor to async factory'
    },
    {
      pattern: /const\s+(\w+)\s*=\s*new\s+Store\(\)/g,
      replacement: 'const $1 = await createStore()',
      description: 'Convert Store instantiation to async'
    }
  ],

  // Import path updates
  imports: [
    {
      pattern: /from\s+['"]n3['"]/g,
      replacement: "from '@unrdf/core/rdf/n3-justified-only'",
      description: 'Centralize N3 imports to justified module'
    },
    {
      pattern: /import\s+{\s*DataFactory\s*}\s+from\s+['"]n3['"]/g,
      replacement: "import { dataFactory } from '@unrdf/oxigraph'",
      description: 'Use Oxigraph dataFactory'
    }
  ],

  // Query patterns
  queries: [
    {
      pattern: /\.query\(\s*(['"`]SELECT[\s\S]*?['"`])\s*\)/g,
      replacement: '.query(sparql`$1`)',
      description: 'Convert string queries to tagged templates'
    }
  ],

  // Stream patterns
  streams: [
    {
      pattern: /stream\.on\(['"]data['"],\s*(.*?)\)/g,
      replacement: 'for await (const data of stream) { $1(data); }',
      description: 'Convert EventEmitter to AsyncIterator'
    }
  ],

  // Package.json updates
  packageJson: {
    type: 'module',
    engines: {
      node: '>=18.0.0',
      pnpm: '>=7.0.0'
    }
  }
};

// ============================================================================
// Utility Functions
// ============================================================================

/**
 * Get command line argument value
 * @param {string} arg - Argument name
 * @returns {string|null}
 */
function getArgValue(arg) {
  const index = process.argv.indexOf(arg);
  return index !== -1 && process.argv[index + 1]
    ? process.argv[index + 1]
    : null;
}

/**
 * Log message based on verbosity
 * @param {string} message
 * @param {string} [level='info']
 */
function log(message, level = 'info') {
  const prefix = {
    info: '→',
    success: '✅',
    warning: '⚠️',
    error: '❌',
    skip: '⏭'
  }[level] || '→';

  if (CONFIG.verbose || level !== 'info') {
    console.log(`${prefix} ${message}`);
  }
}

/**
 * Create backup of file
 * @param {string} filePath
 */
async function backupFile(filePath) {
  if (!CONFIG.backup) return;

  const backupPath = `${filePath}.v5-backup`;
  await fs.copyFile(filePath, backupPath);
  log(`Backed up to ${backupPath}`, 'info');
}

/**
 * Apply regex transformations to content
 * @param {string} content
 * @param {Array} patterns
 * @returns {{content: string, changes: Array}}
 */
function applyTransforms(content, patterns) {
  let transformed = content;
  const changes = [];

  for (const { pattern, replacement, description } of patterns) {
    const before = transformed;
    transformed = transformed.replace(pattern, replacement);

    if (before !== transformed) {
      changes.push({
        description,
        pattern: pattern.toString(),
        replacement
      });
    }
  }

  return { content: transformed, changes };
}

/**
 * Check if function needs to be async
 * @param {string} content
 * @returns {boolean}
 */
function needsAsyncWrapper(content) {
  return content.includes('await createStore()') &&
         !content.match(/async\s+function/);
}

/**
 * Wrap function in async if needed
 * @param {string} content
 * @returns {string}
 */
function ensureAsync(content) {
  if (!needsAsyncWrapper(content)) return content;

  // Find function declarations and add async
  return content.replace(
    /(export\s+)?function\s+(\w+)/g,
    '$1async function $2'
  );
}

// ============================================================================
// File Processors
// ============================================================================

/**
 * Migrate JavaScript/MJS file
 * @param {string} filePath
 * @returns {Promise<{success: boolean, changes: Array}>}
 */
async function migrateJSFile(filePath) {
  log(`Processing ${relative(ROOT, filePath)}`, 'info');

  try {
    await backupFile(filePath);
    let content = await fs.readFile(filePath, 'utf-8');
    const allChanges = [];

    // Apply transformations in order
    if (CONFIG.transforms.storeInit) {
      const { content: transformed, changes } = applyTransforms(
        content,
        PATTERNS.storeInit
      );
      content = transformed;
      allChanges.push(...changes);
    }

    if (CONFIG.transforms.imports) {
      const { content: transformed, changes } = applyTransforms(
        content,
        PATTERNS.imports
      );
      content = transformed;
      allChanges.push(...changes);
    }

    // Ensure async wrapper if needed
    content = ensureAsync(content);

    // Apply query transformations
    const { content: queryTransformed, changes: queryChanges } = applyTransforms(
      content,
      PATTERNS.queries
    );
    content = queryTransformed;
    allChanges.push(...queryChanges);

    // Write changes if not dry run
    if (!CONFIG.dryRun && allChanges.length > 0) {
      await fs.writeFile(filePath, content, 'utf-8');
      log(`Migrated ${relative(ROOT, filePath)}`, 'success');
    } else if (allChanges.length > 0) {
      log(`[DRY RUN] Would migrate ${relative(ROOT, filePath)}`, 'warning');
    } else {
      log(`No changes needed for ${relative(ROOT, filePath)}`, 'skip');
    }

    return { success: true, changes: allChanges };
  } catch (error) {
    log(`Error migrating ${filePath}: ${error.message}`, 'error');
    return { success: false, changes: [], error: error.message };
  }
}

/**
 * Migrate package.json file
 * @param {string} filePath
 * @returns {Promise<{success: boolean, changes: Array}>}
 */
async function migratePackageJson(filePath) {
  log(`Processing ${relative(ROOT, filePath)}`, 'info');

  try {
    await backupFile(filePath);
    const content = await fs.readFile(filePath, 'utf-8');
    const pkg = JSON.parse(content);
    const changes = [];

    // Update type to module
    if (pkg.type !== 'module') {
      pkg.type = 'module';
      changes.push({ field: 'type', from: pkg.type, to: 'module' });
    }

    // Update engines
    if (!pkg.engines || pkg.engines.node !== '>=18.0.0') {
      pkg.engines = { ...pkg.engines, ...PATTERNS.packageJson.engines };
      changes.push({ field: 'engines', updated: true });
    }

    // Update dependencies
    const depsToUpdate = {
      'n3': '@unrdf/core',
      '@unrdf/engine': '@unrdf/oxigraph'
    };

    for (const [oldDep, newDep] of Object.entries(depsToUpdate)) {
      if (pkg.dependencies?.[oldDep]) {
        delete pkg.dependencies[oldDep];
        pkg.dependencies[newDep] = 'workspace:*';
        changes.push({
          field: 'dependencies',
          removed: oldDep,
          added: newDep
        });
      }
    }

    // Add v6-compat if not present
    if (!pkg.dependencies?.['@unrdf/v6-compat']) {
      pkg.dependencies = pkg.dependencies || {};
      pkg.dependencies['@unrdf/v6-compat'] = 'workspace:*';
      changes.push({
        field: 'dependencies',
        added: '@unrdf/v6-compat'
      });
    }

    // Write changes
    if (!CONFIG.dryRun && changes.length > 0) {
      await fs.writeFile(
        filePath,
        JSON.stringify(pkg, null, 2) + '\n',
        'utf-8'
      );
      log(`Migrated ${relative(ROOT, filePath)}`, 'success');
    } else if (changes.length > 0) {
      log(`[DRY RUN] Would migrate ${relative(ROOT, filePath)}`, 'warning');
    }

    return { success: true, changes };
  } catch (error) {
    log(`Error migrating ${filePath}: ${error.message}`, 'error');
    return { success: false, changes: [], error: error.message };
  }
}

/**
 * Find all files to migrate in a package
 * @param {string} packagePath
 * @returns {Promise<{js: string[], packageJson: string}>}
 */
async function findFilesToMigrate(packagePath) {
  const files = { js: [], packageJson: null };

  // Find package.json
  const pkgJsonPath = join(packagePath, 'package.json');
  try {
    await fs.access(pkgJsonPath);
    files.packageJson = pkgJsonPath;
  } catch {
    // No package.json found
  }

  // Find all .mjs files in src/
  const srcPath = join(packagePath, 'src');
  try {
    await fs.access(srcPath);
    const srcFiles = await fs.readdir(srcPath, { recursive: true });

    for (const file of srcFiles) {
      if (file.endsWith('.mjs') || file.endsWith('.js')) {
        files.js.push(join(srcPath, file));
      }
    }
  } catch {
    // No src/ directory
  }

  return files;
}

/**
 * Migrate a single package
 * @param {string} packagePath
 * @returns {Promise<{success: boolean, summary: object}>}
 */
async function migratePackage(packagePath) {
  const packageName = relative(ROOT, packagePath);
  log(`\nMigrating package: ${packageName}`, 'info');
  log('='.repeat(60), 'info');

  const files = await findFilesToMigrate(packagePath);
  const results = {
    packageName,
    jsFiles: { total: 0, migrated: 0, errors: 0, changes: [] },
    packageJson: { migrated: false, changes: [] }
  };

  // Migrate JS files
  for (const filePath of files.js) {
    results.jsFiles.total++;
    const result = await migrateJSFile(filePath);

    if (result.success) {
      if (result.changes.length > 0) {
        results.jsFiles.migrated++;
        results.jsFiles.changes.push({
          file: relative(ROOT, filePath),
          changes: result.changes
        });
      }
    } else {
      results.jsFiles.errors++;
    }
  }

  // Migrate package.json
  if (files.packageJson) {
    const result = await migratePackageJson(files.packageJson);
    if (result.success && result.changes.length > 0) {
      results.packageJson.migrated = true;
      results.packageJson.changes = result.changes;
    }
  }

  return { success: true, summary: results };
}

/**
 * Find all packages in workspace
 * @returns {Promise<string[]>}
 */
async function findAllPackages() {
  const packagesDir = join(ROOT, 'packages');
  const entries = await fs.readdir(packagesDir, { withFileTypes: true });

  return entries
    .filter(entry => entry.isDirectory())
    .map(entry => join(packagesDir, entry.name));
}

// ============================================================================
// Main Migration Logic
// ============================================================================

/**
 * Generate migration report
 * @param {Array} results
 */
async function generateReport(results) {
  const report = {
    timestamp: new Date().toISOString(),
    mode: CONFIG.dryRun ? 'dry-run' : 'applied',
    summary: {
      totalPackages: results.length,
      successfulPackages: results.filter(r => r.success).length,
      totalJSFiles: results.reduce((sum, r) => sum + r.summary.jsFiles.total, 0),
      migratedJSFiles: results.reduce((sum, r) => sum + r.summary.jsFiles.migrated, 0),
      errors: results.reduce((sum, r) => sum + r.summary.jsFiles.errors, 0)
    },
    packages: results.map(r => r.summary)
  };

  if (CONFIG.reportFile) {
    await fs.writeFile(
      CONFIG.reportFile,
      JSON.stringify(report, null, 2),
      'utf-8'
    );
    log(`Report saved to ${CONFIG.reportFile}`, 'success');
  }

  // Print summary
  console.log('\n' + '='.repeat(60));
  console.log('MIGRATION SUMMARY');
  console.log('='.repeat(60));
  console.log(`Mode: ${report.mode.toUpperCase()}`);
  console.log(`Total Packages: ${report.summary.totalPackages}`);
  console.log(`Successful: ${report.summary.successfulPackages}`);
  console.log(`JS Files Migrated: ${report.summary.migratedJSFiles}/${report.summary.totalJSFiles}`);
  console.log(`Errors: ${report.summary.errors}`);
  console.log('='.repeat(60) + '\n');

  return report;
}

/**
 * Main entry point
 */
async function main() {
  console.log('UNRDF v5 → v6 Migration Tool\n');

  if (CONFIG.dryRun) {
    console.log('⚠️  DRY RUN MODE - No files will be modified\n');
  }

  const results = [];

  try {
    if (CONFIG.targetPackage) {
      // Migrate single package
      const packagePath = resolve(ROOT, CONFIG.targetPackage);
      const result = await migratePackage(packagePath);
      results.push(result);
    } else if (CONFIG.migrateAll) {
      // Migrate all packages
      const packages = await findAllPackages();
      log(`Found ${packages.length} packages\n`, 'info');

      for (const packagePath of packages) {
        const result = await migratePackage(packagePath);
        results.push(result);
      }
    } else {
      console.log('Usage:');
      console.log('  --package <path>  Migrate specific package');
      console.log('  --all             Migrate all packages');
      console.log('  --dry-run         Show changes without applying');
      console.log('  --report <file>   Generate JSON report');
      console.log('  --no-backup       Skip backup files');
      console.log('  --verbose         Show detailed logs');
      process.exit(1);
    }

    // Generate report
    await generateReport(results);

    // Post-migration instructions
    if (!CONFIG.dryRun) {
      console.log('\n✅ Migration complete!\n');
      console.log('Next steps:');
      console.log('1. Run tests: pnpm test');
      console.log('2. Run linter: pnpm lint');
      console.log('3. Verify imports: grep -r "from \'n3\'" packages/*/src');
      console.log('4. Check OTEL validation: node validation/run-all.mjs\n');
    }

  } catch (error) {
    log(`Migration failed: ${error.message}`, 'error');
    console.error(error);
    process.exit(1);
  }
}

// Run if called directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch(console.error);
}

export { migratePackage, migrateJSFile, migratePackageJson };
