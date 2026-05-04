#!/usr/bin/env node
/**
 * @fileoverview Extension ecosystem validation script.
 *
 * Validates:
 * - Contract compliance (Zod schema validation)
 * - Determinism (loadOrder uniqueness and ordering)
 * - Collision detection (noun:verb pairs)
 * - Code quality (async/await, error handling, schemas)
 */

import { Registry, ExtensionSchema } from './src/lib/registry.mjs';
import { extensions, loadManifest } from './src/manifest/extensions.mjs';
import { z } from 'zod';

/**
 * Color output for terminal
 */
const colors = {
  reset: '\x1b[0m',
  red: '\x1b[31m',
  green: '\x1b[32m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  cyan: '\x1b[36m'
};

function log(color, symbol, message) {
  console.log(`${color}${symbol}${colors.reset} ${message}`);
}

/**
 * Validation results accumulator
 */
const results = {
  passed: 0,
  failed: 0,
  warnings: 0,
  errors: []
};

/**
 * 1. Validate LoadOrder Determinism
 */
function validateLoadOrder() {
  log(colors.blue, '📋', 'Validating LoadOrder determinism...');

  const loadOrders = extensions.map(e => e.loadOrder);
  const uniqueOrders = new Set(loadOrders);

  // Check for duplicates
  if (loadOrders.length !== uniqueOrders.size) {
    const duplicates = loadOrders.filter((item, index) => loadOrders.indexOf(item) !== index);
    results.errors.push(`Duplicate loadOrder values found: ${duplicates.join(', ')}`);
    log(colors.red, '❌', `Duplicate loadOrder values: ${duplicates.join(', ')}`);
    results.failed++;
    return false;
  }

  // Check for strict ascending order
  const sorted = [...loadOrders].sort((a, b) => a - b);
  const isStrictlyAscending = loadOrders.every((val, i) => val === sorted[i]);

  if (!isStrictlyAscending) {
    results.errors.push('LoadOrder is not strictly ascending');
    log(colors.red, '❌', 'LoadOrder is not strictly ascending');
    results.failed++;
    return false;
  }

  log(colors.green, '✅', `LoadOrder is strictly ordered: [${loadOrders.join(', ')}]`);
  log(colors.green, '✅', `Total unique loadOrders: ${uniqueOrders.size}`);
  results.passed++;
  return true;
}

/**
 * 2. Validate Manifest Integrity
 */
function validateManifest() {
  log(colors.blue, '📋', 'Validating manifest integrity...');

  const expectedFields = ['id', 'path', 'loadOrder', 'enabled'];
  let allValid = true;

  for (const entry of extensions) {
    for (const field of expectedFields) {
      if (!(field in entry)) {
        results.errors.push(`Extension ${entry.id || 'unknown'} missing field: ${field}`);
        log(colors.red, '❌', `Missing field '${field}' in ${entry.id || 'unknown'}`);
        results.failed++;
        allValid = false;
      }
    }
  }

  if (allValid) {
    log(colors.green, '✅', `All ${extensions.length} manifest entries have required fields`);
    results.passed++;
  }

  return allValid;
}

/**
 * 3. Load extensions and validate contracts
 */
async function validateContracts() {
  log(colors.blue, '📋', 'Validating extension contracts...');

  const registry = new Registry({ failOnCollision: false });

  let loadedCount = 0;
  let failedCount = 0;

  for (const entry of extensions) {
    if (!entry.enabled) continue;

    try {
      const module = await import(entry.path);
      const ext = module.default || module.extension;

      if (!ext) {
        throw new Error(`No default export found in ${entry.path}`);
      }

      // Validate against ExtensionSchema
      const validation = ExtensionSchema.safeParse(ext);
      if (!validation.success) {
        results.errors.push(`${entry.id}: ${validation.error.message}`);
        log(colors.red, '❌', `${entry.id}: Contract validation failed`);
        log(colors.red, '  ', validation.error.message);
        failedCount++;
        results.failed++;
        continue;
      }

      // Register extension
      registry.registerExtension(ext, entry.loadOrder);
      loadedCount++;

    } catch (e) {
      results.errors.push(`${entry.id}: ${e.message}`);
      log(colors.red, '❌', `${entry.id}: ${e.message}`);
      failedCount++;
      results.failed++;
    }
  }

  log(colors.green, '✅', `${loadedCount}/${extensions.length} extensions loaded successfully`);

  if (failedCount > 0) {
    log(colors.red, '❌', `${failedCount} extensions failed to load`);
  } else {
    results.passed++;
  }

  // Validate all contracts in registry
  const contractErrors = registry.validateContracts();
  if (contractErrors.length > 0) {
    log(colors.red, '❌', `Contract validation errors found:`);
    contractErrors.forEach(err => {
      log(colors.red, '  ', `${err.source}: ${err.noun}:${err.verb} - ${err.issue}`);
      results.errors.push(`${err.source}: ${err.noun}:${err.verb} - ${err.issue}`);
    });
    results.failed++;
  } else {
    log(colors.green, '✅', 'All contracts satisfy schema requirements');
    results.passed++;
  }

  return registry;
}

/**
 * 4. Build collision matrix
 */
function buildCollisionMatrix(registry) {
  log(colors.blue, '📋', 'Building collision matrix...');

  const commands = registry.listCommands();
  const collisionSummary = registry.getCollisionSummary();

  log(colors.cyan, '📊', `Total commands registered: ${commands.length}`);

  // Group by noun
  const nounGroups = {};
  for (const cmd of commands) {
    const [noun, verb] = cmd.split(':');
    if (!nounGroups[noun]) nounGroups[noun] = [];
    nounGroups[noun].push(verb);
  }

  console.log('\n' + colors.cyan + '📊 Command Tree:' + colors.reset);
  for (const [noun, verbs] of Object.entries(nounGroups)) {
    console.log(`  ${colors.blue}${noun}${colors.reset}: ${verbs.join(', ')}`);
  }

  // Check for collisions
  const collisions = Object.entries(collisionSummary);
  if (collisions.length > 0) {
    log(colors.yellow, '⚠️', `Found ${collisions.length} collision(s):`);
    for (const [key, claimants] of collisions) {
      const sources = claimants.map(c => c.ext.id).join(', ');
      log(colors.yellow, '  ', `${key}: claimed by [${sources}]`);
      results.warnings++;
    }
  } else {
    log(colors.green, '✅', 'No noun:verb collisions detected');
    results.passed++;
  }

  return { commands, nounGroups, collisions };
}

/**
 * 5. Analyze code quality
 */
async function analyzeCodeQuality() {
  log(colors.blue, '📋', 'Analyzing code quality...');

  let issuesFound = 0;

  for (const entry of extensions) {
    if (!entry.enabled) continue;

    try {
      const module = await import(entry.path);
      const ext = module.default || module.extension;

      // Check all handlers are async
      for (const [noun, nounData] of Object.entries(ext.nouns)) {
        for (const [verb, verbData] of Object.entries(nounData.verbs)) {
          const handler = verbData.handler;

          // Check if handler is a function
          if (typeof handler !== 'function') {
            log(colors.red, '❌', `${entry.id}:${noun}:${verb} - handler is not a function`);
            issuesFound++;
            continue;
          }

          // Check if handler is async (returns Promise)
          const isAsync = handler.constructor.name === 'AsyncFunction';
          if (!isAsync) {
            log(colors.yellow, '⚠️', `${entry.id}:${noun}:${verb} - handler is not async`);
            results.warnings++;
          }

          // Check if argsSchema is valid Zod schema (if present)
          if (verbData.argsSchema) {
            if (typeof verbData.argsSchema.parse !== 'function') {
              log(colors.red, '❌', `${entry.id}:${noun}:${verb} - argsSchema is not a valid Zod schema`);
              issuesFound++;
            }
          }
        }
      }
    } catch (e) {
      // Already logged in validateContracts
    }
  }

  if (issuesFound === 0) {
    log(colors.green, '✅', 'Code quality checks passed');
    results.passed++;
  } else {
    log(colors.red, '❌', `${issuesFound} code quality issues found`);
    results.failed++;
  }
}

/**
 * 6. Generate determinism proof
 */
function generateDeterminismProof(registry, collisionData) {
  log(colors.blue, '📋', 'Generating determinism proof...');

  const loadOrders = extensions.map(e => e.loadOrder);
  const uniqueOrders = new Set(loadOrders);
  const { collisions } = collisionData;

  console.log('\n' + colors.cyan + '═══════════════════════════════════════' + colors.reset);
  console.log(colors.cyan + '   DETERMINISM PROOF' + colors.reset);
  console.log(colors.cyan + '═══════════════════════════════════════' + colors.reset);

  console.log(`\n${colors.blue}Total Extensions:${colors.reset} ${extensions.length}`);
  console.log(`${colors.blue}Unique LoadOrders:${colors.reset} ${uniqueOrders.size}`);
  console.log(`${colors.blue}LoadOrder Range:${colors.reset} [${Math.min(...loadOrders)}, ${Math.max(...loadOrders)}]`);
  console.log(`${colors.blue}Collisions Found:${colors.reset} ${collisions.length}`);
  console.log(`${colors.blue}Override Rules:${colors.reset} 0`);

  const isFullyDeterministic =
    loadOrders.length === uniqueOrders.size && // No duplicates
    collisions.length === 0; // No unresolved collisions

  console.log('\n' + colors.cyan + '───────────────────────────────────────' + colors.reset);

  if (isFullyDeterministic) {
    log(colors.green, '✅', 'Registry is FULLY DETERMINISTIC');
    console.log(`${colors.green}  → LoadOrder is ≺-total (completely ordered)${colors.reset}`);
    console.log(`${colors.green}  → No ambiguous collision situations${colors.reset}`);
    console.log(`${colors.green}  → All commands have unique noun:verb paths${colors.reset}`);
    results.passed++;
  } else {
    log(colors.yellow, '⚠️', 'Registry has potential ambiguity');
    if (loadOrders.length !== uniqueOrders.size) {
      console.log(`${colors.yellow}  → Duplicate loadOrder values detected${colors.reset}`);
    }
    if (collisions.length > 0) {
      console.log(`${colors.yellow}  → ${collisions.length} collision(s) need resolution${colors.reset}`);
    }
    results.warnings++;
  }

  console.log(colors.cyan + '═══════════════════════════════════════' + colors.reset);

  return isFullyDeterministic;
}

/**
 * Main validation runner
 */
async function main() {
  console.log('\n' + colors.cyan + '╔═══════════════════════════════════════════════╗' + colors.reset);
  console.log(colors.cyan + '║  KGC CLI Extension Ecosystem Validation      ║' + colors.reset);
  console.log(colors.cyan + '╚═══════════════════════════════════════════════╝' + colors.reset);
  console.log('');

  // Run all validations
  validateLoadOrder();
  console.log('');

  validateManifest();
  console.log('');

  const registry = await validateContracts();
  console.log('');

  const collisionData = buildCollisionMatrix(registry);
  console.log('');

  await analyzeCodeQuality();
  console.log('');

  const isDeterministic = generateDeterminismProof(registry, collisionData);
  console.log('');

  // Final summary
  console.log(colors.cyan + '═══════════════════════════════════════' + colors.reset);
  console.log(colors.cyan + '   VALIDATION SUMMARY' + colors.reset);
  console.log(colors.cyan + '═══════════════════════════════════════' + colors.reset);
  console.log(`${colors.green}Passed:${colors.reset}   ${results.passed}`);
  console.log(`${colors.yellow}Warnings:${colors.reset} ${results.warnings}`);
  console.log(`${colors.red}Failed:${colors.reset}   ${results.failed}`);

  if (results.failed > 0) {
    console.log(`\n${colors.red}❌ VALIDATION FAILED${colors.reset}`);
    console.log(`\n${colors.red}Errors:${colors.reset}`);
    results.errors.forEach(err => console.log(`  - ${err}`));
    process.exit(1);
  } else if (results.warnings > 0) {
    console.log(`\n${colors.yellow}⚠️  VALIDATION PASSED WITH WARNINGS${colors.reset}`);
    process.exit(0);
  } else {
    console.log(`\n${colors.green}✅ ALL VALIDATIONS PASSED${colors.reset}`);
    process.exit(0);
  }
}

main().catch(e => {
  console.error(`${colors.red}Fatal error:${colors.reset}`, e);
  process.exit(1);
});
