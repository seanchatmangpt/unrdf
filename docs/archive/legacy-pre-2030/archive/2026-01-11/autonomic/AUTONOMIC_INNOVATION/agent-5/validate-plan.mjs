#!/usr/bin/env node

/**
 * Validation script for Agent 5 Commutativity Analysis Plan
 *
 * Checks:
 * - All required files documented
 * - Example capsules are valid JSON
 * - Plan completeness
 * - SPARC methodology coverage
 *
 * Usage: node validate-plan.mjs
 */

import { readFile } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import { dirname, join } from 'node:path';

const __dirname = dirname(fileURLToPath(import.meta.url));

// ANSI colors for output
const colors = {
  green: '\x1b[32m',
  red: '\x1b[31m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  reset: '\x1b[0m'
};

const log = {
  success: (msg) => console.log(`${colors.green}✅ ${msg}${colors.reset}`),
  error: (msg) => console.log(`${colors.red}❌ ${msg}${colors.reset}`),
  warn: (msg) => console.log(`${colors.yellow}⚠️  ${msg}${colors.reset}`),
  info: (msg) => console.log(`${colors.blue}ℹ️  ${msg}${colors.reset}`)
};

async function validateJSON(filePath) {
  try {
    const content = await readFile(join(__dirname, filePath), 'utf-8');
    JSON.parse(content);
    return true;
  } catch (error) {
    log.error(`Invalid JSON in ${filePath}: ${error.message}`);
    return false;
  }
}

async function validateMarkdown(filePath, requiredSections) {
  try {
    const content = await readFile(join(__dirname, filePath), 'utf-8');

    let allFound = true;
    for (const section of requiredSections) {
      if (!content.includes(section)) {
        log.error(`Missing section in ${filePath}: ${section}`);
        allFound = false;
      }
    }

    return allFound;
  } catch (error) {
    log.error(`Cannot read ${filePath}: ${error.message}`);
    return false;
  }
}

async function main() {
  log.info('Validating Agent 5 Commutativity Analysis Plan...\n');

  let allPassed = true;

  // 1. Validate PLAN.md exists and has SPARC sections
  log.info('Checking PLAN.md structure...');
  const planSections = [
    'SPARC Phase 1: Specification',
    'SPARC Phase 2: Pseudocode',
    'SPARC Phase 3: Architecture',
    'SPARC Phase 4: Refinement',
    'SPARC Phase 5: Completion',
    'canReorder',
    'conflictCertificate',
    'Adversarial PM Verification',
    'Mathematical Foundation'
  ];

  if (await validateMarkdown('PLAN.md', planSections)) {
    log.success('PLAN.md has all required SPARC sections');
  } else {
    allPassed = false;
  }

  // 2. Validate README.md
  log.info('\nChecking README.md...');
  const readmeSections = [
    'Key Functions',
    'Commutativity Rules',
    'Test Cases',
    'Implementation Checklist',
    'Performance Targets',
    'Adversarial PM Checks'
  ];

  if (await validateMarkdown('README.md', readmeSections)) {
    log.success('README.md is complete');
  } else {
    allPassed = false;
  }

  // 3. Validate example capsules
  log.info('\nValidating example capsules...');
  const examples = [
    'examples/disjoint-capsules.json',
    'examples/commutative-overlap.json',
    'examples/add-del-conflict.json',
    'examples/self-conflict.json'
  ];

  for (const example of examples) {
    if (await validateJSON(example)) {
      log.success(`${example} is valid JSON`);
    } else {
      allPassed = false;
    }
  }

  // 4. Check for required function signatures in PLAN.md
  log.info('\nChecking function signatures...');
  const plan = await readFile(join(__dirname, 'PLAN.md'), 'utf-8');

  const requiredFunctions = [
    'canReorder(capsuleA, capsuleB)',
    'conflictCertificate(capsuleA, capsuleB)',
    'findConflicts',
    'minimizeConflict',
    'canonicalize'
  ];

  for (const func of requiredFunctions) {
    if (plan.includes(func)) {
      log.success(`Function documented: ${func}`);
    } else {
      log.error(`Missing function: ${func}`);
      allPassed = false;
    }
  }

  // 5. Check for test case coverage
  log.info('\nChecking test case coverage...');
  const testCases = [
    'Disjoint Impact Sets',
    'Overlapping Subjects (Read-Only)',
    'Add-Delete Conflict',
    'Self-Conflict'
  ];

  for (const testCase of testCases) {
    if (plan.includes(testCase)) {
      log.success(`Test case defined: ${testCase}`);
    } else {
      log.error(`Missing test case: ${testCase}`);
      allPassed = false;
    }
  }

  // 6. Check for Adversarial PM questions
  log.info('\nChecking Adversarial PM verification...');
  const adversarialQuestions = [
    'Did I RUN',
    'Did I READ output',
    'Can I PROVE',
    'What BREAKS'
  ];

  let adversarialCount = 0;
  for (const question of adversarialQuestions) {
    if (plan.includes(question)) {
      adversarialCount++;
    }
  }

  if (adversarialCount >= 3) {
    log.success(`Adversarial PM questions present (${adversarialCount}/4)`);
  } else {
    log.warn(`Few Adversarial PM questions (${adversarialCount}/4)`);
  }

  // 7. Check for mathematical proofs
  log.info('\nChecking mathematical rigor...');
  if (plan.includes('Theorem') && plan.includes('Proof')) {
    log.success('Mathematical proofs included');
  } else {
    log.warn('No formal proofs found');
  }

  // 8. Summary
  console.log('\n' + '='.repeat(60));
  if (allPassed) {
    log.success('ALL VALIDATIONS PASSED');
    log.info('Plan is ready for implementation');
    process.exit(0);
  } else {
    log.error('SOME VALIDATIONS FAILED');
    log.warn('Review errors above before proceeding');
    process.exit(1);
  }
}

main().catch(error => {
  log.error(`Validation script failed: ${error.message}`);
  console.error(error);
  process.exit(1);
});
