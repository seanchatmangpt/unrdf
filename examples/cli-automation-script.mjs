#!/usr/bin/env node

/**
 * CLI Automation Script Example
 *
 * Demonstrates how to automate UNRDF CLI operations for:
 * - Batch processing RDF files
 * - Automated validation workflows
 * - CI/CD integration
 * - Data quality monitoring
 */

import { exec } from 'node:child_process';
import { promisify } from 'node:util';
import { writeFile, readdir, mkdir } from 'node:fs/promises';
import { resolve } from 'node:path';

const execAsync = promisify(exec);

console.log('=== UNRDF CLI Automation Examples ===\n');

// Example 1: Batch Validation
console.log('1. Batch Validation of RDF Files\n');

// Create test files
const testDir = resolve(process.cwd(), 'test-data');
await mkdir(testDir, { recursive: true });

const testFiles = [
  {
    name: 'valid-data.ttl',
    content: `
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:Alice a foaf:Person ;
  foaf:name "Alice Smith" ;
  foaf:email "alice@example.org" .
    `,
  },
  {
    name: 'invalid-data.ttl',
    content: `
@prefix ex: <http://example.org/> .
ex:Bob a ex:Person  # Missing semicolon
ex:name "Bob"
    `,
  },
];

for (const file of testFiles) {
  await writeFile(resolve(testDir, file.name), file.content, 'utf-8');
}

console.log(`✓ Created ${testFiles.length} test files\n`);

// Batch validate all files
console.log('Running batch validation...');

async function batchValidate(directory) {
  const files = await readdir(directory);
  const results = [];

  for (const file of files) {
    if (file.endsWith('.ttl')) {
      const filePath = resolve(directory, file);

      try {
        const { stdout } = await execAsync(`npx unrdf parse ${filePath}`);
        results.push({
          file,
          status: 'valid',
          output: stdout.trim(),
        });
        console.log(`  ✓ ${file}: VALID`);
      } catch (error) {
        results.push({
          file,
          status: 'invalid',
          error: error.message,
        });
        console.log(`  ✗ ${file}: INVALID`);
      }
    }
  }

  return results;
}

const validationResults = await batchValidate(testDir);
console.log(
  `\nValidation complete: ${validationResults.filter(r => r.status === 'valid').length}/${validationResults.length} files valid\n`
);

// Example 2: Automated Data Quality Checks
console.log('2. Automated Data Quality Checks\n');

// Create SHACL shapes
const shapesContent = `
@prefix ex: <http://example.org/> .
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:PersonShape a sh:NodeShape ;
  sh:targetClass foaf:Person ;
  sh:property [
    sh:path foaf:name ;
    sh:datatype xsd:string ;
    sh:minCount 1 ;
    sh:message "Person must have a name" ;
  ] ;
  sh:property [
    sh:path foaf:email ;
    sh:pattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\\\.[a-zA-Z]{2,}$" ;
    sh:message "Email must be valid" ;
  ] .
`;

const shapesPath = resolve(testDir, 'shapes.ttl');
await writeFile(shapesPath, shapesContent, 'utf-8');

console.log('Running SHACL validation...');

async function validateWithShapes(dataFile, shapesFile) {
  try {
    const { stdout } = await execAsync(
      `npx unrdf graph validate ${dataFile} --shapes ${shapesFile} --output json`
    );
    return JSON.parse(stdout);
  } catch (error) {
    console.error(`Validation failed: ${error.message}`);
    return { conforms: false, error: error.message };
  }
}

const dataPath = resolve(testDir, 'valid-data.ttl');
const shaclResult = await validateWithShapes(dataPath, shapesPath);

if (shaclResult.conforms) {
  console.log('✓ SHACL validation passed\n');
} else {
  console.log('✗ SHACL validation failed');
  console.log('Violations:', shaclResult.results);
  console.log();
}

// Example 3: CI/CD Pipeline Integration
console.log('3. CI/CD Pipeline Script\n');

async function cicdPipeline() {
  const steps = [
    {
      name: 'Lint RDF files',
      command: 'npx unrdf parse test-data/*.ttl',
    },
    {
      name: 'Validate with SHACL',
      command: `npx unrdf graph validate ${dataPath} --shapes ${shapesPath}`,
    },
    {
      name: 'Run quality checks',
      command: 'npx unrdf hook list --active',
    },
    {
      name: 'Generate report',
      command: 'npx unrdf graph stats test-data/valid-data.ttl --output json',
    },
  ];

  const results = [];

  for (const step of steps) {
    console.log(`Running: ${step.name}...`);

    try {
      const { stdout, stderr } = await execAsync(step.command);
      results.push({
        step: step.name,
        status: 'success',
        output: stdout || stderr,
      });
      console.log(`  ✓ ${step.name} passed`);
    } catch (error) {
      results.push({
        step: step.name,
        status: 'failed',
        error: error.message,
      });
      console.log(`  ✗ ${step.name} failed`);

      // Fail fast
      console.log('\n✗ Pipeline failed at step:', step.name);
      return { success: false, results };
    }
  }

  console.log('\n✓ All pipeline steps passed\n');
  return { success: true, results };
}

const pipelineResult = await cicdPipeline();

// Save pipeline results
const resultsPath = resolve(testDir, 'pipeline-results.json');
await writeFile(resultsPath, JSON.stringify(pipelineResult, null, 2), 'utf-8');
console.log(`✓ Pipeline results saved to ${resultsPath}\n`);

// Example 4: Data Migration Script
console.log('4. Data Migration Automation\n');

async function _migrateData(sourceFormat, targetFormat) {
  const sourceFile = resolve(testDir, `data.${sourceFormat}`);
  const targetFile = resolve(testDir, `data.${targetFormat}`);

  console.log(`Migrating from ${sourceFormat} to ${targetFormat}...`);

  try {
    // Convert format
    await execAsync(
      `npx unrdf graph export ${sourceFile} --format ${targetFormat} --output ${targetFile}`
    );

    console.log(`✓ Migrated to ${targetFile}\n`);
    return true;
  } catch (error) {
    console.error(`Migration failed: ${error.message}\n`);
    return false;
  }
}

// Example 5: Monitoring Script
console.log('5. Data Quality Monitoring\n');

async function monitorDataQuality() {
  const monitoringResults = {
    timestamp: new Date().toISOString(),
    checks: [],
  };

  // Check 1: File count
  const files = await readdir(testDir);
  const rdfFiles = files.filter(f => f.endsWith('.ttl'));

  monitoringResults.checks.push({
    name: 'RDF File Count',
    value: rdfFiles.length,
    threshold: 1,
    status: rdfFiles.length >= 1 ? 'pass' : 'fail',
  });

  // Check 2: SHACL compliance
  try {
    const result = await validateWithShapes(dataPath, shapesPath);
    monitoringResults.checks.push({
      name: 'SHACL Compliance',
      value: result.conforms,
      status: result.conforms ? 'pass' : 'fail',
    });
  } catch (error) {
    monitoringResults.checks.push({
      name: 'SHACL Compliance',
      value: false,
      status: 'error',
      error: error.message,
    });
  }

  // Check 3: Triple count
  try {
    const { stdout } = await execAsync(`npx unrdf graph stats ${dataPath} --output json`);
    const stats = JSON.parse(stdout);

    monitoringResults.checks.push({
      name: 'Triple Count',
      value: stats.quadCount,
      threshold: 1,
      status: stats.quadCount >= 1 ? 'pass' : 'fail',
    });
  } catch (error) {
    monitoringResults.checks.push({
      name: 'Triple Count',
      value: 0,
      status: 'error',
    });
  }

  return monitoringResults;
}

const monitoringResults = await monitorDataQuality();
console.log('Monitoring results:');
console.log(JSON.stringify(monitoringResults, null, 2));
console.log();

// Check if all monitoring passed
const allPassed = monitoringResults.checks.every(c => c.status === 'pass');
console.log(allPassed ? '✓ All quality checks passed\n' : '✗ Some quality checks failed\n');

// Example 6: Context Switching Automation
console.log('6. Multi-Environment Context Management\n');

const environments = [
  { name: 'dev', sidecar: 'localhost:50051' },
  { name: 'staging', sidecar: 'staging.example.com:50051' },
  { name: 'prod', sidecar: 'prod.example.com:50051' },
];

async function _setupContexts() {
  for (const env of environments) {
    console.log(`Setting up ${env.name} context...`);

    try {
      await execAsync(`npx unrdf context create ${env.name} --sidecar ${env.sidecar}`);
      console.log(`  ✓ Created ${env.name} context`);
    } catch (error) {
      console.log(`  ⚠ Context ${env.name} may already exist`);
    }
  }

  console.log();
}

async function _switchContext(contextName) {
  console.log(`Switching to ${contextName} context...`);

  try {
    await execAsync(`npx unrdf context use ${contextName}`);
    console.log(`✓ Switched to ${contextName}\n`);
    return true;
  } catch (error) {
    console.error(`Failed to switch context: ${error.message}\n`);
    return false;
  }
}

// await setupContexts();
// await switchContext('dev');

console.log('✓ All automation examples complete!\n');

console.log('=== Summary ===\n');
console.log('These scripts demonstrate:');
console.log('  1. Batch validation of RDF files');
console.log('  2. Automated SHACL quality checks');
console.log('  3. CI/CD pipeline integration');
console.log('  4. Data format migration');
console.log('  5. Continuous quality monitoring');
console.log('  6. Multi-environment management');
console.log();
console.log('Use these patterns in your own automation scripts!\n');

/**
 * Integration with CI/CD Systems:
 *
 * GitHub Actions:
 * ```yaml
 * name: RDF Validation
 * on: [push, pull_request]
 * jobs:
 *   validate:
 *     runs-on: ubuntu-latest
 *     steps:
 *       - uses: actions/checkout@v2
 *       - name: Install UNRDF
 *         run: npm install -g unrdf
 *       - name: Validate RDF
 *         run: node cli-automation-script.mjs
 * ```
 *
 * GitLab CI:
 * ```yaml
 * validate:
 *   script:
 *     - npm install -g unrdf
 *     - node cli-automation-script.mjs
 * ```
 *
 * Jenkins:
 * ```groovy
 * stage('RDF Validation') {
 *   steps {
 *     sh 'npm install -g unrdf'
 *     sh 'node cli-automation-script.mjs'
 *   }
 * }
 * ```
 */
