#!/usr/bin/env node
/**
 * @file Manual Test Script for Sync Command
 * @description End-to-end manual test for the sync command pipeline
 *
 * Prerequisites:
 *   - Run from project root: /home/user/unrdf
 *   - Dependencies installed: pnpm install
 *
 * Usage:
 *   node packages/cli/test/manual/test-sync.mjs
 *
 * This script:
 * 1. Creates a temp directory with test files
 * 2. Runs the sync command
 * 3. Verifies the output
 * 4. Cleans up
 *
 * The script is self-contained with inline test data (ontology, template, config)
 * and handles all file creation and cleanup automatically.
 */

import { writeFile, mkdir, rm, readFile, access } from 'fs/promises';
import { join } from 'path';
import { tmpdir } from 'os';

// Check if dependencies are installed before importing
let runSync;
try {
  const orchestrator = await import('../../src/cli/commands/sync/orchestrator.mjs');
  runSync = orchestrator.runSync;
} catch (err) {
  console.error('\x1b[31mError: Dependencies not installed\x1b[0m\n');
  console.error('Please run the following commands first:\n');
  console.error('  cd /home/user/unrdf');
  console.error('  pnpm install\n');
  console.error('Then run this test again:\n');
  console.error('  node packages/cli/test/manual/test-sync.mjs\n');
  console.error(`Original error: ${err.message}`);
  process.exit(1);
}

// =============================================================================
// Test Data
// =============================================================================

/**
 * Minimal test ontology in Turtle format
 */
const TEST_ONTOLOGY = `
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix ex: <http://example.org/schema#> .

# User entity
ex:User a owl:Class ;
    rdfs:label "User" ;
    rdfs:comment "A user in the system" .

ex:username a owl:DatatypeProperty ;
    rdfs:domain ex:User ;
    rdfs:range xsd:string ;
    rdfs:label "username" ;
    rdfs:comment "The user's unique username" .

ex:email a owl:DatatypeProperty ;
    rdfs:domain ex:User ;
    rdfs:range xsd:string ;
    rdfs:label "email" ;
    rdfs:comment "The user's email address" .

# Post entity
ex:Post a owl:Class ;
    rdfs:label "Post" ;
    rdfs:comment "A blog post" .

ex:title a owl:DatatypeProperty ;
    rdfs:domain ex:Post ;
    rdfs:range xsd:string ;
    rdfs:label "title" ;
    rdfs:comment "The post title" .

ex:content a owl:DatatypeProperty ;
    rdfs:domain ex:Post ;
    rdfs:range xsd:string ;
    rdfs:label "content" ;
    rdfs:comment "The post content" .
`;

/**
 * Simple Nunjucks template
 */
const TEST_TEMPLATE = `---
to: entities.mjs
description: Generated entity summary from RDF ontology
---
/**
 * @file Entity Summary
 * @generated {{ now | date("YYYY-MM-DD") }}
 * @description Auto-generated from RDF ontology
 */

// Entity count: {{ sparql_results | length }}

{% for row in sparql_results %}
/** {{ row.label }} - {{ row.comment | default("No description") }} */
export const {{ row.label | upper }} = '{{ row.entity }}';
{% endfor %}

export const ALL_ENTITIES = [
{% for row in sparql_results %}
  {{ row.label | upper }},
{% endfor %}
];
`;

/**
 * SPARQL query to extract classes
 */
const SPARQL_QUERY = `
SELECT ?entity ?label ?comment
WHERE {
  ?entity a owl:Class .
  OPTIONAL { ?entity rdfs:label ?label }
  OPTIONAL { ?entity rdfs:comment ?comment }
}
ORDER BY ?label
`;

// =============================================================================
// Test Functions
// =============================================================================

/**
 * Generates ggen.toml configuration
 */
function generateConfig(ontologyPath, templatePath, outputDir) {
  return `
[project]
name = "manual-test-sync"
version = "1.0.0"
description = "Manual test for sync command"

[ontology]
source = "${ontologyPath}"
format = "turtle"

[generation]
output_dir = "${outputDir}"

[[generation.rules]]
name = "entities"
description = "Generate entity constants"
query = """${SPARQL_QUERY}"""
template = "${templatePath}"
output_file = "entities.mjs"
enabled = true
`;
}

/**
 * Prints a section header
 */
function printSection(title) {
  console.log('\n' + '='.repeat(60));
  console.log(`  ${title}`);
  console.log('='.repeat(60) + '\n');
}

/**
 * Prints success message
 */
function printSuccess(message) {
  console.log('\x1b[32m✓\x1b[0m ' + message);
}

/**
 * Prints error message
 */
function printError(message) {
  console.log('\x1b[31m✗\x1b[0m ' + message);
}

/**
 * Prints info message
 */
function printInfo(message) {
  console.log('\x1b[36mℹ\x1b[0m ' + message);
}

/**
 * Main test function
 */
async function runTest() {
  let testDir;
  let success = true;

  try {
    printSection('Manual Sync Command Test');

    // Step 1: Create temp directory
    printInfo('Creating temporary test directory...');
    testDir = join(tmpdir(), `sync-manual-test-${Date.now()}`);
    await mkdir(testDir, { recursive: true });
    printSuccess(`Created: ${testDir}`);

    // Step 2: Create directory structure
    printInfo('Setting up test files...');
    const ontologyPath = join(testDir, 'schema.ttl');
    const templatePath = join(testDir, 'templates', 'entities.njk');
    const configPath = join(testDir, 'ggen.toml');
    const outputDir = join(testDir, 'generated');

    await mkdir(join(testDir, 'templates'), { recursive: true });
    await mkdir(outputDir, { recursive: true });

    // Step 3: Write test files
    await writeFile(ontologyPath, TEST_ONTOLOGY);
    printSuccess('Created ontology: schema.ttl');

    await writeFile(templatePath, TEST_TEMPLATE);
    printSuccess('Created template: templates/entities.njk');

    const config = generateConfig(ontologyPath, templatePath, outputDir);
    await writeFile(configPath, config);
    printSuccess('Created config: ggen.toml');

    // Step 4: Run sync command
    printSection('Running Sync Command');
    printInfo('Executing runSync()...');

    const startTime = Date.now();
    const result = await runSync({
      config: configPath,
      dryRun: false,
      verbose: true,
    });
    const duration = Date.now() - startTime;

    // Step 5: Check result
    printSection('Sync Results');

    if (result.success) {
      printSuccess(`Sync completed successfully in ${duration}ms`);
      printInfo(`Rules processed: ${result.metrics.rulesProcessed}`);
      printInfo(`Files generated: ${result.metrics.filesGenerated}`);
      printInfo(`Total bytes: ${result.metrics.totalBytes}`);
      printInfo(`Errors: ${result.metrics.errors}`);
    } else {
      printError('Sync failed');
      printError(`Error: ${result.error}`);
      success = false;
    }

    // Step 6: Verify output file
    printSection('Verifying Output');

    const outputPath = join(outputDir, 'entities.mjs');

    try {
      await access(outputPath);
      printSuccess('Output file exists: entities.mjs');

      const content = await readFile(outputPath, 'utf-8');

      // Check for expected content patterns
      const checks = [
        { pattern: '@file Entity Summary', name: 'JSDoc header' },
        { pattern: '@generated', name: 'Generated timestamp' },
        { pattern: '// Entity count:', name: 'Entity count comment' },
        { pattern: 'export const USER', name: 'USER constant' },
        { pattern: 'export const POST', name: 'POST constant' },
        { pattern: 'export const ALL_ENTITIES', name: 'ALL_ENTITIES array' },
        { pattern: 'http://example.org/schema#User', name: 'User URI' },
        { pattern: 'http://example.org/schema#Post', name: 'Post URI' },
      ];

      let allChecksPassed = true;
      for (const check of checks) {
        if (content.includes(check.pattern)) {
          printSuccess(`Found: ${check.name}`);
        } else {
          printError(`Missing: ${check.name}`);
          allChecksPassed = false;
          success = false;
        }
      }

      if (allChecksPassed) {
        printSection('Generated Content Preview');
        console.log(content);
      }

    } catch (err) {
      printError(`Output file not found: ${err.message}`);
      success = false;
    }

  } catch (err) {
    printError(`Test failed with error: ${err.message}`);
    if (err.stack) {
      console.error('\nStack trace:');
      console.error(err.stack);
    }
    success = false;
  } finally {
    // Cleanup
    if (testDir) {
      try {
        printSection('Cleanup');
        printInfo('Removing temporary directory...');
        await rm(testDir, { recursive: true, force: true });
        printSuccess('Cleanup complete');
      } catch (err) {
        printError(`Cleanup failed: ${err.message}`);
      }
    }
  }

  // Final result
  printSection('Test Summary');
  if (success) {
    printSuccess('ALL TESTS PASSED');
    process.exit(0);
  } else {
    printError('SOME TESTS FAILED');
    process.exit(1);
  }
}

// =============================================================================
// Execute
// =============================================================================

runTest().catch((err) => {
  console.error('\nUnexpected error:', err);
  process.exit(1);
});
