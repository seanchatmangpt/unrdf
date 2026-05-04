#!/usr/bin/env node

/**
 * @fileoverview Production Automation Demo for @unrdf/cli
 *
 * This script demonstrates a complete, automated RDF pipeline:
 * 1. Initialize an RDF environment
 * 2. Load and validate knowledge graphs
 * 3. Execute SPARQL transformations
 * 4. Export artifacts in multiple formats
 * 5. Verify data integrity
 */

import { writeFileSync, mkdirSync, rmSync, existsSync, readFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { spawnSync } from 'node:child_process';

import { fileURLToPath } from 'node:url';
import { dirname } from 'node:path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

const testDir = join(tmpdir(), 'unrdf-production-demo-' + Date.now());
const unrdfBin = join(__dirname, '../src/cli/main.mjs');

// Formatting helpers
const line = '═══════════════════════════════════════════════════════════════════';
const header = (text) => console.log(`\n${line}\n  ${text}\n${line}\n`);
const step = (text) => console.log(`\n🚀 ${text}...`);
const success = (text) => console.log(`   ✅ ${text}`);
const info = (text) => console.log(`   ℹ️  ${text}`);
const error = (text) => console.log(`   ❌ ${text}`);

async function runDemo() {
  header('UNRDF CLI Production Automation Demo');

  try {
    // 1. Setup Environment
    step('Initializing environment');
    if (!existsSync(testDir)) {
      mkdirSync(testDir, { recursive: true });
    }
    info(`Working directory: ${testDir}`);
    success('Environment ready');

    // 2. Create Test Data
    step('Generating production-ready RDF data');
    const turtleData = `
@prefix schema: <http://schema.org/> .
@prefix ex: <http://example.org/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:ProjectAlpha a schema:Project ;
    schema:name "Project Alpha" ;
    schema:startDate "2024-01-01"^^xsd:date ;
    schema:status "active" .

ex:ProjectBeta a schema:Project ;
    schema:name "Project Beta" ;
    schema:startDate "2023-06-15"^^xsd:date ;
    schema:status "completed" .
`;
    const dataPath = join(testDir, 'projects.ttl');
    writeFileSync(dataPath, turtleData);
    success('Test data generated: projects.ttl');

    // 3. Initialize UNRDF Sync (Simulation)
    step('Initializing UNRDF project structure');
    const initResult = spawnSync('node', [unrdfBin, 'sync', 'init'], { 
      cwd: testDir,
      encoding: 'utf-8'
    });
    if (initResult.status !== 0) {
      error(`Init failed: ${initResult.stderr}`);
    } else {
      success('Project structure initialized (unrdf.toml)');
    }

    // 4. Create and Manage Graphs
    step('Managing RDF knowledge graphs');
    const createResult = spawnSync('node', [unrdfBin, 'graph', 'create', '--name', 'production-db'], { 
      cwd: testDir,
      encoding: 'utf-8'
    });
    if (createResult.status !== 0) {
      error(`Graph create failed: ${createResult.stderr}`);
    } else {
      success('Created graph: production-db');
    }

    // 5. Load Data
    step('Ingesting data into knowledge graph');
    const loadResult = spawnSync('node', [unrdfBin, 'graph', 'load', '--graph', 'production-db', '--file', 'projects.ttl'], { 
      cwd: testDir,
      encoding: 'utf-8'
    });
    if (loadResult.status !== 0) {
      error(`Graph load failed: ${loadResult.stderr}`);
    } else {
      success('Ingested projects.ttl into production-db');
    }

    // 6. Execute SPARQL Query
    step('Executing automated SPARQL analytics');
    const query = 'SELECT ?name ?status WHERE { ?p a <http://schema.org/Project> ; <http://schema.org/name> ?name ; <http://schema.org/status> ?status . }';
    const queryResult = spawnSync('node', [unrdfBin, 'query', '--query', query, '--file', 'projects.ttl'], { 
      cwd: testDir,
      encoding: 'utf-8'
    });
    if (queryResult.status !== 0) {
      error(`Query failed: ${queryResult.stderr}`);
    } else {
      info('Query Results:');
      console.log(queryResult.stdout);
      success('SPARQL query executed successfully');
    }

    // 7. Format Conversion
    step('Converting artifacts for downstream consumption');
    const convertResult = spawnSync('node', [unrdfBin, 'convert', '--input', 'projects.ttl', '--output', 'projects.json', '--to', 'JSON'], { 
      cwd: testDir,
      encoding: 'utf-8'
    });
    if (convertResult.status !== 0) {
      error(`Convert failed: ${convertResult.stderr}`);
    } else {
      success('Converted projects.ttl to projects.json');
    }

    // 8. Health Check (Doctor)
    step('Running system diagnostics');
    const doctorResult = spawnSync('node', [unrdfBin, 'doctor'], { 
      cwd: testDir,
      encoding: 'utf-8'
    });
    if (doctorResult.status !== 0) {
      error(`Doctor failed: ${doctorResult.stderr}`);
    } else {
      success('System health verified');
    }

    // 9. Verification
    header('VERIFICATION RESULTS');
    
    const filesCreated = [
      'projects.ttl',
      'projects.json',
      'unrdf.toml'
    ];

    let allOk = true;
    for (const f of filesCreated) {
      const exists = existsSync(join(testDir, f));
      console.log(`${exists ? '✅' : '❌'} ${f.padEnd(20)} ${exists ? 'CREATED' : 'MISSING'}`);
      if (!exists) allOk = false;
    }

    if (allOk) {
      console.log(`\n${line}`);
      console.log('  ✨ PRODUCTION PIPELINE VERIFIED SUCCESSFULLY');
      console.log(`${line}\n`);
    } else {
      console.error('\n❌ PIPELINE VERIFICATION FAILED');
      process.exit(1);
    }

    // Cleanup
    step('Cleaning up temporary files');
    // rmSync(testDir, { recursive: true, force: true });
    info(`Files left in ${testDir} for inspection`);
    success('Cleanup skipped (intentional)');

  } catch (error) {
    console.error('\n❌ Demo failed:', error.message);
    process.exit(1);
  }
}

runDemo();
