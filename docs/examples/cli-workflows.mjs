#!/usr/bin/env node

/**
 * UNRDF CLI Workflow Examples
 * 
 * This file demonstrates practical CLI workflows and can be executed
 * to show real CLI operations in action.
 * 
 * @fileoverview Executable examples of UNRDF CLI workflows
 */

import { exec } from 'node:child_process';
import { promisify } from 'node:util';
import { writeFile, mkdir, readFile } from 'node:fs/promises';
import { resolve } from 'node:path';

const execAsync = promisify(exec);

/**
 * Setup example data for CLI demonstrations
 */
async function setupExampleData() {
  await mkdir('cli-examples', { recursive: true });
  
  // Sample RDF data
  const sampleData = `@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix ex: <http://example.org/> .
@prefix schema: <https://schema.org/> .

ex:alice a foaf:Person ;
    foaf:name "Alice Johnson" ;
    foaf:age 32 ;
    foaf:email "alice@example.com" ;
    foaf:knows ex:bob ;
    schema:jobTitle "Software Engineer" .

ex:bob a foaf:Person ;
    foaf:name "Bob Smith" ;
    foaf:age 28 ;
    foaf:email "bob@example.com" ;
    schema:jobTitle "Data Scientist" .

ex:company a schema:Organization ;
    schema:name "Tech Innovations Inc." ;
    schema:employee ex:alice, ex:bob .
`;
  
  await writeFile('cli-examples/people.ttl', sampleData);
  
  // SHACL shape for validation
  const shape = `@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

[] a sh:NodeShape ;
   sh:targetClass foaf:Person ;
   sh:property [
     sh:path foaf:name ;
     sh:datatype xsd:string ;
     sh:minCount 1 ;
   ] ;
   sh:property [
     sh:path foaf:age ;
     sh:datatype xsd:integer ;
     sh:minInclusive 0 ;
     sh:maxInclusive 150 ;
   ] .
`;
  
  await writeFile('cli-examples/person-shape.ttl', shape);
  
  // SPARQL queries
  const queries = {
    people: `PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX schema: <https://schema.org/>

SELECT ?name ?age ?job WHERE {
  ?person foaf:name ?name ;
          foaf:age ?age ;
          schema:jobTitle ?job .
} ORDER BY ?age`,
    
    connections: `PREFIX foaf: <http://xmlns.com/foaf/0.1/>

SELECT ?person1Name ?person2Name WHERE {
  ?person1 foaf:name ?person1Name ;
           foaf:knows ?person2 .
  ?person2 foaf:name ?person2Name .
}`,
    
    analytics: `PREFIX foaf: <http://xmlns.com/foaf/0.1/>

SELECT (AVG(?age) as ?avgAge) (COUNT(*) as ?totalPeople) WHERE {
  ?person foaf:age ?age .
}`
  };
  
  await mkdir('cli-examples/queries', { recursive: true });
  await writeFile('cli-examples/queries/people.sparql', queries.people);
  await writeFile('cli-examples/queries/connections.sparql', queries.connections);
  await writeFile('cli-examples/queries/analytics.sparql', queries.analytics);
  
  // Configuration file
  const config = `/**
 * UNRDF CLI Example Configuration
 */
export default {
  baseIRI: 'http://example.org/',
  prefixes: {
    'ex': 'http://example.org/',
    'foaf': 'http://xmlns.com/foaf/0.1/',
    'schema': 'https://schema.org/'
  },
  validation: {
    strict: true,
    validateOnLoad: true
  }
};
`;
  
  await writeFile('cli-examples/unrdf.config.mjs', config);
  
  console.log('✅ Example data setup complete');
}

/**
 * Demonstrate basic CLI operations
 */
async function demonstrateBasicOperations() {
  console.log('\n🔧 Basic CLI Operations');
  console.log('=' .repeat(50));
  
  try {
    // Parse with statistics
    console.log('\n📊 Parsing RDF data with statistics:');
    const { stdout: parseOutput } = await execAsync('unrdf parse cli-examples/people.ttl --stats');
    console.log(parseOutput);
    
    // List prefixes
    console.log('\n🏷️  Available prefixes:');
    const { stdout: prefixOutput } = await execAsync('cd cli-examples && unrdf prefix list');
    console.log(prefixOutput);
    
    // Generate some IDs
    console.log('\n🆔 Generated UUIDs:');
    const { stdout: idOutput } = await execAsync('unrdf id uuid --count 3');
    console.log(idOutput);
    
  } catch (error) {
    console.error('❌ Basic operations error:', error.message);
  }
}

/**
 * Demonstrate SPARQL querying
 */
async function demonstrateSPARQLQueries() {
  console.log('\n🔍 SPARQL Query Examples');
  console.log('=' .repeat(50));
  
  try {
    // People query with table format
    console.log('\n👥 People information (table format):');
    const { stdout: peopleTable } = await execAsync(
      'unrdf query cli-examples/people.ttl --query-file cli-examples/queries/people.sparql --format table'
    );
    console.log(peopleTable);
    
    // Connections query with JSON format
    console.log('\n🤝 Social connections (JSON format):');
    const { stdout: connectionsJson } = await execAsync(
      'unrdf query cli-examples/people.ttl --query-file cli-examples/queries/connections.sparql --format json'
    );
    console.log(connectionsJson);
    
    // Analytics query
    console.log('\n📈 Analytics (CSV format):');
    const { stdout: analyticsCSV } = await execAsync(
      'unrdf query cli-examples/people.ttl --query-file cli-examples/queries/analytics.sparql --format csv'
    );
    console.log(analyticsCSV);
    
  } catch (error) {
    console.error('❌ SPARQL query error:', error.message);
  }
}

/**
 * Demonstrate validation workflows
 */
async function demonstrateValidation() {
  console.log('\n✅ Validation Examples');
  console.log('=' .repeat(50));
  
  try {
    // Validate data against shape
    console.log('\n🔍 Validating RDF data against SHACL shape:');
    const { stdout: validationOutput } = await execAsync(
      'unrdf validate cli-examples/people.ttl --shape cli-examples/person-shape.ttl'
    );
    console.log(validationOutput);
    
    // Generate validation report
    console.log('\n📄 Generating validation report to file...');
    await execAsync(
      'unrdf validate cli-examples/people.ttl --shape cli-examples/person-shape.ttl --output cli-examples/validation-report.json'
    );
    
    const report = JSON.parse(await readFile('cli-examples/validation-report.json', 'utf-8'));
    console.log('Validation report generated:');
    console.log(`  - Conforms: ${report.conforms}`);
    console.log(`  - Violations: ${report.violations}`);
    
  } catch (error) {
    console.error('❌ Validation error:', error.message);
  }
}

/**
 * Demonstrate format conversion
 */
async function demonstrateConversion() {
  console.log('\n🔄 Format Conversion Examples');
  console.log('=' .repeat(50));
  
  try {
    // Convert to JSON-LD
    console.log('\n🔗 Converting Turtle to JSON-LD:');
    const { stdout: jsonldOutput } = await execAsync(
      'unrdf convert cli-examples/people.ttl --to json-ld --output cli-examples/people.jsonld'
    );
    console.log(jsonldOutput);
    
    // Convert to N-Quads
    console.log('\n📦 Converting to N-Quads:');
    const { stdout: nquadsOutput } = await execAsync(
      'unrdf convert cli-examples/people.ttl --to nquads --output cli-examples/people.nq'
    );
    console.log(nquadsOutput);
    
    // Show converted JSON-LD content (first few lines)
    console.log('\n📄 JSON-LD output (preview):');
    const jsonldContent = await readFile('cli-examples/people.jsonld', 'utf-8');
    console.log(JSON.stringify(JSON.parse(jsonldContent), null, 2).split('\n').slice(0, 15).join('\n') + '\n...');
    
  } catch (error) {
    console.error('❌ Conversion error:', error.message);
  }
}

/**
 * Demonstrate metrics and analysis
 */
async function demonstrateAnalytics() {
  console.log('\n📊 Analytics and Metrics');
  console.log('=' .repeat(50));
  
  try {
    // Basic metrics
    console.log('\n📈 Basic dataset metrics:');
    const { stdout: metricsOutput } = await execAsync(
      'unrdf metrics cli-examples/people.ttl'
    );
    console.log(metricsOutput);
    
    // Detailed metrics
    console.log('\n🔍 Detailed analytics:');
    const { stdout: detailedOutput } = await execAsync(
      'unrdf metrics cli-examples/people.ttl --detailed'
    );
    console.log(detailedOutput);
    
    // Store statistics
    console.log('\n🗄️  Store statistics:');
    const { stdout: storeStats } = await execAsync(
      'unrdf store stats cli-examples/people.ttl'
    );
    console.log(storeStats);
    
  } catch (error) {
    console.error('❌ Analytics error:', error.message);
  }
}

/**
 * Demonstrate a complete data processing workflow
 */
async function demonstrateWorkflow() {
  console.log('\n🔄 Complete Data Processing Workflow');
  console.log('=' .repeat(50));
  
  try {
    console.log('\nStep 1: Parse and validate input data...');
    await execAsync('unrdf parse cli-examples/people.ttl --stats > cli-examples/parse-report.txt');
    await execAsync('unrdf validate cli-examples/people.ttl --shape cli-examples/person-shape.ttl --output cli-examples/validation.json');
    console.log('✅ Input validated successfully');
    
    console.log('\nStep 2: Convert to multiple formats...');
    await execAsync('unrdf convert cli-examples/people.ttl --to json-ld --output cli-examples/output.jsonld');
    await execAsync('unrdf convert cli-examples/people.ttl --to nquads --output cli-examples/output.nq');
    console.log('✅ Format conversion completed');
    
    console.log('\nStep 3: Generate analytics report...');
    await execAsync('unrdf metrics cli-examples/people.ttl --detailed > cli-examples/analytics.txt');
    console.log('✅ Analytics generated');
    
    console.log('\nStep 4: Extract insights with SPARQL...');
    await execAsync('unrdf query cli-examples/people.ttl --query-file cli-examples/queries/analytics.sparql --format json > cli-examples/insights.json');
    console.log('✅ Insights extracted');
    
    console.log('\nStep 5: Generate canonical hash for integrity...');
    const { stdout: hashOutput } = await execAsync('unrdf canon hash cli-examples/people.ttl');
    await writeFile('cli-examples/integrity.txt', `Dataset Hash: ${hashOutput.trim()}`);
    console.log(`✅ Integrity hash: ${hashOutput.trim()}`);
    
    console.log('\n🎉 Workflow completed successfully!');
    console.log('\nGenerated files:');
    console.log('  - parse-report.txt (parsing statistics)');
    console.log('  - validation.json (validation report)');
    console.log('  - output.jsonld (JSON-LD format)');
    console.log('  - output.nq (N-Quads format)');
    console.log('  - analytics.txt (detailed metrics)');
    console.log('  - insights.json (SPARQL insights)');
    console.log('  - integrity.txt (canonical hash)');
    
  } catch (error) {
    console.error('❌ Workflow error:', error.message);
  }
}

/**
 * Demonstrate utility operations
 */
async function demonstrateUtilities() {
  console.log('\n🛠️  Utility Operations');
  console.log('=' .repeat(50));
  
  try {
    // Prefix operations
    console.log('\n🏷️  Prefix operations:');
    const { stdout: expandOutput } = await execAsync('cd cli-examples && unrdf prefix expand foaf:Person');
    console.log('Expanded foaf:Person:', expandOutput.trim());
    
    const { stdout: shrinkOutput } = await execAsync('cd cli-examples && unrdf prefix shrink "http://xmlns.com/foaf/0.1/Person"');
    console.log('Shrunk to CURIE:', shrinkOutput.trim());
    
    // ID generation
    console.log('\n🆔 ID generation examples:');
    const { stdout: hashId } = await execAsync('unrdf id hash "Alice Johnson"');
    console.log('Hash-based ID for "Alice Johnson":', hashId.trim());
    
    const { stdout: genericId } = await execAsync('unrdf id generate --prefix person');
    console.log('Generic ID with prefix:', genericId.trim());
    
    // Cache operations
    console.log('\n💾 Cache operations:');
    const { stdout: cacheStats } = await execAsync('unrdf cache stats');
    console.log(cacheStats);
    
  } catch (error) {
    console.error('❌ Utilities error:', error.message);
  }
}

/**
 * Main execution function
 */
async function main() {
  console.log('🚀 UNRDF CLI Workflow Demonstrations');
  console.log('=' .repeat(60));
  console.log('This script demonstrates various UNRDF CLI capabilities');
  console.log('with real examples and practical workflows.\n');
  
  try {
    // Setup
    await setupExampleData();
    
    // Run demonstrations
    await demonstrateBasicOperations();
    await demonstrateSPARQLQueries();
    await demonstrateValidation();
    await demonstrateConversion();
    await demonstrateAnalytics();
    await demonstrateUtilities();
    await demonstrateWorkflow();
    
    console.log('\n🎉 All demonstrations completed successfully!');
    console.log('\nTo explore further:');
    console.log('  - Check the cli-examples/ directory for generated files');
    console.log('  - Run individual CLI commands to experiment');
    console.log('  - Modify the example data and re-run workflows');
    console.log('  - Read the full CLI documentation in docs/cli/');
    
  } catch (error) {
    console.error('\n❌ Demonstration failed:', error.message);
    process.exit(1);
  }
}

// Execute if run directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch(console.error);
}

export {
  setupExampleData,
  demonstrateBasicOperations,
  demonstrateSPARQLQueries,
  demonstrateValidation,
  demonstrateConversion,
  demonstrateAnalytics,
  demonstrateUtilities,
  demonstrateWorkflow
};
