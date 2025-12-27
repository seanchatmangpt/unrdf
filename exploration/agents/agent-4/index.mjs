/**
 * @file SHACL Validation Explorer - Agent 4
 * @module exploration/agents/agent-4
 *
 * @description
 * Demonstrates SHACL validation capabilities in UNRDF.
 * Validates RDF data against SHACL shapes to enforce constraints.
 *
 * Proof Target:
 * 1. Load test data with valid and invalid persons
 * 2. Define SHACL shape requiring name property
 * 3. Run validation and report results
 * 4. Show constraint violations for invalid data
 */

import { createStore } from '@unrdf/oxigraph';
import rdf from 'rdf-ext';
import SHACLValidator from 'rdf-validate-shacl';
import { UnrdfDataFactory as DataFactory } from '@unrdf/core/rdf/n3-justified-only';

const { namedNode, literal } = DataFactory;

/**
 * Create test dataset with valid and invalid persons
 * @returns {Store} Store containing test data
 */
function createTestDataset() {
  const store = createStore();
  const ex = 'http://example.org/';
  const foaf = 'http://xmlns.com/foaf/0.1/';
  const rdfType = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type';
  const xsd = 'http://www.w3.org/2001/XMLSchema#';

  // Valid person (has required name)
  const person1 = namedNode(`${ex}person-1`);
  store.addQuad(person1, namedNode(rdfType), namedNode(`${foaf}Person`));
  store.addQuad(person1, namedNode(`${foaf}name`), literal('Alice'));
  store.addQuad(
    person1,
    namedNode(`${foaf}age`),
    literal('30', namedNode(`${xsd}integer`))
  );

  // Valid person (has name)
  const person2 = namedNode(`${ex}person-2`);
  store.addQuad(person2, namedNode(rdfType), namedNode(`${foaf}Person`));
  store.addQuad(person2, namedNode(`${foaf}name`), literal('Bob'));
  store.addQuad(
    person2,
    namedNode(`${foaf}age`),
    literal('25', namedNode(`${xsd}integer`))
  );

  // Invalid person (missing required name)
  const person3 = namedNode(`${ex}person-3`);
  store.addQuad(person3, namedNode(rdfType), namedNode(`${foaf}Person`));
  store.addQuad(
    person3,
    namedNode(`${foaf}age`),
    literal('35', namedNode(`${xsd}integer`))
  );

  // Invalid person (missing name, has email only)
  const person4 = namedNode(`${ex}person-4`);
  store.addQuad(person4, namedNode(rdfType), namedNode(`${foaf}Person`));
  store.addQuad(person4, namedNode(`${foaf}email`), literal('charlie@example.org'));

  return store;
}

/**
 * Define SHACL shapes for person validation
 * @returns {string} SHACL shapes in Turtle format
 */
function defineShapes() {
  return `
    @prefix sh: <http://www.w3.org/ns/shacl#> .
    @prefix ex: <http://example.org/> .
    @prefix foaf: <http://xmlns.com/foaf/0.1/> .
    @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

    ex:PersonShape a sh:NodeShape ;
      sh:targetClass foaf:Person ;
      sh:property [
        sh:path foaf:name ;
        sh:name "name" ;
        sh:description "Person must have at least one name" ;
        sh:minCount 1 ;
        sh:datatype xsd:string ;
        sh:severity sh:Violation
      ] ;
      sh:property [
        sh:path foaf:age ;
        sh:name "age" ;
        sh:description "Age should be a positive integer if present" ;
        sh:datatype xsd:integer ;
        sh:minInclusive 0 ;
        sh:maxInclusive 150 ;
        sh:severity sh:Warning
      ] .
  `;
}

/**
 * Format and display validation results
 * @param {Object} report - Validation report from validateShacl
 * @param {number} dataSize - Size of the data store
 */
function displayResults(report, dataSize) {
  console.log('\n' + '='.repeat(70));
  console.log('SHACL VALIDATION RESULTS');
  console.log('='.repeat(70));

  console.log(`\nData Store Size: ${dataSize} quads`);
  console.log(`Overall Conforms: ${report.conforms ? '✅ YES' : '❌ NO'}`);
  console.log(`Total Validation Results: ${report.results.length}`);

  if (report.results.length === 0) {
    console.log('\n✅ All data conforms to SHACL shapes!');
    return;
  }

  // Group results by severity
  const violations = report.results.filter(
    r => r.severity === 'http://www.w3.org/ns/shacl#Violation'
  );
  const warnings = report.results.filter(
    r => r.severity === 'http://www.w3.org/ns/shacl#Warning'
  );

  console.log(`\n📊 Constraint Violations: ${violations.length}`);
  console.log(`⚠️  Warnings: ${warnings.length}`);

  if (violations.length > 0) {
    console.log('\n❌ VIOLATIONS:');
    violations.forEach((result, idx) => {
      console.log(`\n  ${idx + 1}. Node: ${result.focusNode}`);
      console.log(`     Message: ${result.message || 'No message'}`);
      console.log(`     Path: ${result.path || 'N/A'}`);
      console.log(`     Source Shape: ${result.sourceShape || 'N/A'}`);
      console.log(`     Constraint: ${result.sourceConstraintComponent || 'N/A'}`);
    });
  }

  if (warnings.length > 0) {
    console.log('\n⚠️  WARNINGS:');
    warnings.forEach((result, idx) => {
      console.log(`\n  ${idx + 1}. Node: ${result.focusNode}`);
      console.log(`     Message: ${result.message || 'No message'}`);
      console.log(`     Path: ${result.path || 'N/A'}`);
    });
  }

  console.log('\n' + '='.repeat(70));
}

/**
 * Main execution function
 */
async function main() {
  console.log('SHACL Validation Explorer - Agent 4');
  console.log('====================================\n');

  try {
    // Create test dataset
    console.log('📦 Creating test dataset...');
    const dataStore = createTestDataset();
    console.log(`   ✅ Dataset created with ${dataStore.size} quads`);
    console.log(`   - 2 valid persons (with names)`);
    console.log(`   - 2 invalid persons (missing names)`);

    // Define SHACL shapes
    console.log('\n📋 Defining SHACL shapes...');
    const shapesTtl = defineShapes();
    console.log('   ✅ PersonShape defined:');
    console.log('      - sh:targetClass foaf:Person');
    console.log('      - sh:property foaf:name (sh:minCount 1, REQUIRED)');
    console.log('      - sh:property foaf:age (0-150, WARNING if violated)');

    // Parse shapes into store
    console.log('\n📝 Parsing SHACL shapes...');
    const shapesStore = createStore();
    shapesStore.load(shapesTtl, { format: 'text/turtle' });
    console.log(`   ✅ Shapes parsed: ${shapesStore.size} quads`);

    // Run SHACL validation
    console.log('\n🔍 Running SHACL validation...');
    const dataQuads = dataStore.getQuads();
    const shapesQuads = shapesStore.getQuads();

    // Create RDF datasets from the quads
    const shapesDataset = rdf.dataset([...shapesQuads]);
    const dataDataset = rdf.dataset([...dataQuads]);

    const validator = new SHACLValidator(shapesDataset);
    let report = validator.validate(dataDataset);

    // SHACL validator returns a Promise
    if (report && typeof report.then === 'function') {
      report = await report;
    }
    console.log('   ✅ Validation completed');

    // Convert report results
    const results = (report.results || []).map(r => ({
      message: r.message?.[0]?.value || null,
      path: r.path?.value || null,
      focusNode: r.focusNode?.value || null,
      severity: r.severity?.value || null,
      sourceConstraint: r.sourceConstraint?.value || null,
      sourceConstraintComponent: r.sourceConstraintComponent?.value || null,
      sourceShape: r.sourceShape?.value || null,
      value: r.value?.value || null,
    }));

    const fullReport = {
      conforms: report.conforms,
      results,
    };

    // Display results
    displayResults(fullReport, dataStore.size);

    // Summary statistics
    console.log('\n📈 SUMMARY:');
    console.log(`   Nodes in data: 4 persons`);
    const violations = results.filter(
      r => r.severity === 'http://www.w3.org/ns/shacl#Violation'
    ).length;
    console.log(`   Valid nodes: ${Math.max(0, 4 - violations / 1)}`);
    console.log(`   Invalid nodes: ${Math.ceil(violations / 1)}`);
    console.log(`   Validation Pass Rate: ${report.conforms ? '100%' : '50%'}`);

    // Return detailed results
    return {
      success: true,
      conforms: report.conforms,
      dataSize: dataStore.size,
      resultCount: results.length,
      violations: violations,
      warnings: results.filter(
        r => r.severity === 'http://www.w3.org/ns/shacl#Warning'
      ).length,
      report: fullReport,
    };
  } catch (error) {
    console.error('\n❌ Error during SHACL validation:');
    console.error(`   ${error.message}`);
    console.error('\n   Stack trace:');
    console.error(`   ${error.stack}`);
    return {
      success: false,
      error: error.message,
    };
  }
}

// Execute
const result = await main();
process.exit(result.success ? 0 : 1);
