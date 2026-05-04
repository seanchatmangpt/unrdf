/**
 * @file SHACL Validation for Chatman RDF Knowledge Graph
 * @module chatman-equation/validate-rdf
 * @description Validates generated Turtle files against SHACL shapes
 */

import { readFileSync } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));
const dataDir = join(__dirname, '../data');
const shapesDir = join(__dirname, '../shapes');

/**
 * Simple RDF validation - checks file structure and basic requirements
 * Note: Full SHACL validation requires an RDF reasoner
 * This provides basic structural validation
 * @param {string} filePath - Path to Turtle file
 * @returns {Object} Validation results
 */
function validateTurtleFile(filePath) {
  const content = readFileSync(filePath, 'utf-8');
  const errors = [];
  const warnings = [];

  // Check for required prefixes
  const requiredPrefixes = [
    '@prefix rdf:',
    '@prefix rdfs:',
    '@prefix xsd:',
    '@prefix dcterms:',
    '@prefix prov:',
    '@prefix chatman:',
  ];

  requiredPrefixes.forEach(prefix => {
    if (!content.includes(prefix)) {
      errors.push(`Missing required prefix: ${prefix}`);
    }
  });

  // Check for PROV-O provenance
  if (!content.includes('prov:wasGeneratedBy')) {
    warnings.push('No prov:wasGeneratedBy found - document provenance missing');
  }

  if (!content.includes('prov:wasAttributedTo')) {
    warnings.push('No prov:wasAttributedTo found - attribution missing');
  }

  if (!content.includes('prov:generatedAtTime')) {
    warnings.push('No prov:generatedAtTime found - timestamps missing');
  }

  // Check for basic syntax errors
  const lines = content.split('\n');
  lines.forEach((line, idx) => {
    const trimmed = line.trim();

    // Skip comments and empty lines
    if (trimmed.startsWith('#') || trimmed === '') return;

    // Check for common syntax issues
    if (trimmed.includes('^^xsd:') && !trimmed.match(/"\^\^xsd:\w+/)) {
      warnings.push(`Line ${idx + 1}: Possible malformed datatype`);
    }
  });

  // Count triples (rough estimate)
  const tripleCount = content.split(/\s+\.\s*\n/).length - 1;

  return {
    valid: errors.length === 0,
    errors,
    warnings,
    tripleCount,
    prefixCount: (content.match(/@prefix/g) || []).length,
  };
}

/**
 * Validate lineage.ttl
 * @returns {Object} Validation results
 */
function validateLineage() {
  const filePath = join(dataDir, 'lineage.ttl');
  console.log(`\n📋 Validating ${filePath}...`);

  const result = validateTurtleFile(filePath);

  console.log(`  Prefixes: ${result.prefixCount}`);
  console.log(`  Triples: ~${result.tripleCount}`);

  if (result.errors.length > 0) {
    console.log(`\n  ❌ Errors:`);
    result.errors.forEach(err => console.log(`     - ${err}`));
  }

  if (result.warnings.length > 0) {
    console.log(`\n  ⚠️  Warnings:`);
    result.warnings.forEach(warn => console.log(`     - ${warn}`));
  }

  if (result.valid && result.warnings.length === 0) {
    console.log(`\n  ✅ Validation passed with no issues`);
  } else if (result.valid) {
    console.log(`\n  ✅ Validation passed with warnings`);
  } else {
    console.log(`\n  ❌ Validation failed`);
  }

  return result;
}

/**
 * Validate achievements.ttl
 * @returns {Object} Validation results
 */
function validateAchievements() {
  const filePath = join(dataDir, 'achievements.ttl');
  console.log(`\n📋 Validating ${filePath}...`);

  const result = validateTurtleFile(filePath);

  console.log(`  Prefixes: ${result.prefixCount}`);
  console.log(`  Triples: ~${result.tripleCount}`);

  if (result.errors.length > 0) {
    console.log(`\n  ❌ Errors:`);
    result.errors.forEach(err => console.log(`     - ${err}`));
  }

  if (result.warnings.length > 0) {
    console.log(`\n  ⚠️  Warnings:`);
    result.warnings.forEach(warn => console.log(`     - ${warn}`));
  }

  if (result.valid && result.warnings.length === 0) {
    console.log(`\n  ✅ Validation passed with no issues`);
  } else if (result.valid) {
    console.log(`\n  ✅ Validation passed with warnings`);
  } else {
    console.log(`\n  ❌ Validation failed`);
  }

  return result;
}

/**
 * Validate SHACL shapes file
 * @returns {Object} Validation results
 */
function validateShapes() {
  const filePath = join(shapesDir, 'chatman-shapes.ttl');
  console.log(`\n📋 Validating ${filePath}...`);

  const result = validateTurtleFile(filePath);

  // Additional SHACL-specific checks
  const content = readFileSync(filePath, 'utf-8');

  if (!content.includes('sh:NodeShape')) {
    result.errors.push('No sh:NodeShape found - not a valid SHACL shapes file');
    result.valid = false;
  }

  const shapeCount = (content.match(/sh:NodeShape/g) || []).length;
  console.log(`  Shapes: ${shapeCount}`);
  console.log(`  Prefixes: ${result.prefixCount}`);

  if (result.errors.length > 0) {
    console.log(`\n  ❌ Errors:`);
    result.errors.forEach(err => console.log(`     - ${err}`));
  }

  if (result.warnings.length > 0) {
    console.log(`\n  ⚠️  Warnings:`);
    result.warnings.forEach(warn => console.log(`     - ${warn}`));
  }

  if (result.valid && result.warnings.length === 0) {
    console.log(`\n  ✅ Validation passed with no issues`);
  } else if (result.valid) {
    console.log(`\n  ✅ Validation passed with warnings`);
  } else {
    console.log(`\n  ❌ Validation failed`);
  }

  return result;
}

/**
 * Main validation function
 */
export function validateAll() {
  console.log('\n' + '='.repeat(70));
  console.log('  CHATMAN LINEAGE RDF VALIDATION');
  console.log('='.repeat(70));

  const results = {
    lineage: validateLineage(),
    achievements: validateAchievements(),
    shapes: validateShapes(),
  };

  console.log('\n' + '='.repeat(70));
  console.log('  SUMMARY');
  console.log('='.repeat(70));

  const allValid = results.lineage.valid && results.achievements.valid && results.shapes.valid;
  const totalErrors = results.lineage.errors.length + results.achievements.errors.length + results.shapes.errors.length;
  const totalWarnings = results.lineage.warnings.length + results.achievements.warnings.length + results.shapes.warnings.length;

  console.log(`\n  Files validated: 3`);
  console.log(`  Total errors: ${totalErrors}`);
  console.log(`  Total warnings: ${totalWarnings}`);
  console.log(`  Status: ${allValid ? '✅ PASS' : '❌ FAIL'}\n`);

  if (allValid) {
    console.log('  🎉 All validations passed!\n');
    console.log('  Generated RDF files:');
    console.log(`    - ${join(dataDir, 'lineage.ttl')} (~${results.lineage.tripleCount} triples)`);
    console.log(`    - ${join(dataDir, 'achievements.ttl')} (~${results.achievements.tripleCount} triples)`);
    console.log(`\n  SHACL shapes: ${join(shapesDir, 'chatman-shapes.ttl')}\n`);
  }

  return { success: allValid, results };
}

// Run if called directly
if (import.meta.url === `file://${process.argv[1]}`) {
  const { success } = validateAll();
  process.exit(success ? 0 : 1);
}
