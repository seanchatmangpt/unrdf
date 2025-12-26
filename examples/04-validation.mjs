/**
 * Example 04: Data Validation
 *
 * This example demonstrates:
 * - Basic Zod validation
 * - Custom validation rules
 * - Quad validation
 * - Store validation
 *
 * Note: Full SHACL validation requires additional setup.
 * This example shows validation patterns using UNRDF's built-in capabilities.
 *
 * Run: node examples/04-validation.mjs
 */

import {
  createStore,
  namedNode,
  literal,
  FOAF,
  RDF,
  validateQuad,
  validateStore
} from '@unrdf/core';

console.log('=== Example 04: Data Validation ===\n');

const ex = (name) => namedNode(`http://example.org/${name}`);

// ============================================================================
// 1. Basic Quad Validation
// ============================================================================
console.log('--- 1. Basic Quad Validation ---');

// Valid quad
const validQuad = {
  subject: ex('alice'),
  predicate: FOAF.name,
  object: literal('Alice Smith')
};

try {
  const isValid = validateQuad(validQuad);
  console.log(`  ✅ Valid quad: ${isValid.success}`);
} catch (error) {
  console.log(`  ❌ Invalid quad: ${error.message}`);
}

// Invalid quad (missing predicate)
console.log('\n  Testing invalid quad (missing predicate)...');
try {
  const invalidQuad = {
    subject: ex('alice'),
    // predicate: missing!
    object: literal('Alice Smith')
  };
  validateQuad(invalidQuad);
  console.log('  ✅ Quad is valid');
} catch (error) {
  console.log(`  ❌ Validation failed: ${error.message}`);
}

// ============================================================================
// 2. Custom Validation Logic
// ============================================================================
console.log('\n--- 2. Custom Validation Logic ---');

/**
 * Validate that a person has required properties
 */
function validatePerson(personUri, store) {
  const errors = [];

  // Check if person exists
  const typeQuads = store.getQuads(personUri, RDF.type, FOAF.Person);
  if (typeQuads.length === 0) {
    errors.push(`${personUri.value} is not typed as foaf:Person`);
  }

  // Check for required name
  const nameQuads = store.getQuads(personUri, FOAF.name, null);
  if (nameQuads.length === 0) {
    errors.push(`${personUri.value} is missing foaf:name`);
  } else if (nameQuads.length > 1) {
    errors.push(`${personUri.value} has multiple names`);
  }

  // Check for valid email if present
  const emailQuads = store.getQuads(personUri, FOAF.mbox, null);
  for (const quad of emailQuads) {
    const email = quad.object.value;
    if (!email.includes('@')) {
      errors.push(`${personUri.value} has invalid email: ${email}`);
    }
  }

  return {
    valid: errors.length === 0,
    errors
  };
}

// Test validation
const store = createStore();

// Add valid person
store.addQuad(ex('alice'), RDF.type, FOAF.Person);
store.addQuad(ex('alice'), FOAF.name, literal('Alice Smith'));
store.addQuad(ex('alice'), FOAF.mbox, literal('alice@example.com'));

const aliceValidation = validatePerson(ex('alice'), store);
console.log(`  Alice validation: ${aliceValidation.valid ? '✅ Valid' : '❌ Invalid'}`);
if (!aliceValidation.valid) {
  aliceValidation.errors.forEach(err => console.log(`    - ${err}`));
}

// Add invalid person (missing name)
store.addQuad(ex('bob'), RDF.type, FOAF.Person);
store.addQuad(ex('bob'), FOAF.mbox, literal('invalid-email'));

const bobValidation = validatePerson(ex('bob'), store);
console.log(`  Bob validation: ${bobValidation.valid ? '✅ Valid' : '❌ Invalid'}`);
if (!bobValidation.valid) {
  bobValidation.errors.forEach(err => console.log(`    - ${err}`));
}

// ============================================================================
// 3. Store-level Validation
// ============================================================================
console.log('\n--- 3. Store-level Validation ---');

try {
  const storeValidation = validateStore(store);
  console.log(`  ✅ Store is valid: ${storeValidation.success}`);
  console.log(`  Total triples: ${store.size}`);
} catch (error) {
  console.log(`  ❌ Store validation failed: ${error.message}`);
}

// ============================================================================
// 4. Validation Rules (Business Logic)
// ============================================================================
console.log('\n--- 4. Business Logic Validation ---');

/**
 * Validate business rules for a team
 */
function validateTeamRules(store) {
  const rules = [];

  // Rule 1: Every person should have at least one connection
  const peopleQuads = store.getQuads(null, RDF.type, FOAF.Person);
  for (const quad of peopleQuads) {
    const person = quad.subject;
    const connections = store.getQuads(person, FOAF.knows, null);

    if (connections.length === 0) {
      const nameQuad = store.getQuads(person, FOAF.name, null)[0];
      const name = nameQuad ? nameQuad.object.value : person.value;
      rules.push({
        type: 'WARNING',
        message: `${name} has no connections`
      });
    }
  }

  // Rule 2: Emails should be unique
  const emailMap = new Map();
  const emailQuads = store.getQuads(null, FOAF.mbox, null);

  for (const quad of emailQuads) {
    const email = quad.object.value;
    if (emailMap.has(email)) {
      rules.push({
        type: 'ERROR',
        message: `Duplicate email: ${email}`
      });
    }
    emailMap.set(email, quad.subject);
  }

  // Rule 3: Person should have name
  for (const quad of peopleQuads) {
    const person = quad.subject;
    const nameQuads = store.getQuads(person, FOAF.name, null);

    if (nameQuads.length === 0) {
      rules.push({
        type: 'ERROR',
        message: `Person ${person.value} has no name`
      });
    }
  }

  return rules;
}

const validationRules = validateTeamRules(store);
console.log(`  Found ${validationRules.length} validation issues:\n`);

const errors = validationRules.filter(r => r.type === 'ERROR');
const warnings = validationRules.filter(r => r.type === 'WARNING');

if (errors.length > 0) {
  console.log('  Errors:');
  errors.forEach(rule => console.log(`    ❌ ${rule.message}`));
}

if (warnings.length > 0) {
  console.log('  Warnings:');
  warnings.forEach(rule => console.log(`    ⚠️  ${rule.message}`));
}

if (validationRules.length === 0) {
  console.log('  ✅ All rules passed!');
}

// ============================================================================
// 5. Validation with Corrections
// ============================================================================
console.log('\n--- 5. Validation with Auto-Correction ---');

/**
 * Validate and auto-correct email format
 */
function validateAndCorrectEmails(store) {
  const corrections = [];

  const emailQuads = store.getQuads(null, FOAF.mbox, null);

  for (const quad of emailQuads) {
    const email = quad.object.value;
    const trimmed = email.trim().toLowerCase();

    if (email !== trimmed) {
      corrections.push({
        original: email,
        corrected: trimmed,
        subject: quad.subject
      });

      // Remove old quad
      store.removeQuad(quad);

      // Add corrected quad
      store.addQuad(quad.subject, FOAF.mbox, literal(trimmed));
    }
  }

  return corrections;
}

// Add email with spacing issues
store.addQuad(ex('carol'), RDF.type, FOAF.Person);
store.addQuad(ex('carol'), FOAF.name, literal('Carol White'));
store.addQuad(ex('carol'), FOAF.mbox, literal('  Carol@Example.COM  '));

console.log('  Before correction:');
const beforeQuad = store.getQuads(ex('carol'), FOAF.mbox, null)[0];
console.log(`    Carol's email: "${beforeQuad.object.value}"`);

const corrections = validateAndCorrectEmails(store);

console.log('\n  After correction:');
const afterQuad = store.getQuads(ex('carol'), FOAF.mbox, null)[0];
console.log(`    Carol's email: "${afterQuad.object.value}"`);

if (corrections.length > 0) {
  console.log(`\n  Applied ${corrections.length} correction(s)`);
  corrections.forEach(c => {
    console.log(`    "${c.original}" → "${c.corrected}"`);
  });
}

// ============================================================================
// Summary
// ============================================================================
console.log('\n--- Summary ---');
console.log('  Validation strategies:');
console.log('  ✓ Quad-level validation (structure)');
console.log('  ✓ Store-level validation (integrity)');
console.log('  ✓ Custom business rules');
console.log('  ✓ Auto-correction');
console.log('  ✓ Error vs. Warning levels');

console.log('\n--- Validation Results ---');
const finalRules = validateTeamRules(store);
const finalErrors = finalRules.filter(r => r.type === 'ERROR');
console.log(`  Errors: ${finalErrors.length}`);
console.log(`  Warnings: ${finalRules.length - finalErrors.length}`);
console.log(`  Total triples: ${store.size}`);

console.log('\n✅ Example complete!');
console.log('\nNext: Try examples/05-advanced-patterns.mjs for advanced use cases');
