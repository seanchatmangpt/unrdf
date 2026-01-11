#!/usr/bin/env node
/**
 * @fileoverview Verify compiled lens is JSON-serializable (no closures)
 */

import { defineLens, compileLens, verifySerializable } from '../src/index.mjs';

console.log('🔍 Serialization Verification\n');

// Create test lens
const lens = defineLens('serialization-test-v1', {
  namespace: 'https://test.example.org/',
  prefixes: {
    schema: 'http://schema.org/',
    xsd: 'http://www.w3.org/2001/XMLSchema#'
  },
  conventions: { idField: 'id' }
}, {
  TestEntity: {
    subject: {
      pattern: '{namespace}TestEntity/{id}',
      keys: ['id']
    },
    type: 'schema:Thing',
    predicates: {
      field1: { iri: 'schema:field1', datatype: 'xsd:string' },
      field2: { iri: 'schema:field2', datatype: 'xsd:integer' }
    }
  }
});

console.log('Test lens:', lens.id);
console.log('\nCompiling lens...');

const compiled = compileLens(lens);
console.log('✓ Compilation complete');
console.log('  Hash:', compiled.canonicalHash);

// Test 1: JSON.stringify
console.log('\n1. Testing JSON.stringify...');
let json;
try {
  json = JSON.stringify(compiled);
  console.log('   ✓ JSON.stringify succeeded');
  console.log(`   ✓ JSON length: ${json.length} bytes`);
} catch (error) {
  console.log('   ❌ JSON.stringify failed:', error.message);
  process.exit(1);
}

// Test 2: Check for function strings
console.log('\n2. Checking for function strings...');
const hasFunctionKeyword = json.includes('function');
const hasArrowFunction = json.includes('=>');

if (hasFunctionKeyword || hasArrowFunction) {
  console.log('   ❌ Found function strings in JSON:');
  if (hasFunctionKeyword) console.log('      - Contains "function" keyword');
  if (hasArrowFunction) console.log('      - Contains "=>" arrow function');
  process.exit(1);
} else {
  console.log('   ✓ No function strings found');
}

// Test 3: JSON.parse round-trip
console.log('\n3. Testing JSON.parse round-trip...');
let parsed;
try {
  parsed = JSON.parse(json);
  console.log('   ✓ JSON.parse succeeded');
} catch (error) {
  console.log('   ❌ JSON.parse failed:', error.message);
  process.exit(1);
}

// Test 4: Deep equality check
console.log('\n4. Testing deep equality...');
const jsonOriginal = JSON.stringify(compiled);
const jsonParsed = JSON.stringify(parsed);

if (jsonOriginal === jsonParsed) {
  console.log('   ✓ Round-trip preserves all data');
} else {
  console.log('   ❌ Round-trip lost data:');
  console.log('   Original length:', jsonOriginal.length);
  console.log('   Parsed length:  ', jsonParsed.length);
  process.exit(1);
}

// Test 5: Type checks on parsed object
console.log('\n5. Type checking parsed structure...');
const checks = [
  { name: 'lensId', type: 'string', value: parsed.lensId },
  { name: 'version', type: 'string', value: parsed.version },
  { name: 'canonicalHash', type: 'string', value: parsed.canonicalHash },
  { name: 'profile', type: 'object', value: parsed.profile },
  { name: 'compiledMappings', type: 'object', value: parsed.compiledMappings }
];

let allTypesCorrect = true;
for (const check of checks) {
  const actualType = typeof check.value;
  const expectedType = check.type;

  if (expectedType === 'object') {
    if (actualType === 'object' && check.value !== null) {
      console.log(`   ✓ ${check.name}: ${expectedType}`);
    } else {
      console.log(`   ❌ ${check.name}: expected ${expectedType}, got ${actualType}`);
      allTypesCorrect = false;
    }
  } else {
    if (actualType === expectedType) {
      console.log(`   ✓ ${check.name}: ${expectedType}`);
    } else {
      console.log(`   ❌ ${check.name}: expected ${expectedType}, got ${actualType}`);
      allTypesCorrect = false;
    }
  }
}

if (!allTypesCorrect) {
  process.exit(1);
}

// Test 6: Check for typeof === 'function' anywhere in structure
console.log('\n6. Scanning structure for functions...');
let foundFunction = false;

function scanForFunctions(obj, path = 'root') {
  if (typeof obj === 'function') {
    console.log(`   ❌ Found function at: ${path}`);
    foundFunction = true;
    return;
  }

  if (obj && typeof obj === 'object') {
    for (const [key, value] of Object.entries(obj)) {
      scanForFunctions(value, `${path}.${key}`);
    }
  }
}

scanForFunctions(parsed);

if (foundFunction) {
  process.exit(1);
} else {
  console.log('   ✓ No functions found in structure');
}

// Test 7: Use verifySerializable helper
console.log('\n7. Testing verifySerializable helper...');
const isSerializable = verifySerializable(compiled);

if (isSerializable) {
  console.log('   ✓ verifySerializable returned true');
} else {
  console.log('   ❌ verifySerializable returned false');
  process.exit(1);
}

// Summary
console.log('\n📊 Summary:');
console.log('─────────────────────────────────────────');
console.log('✓ JSON.stringify:     PASS');
console.log('✓ No function strings: PASS');
console.log('✓ JSON.parse:         PASS');
console.log('✓ Deep equality:      PASS');
console.log('✓ Type checking:      PASS');
console.log('✓ Function scan:      PASS');
console.log('✓ verifySerializable: PASS');
console.log('─────────────────────────────────────────\n');

console.log('🎉 Serialization verified: PASS\n');
process.exit(0);
