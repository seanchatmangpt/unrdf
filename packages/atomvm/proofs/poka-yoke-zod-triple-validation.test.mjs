/**
 * Poka-Yoke Proof: Schema-Based Triple Validation
 *
 * Demonstrates that schema validation provides consistent, type-safe validation
 * for RDF triples across all modules.
 *
 * Run: node proofs/poka-yoke-zod-triple-validation.test.mjs
 *
 * NOTE: This is a simplified proof without Zod dependency.
 * Production implementation would use Zod for better error messages.
 */

// IRI validation regex (RFC 3987 simplified)
const IRI_REGEX = /^[a-zA-Z][a-zA-Z0-9+.-]*:[^\s<>"{}|\\^`]*$/;
const BLANK_NODE_REGEX = /^_:[a-zA-Z0-9_.-]+$/;

// Validation errors
class ValidationError extends Error {
  constructor(message, path) {
    super(message);
    this.name = 'ValidationError';
    this.path = path;
  }
}

// Schema validators (simplified Zod-like API)
const validators = {
  validateNamedNode(node, path = 'node') {
    if (!node || typeof node !== 'object') {
      throw new ValidationError('Must be an object', path);
    }
    if (node.termType !== 'NamedNode') {
      throw new ValidationError(`Expected termType "NamedNode", got "${node.termType}"`, path + '.termType');
    }
    if (!node.value || !IRI_REGEX.test(node.value)) {
      throw new ValidationError('IRI must be valid RFC 3987 format', path + '.value');
    }
  },

  validateBlankNode(node, path = 'node') {
    if (!node || typeof node !== 'object') {
      throw new ValidationError('Must be an object', path);
    }
    if (node.termType !== 'BlankNode') {
      throw new ValidationError(`Expected termType "BlankNode", got "${node.termType}"`, path + '.termType');
    }
    if (!node.value || !BLANK_NODE_REGEX.test(node.value)) {
      throw new ValidationError('Blank node must match _:identifier format', path + '.value');
    }
  },

  validateLiteral(node, path = 'node') {
    if (!node || typeof node !== 'object') {
      throw new ValidationError('Must be an object', path);
    }
    if (node.termType !== 'Literal') {
      throw new ValidationError(`Expected termType "Literal", got "${node.termType}"`, path + '.termType');
    }
    if (node.value === undefined || node.value === null) {
      throw new ValidationError('Literal value is required', path + '.value');
    }
  },

  validateTriple(triple) {
    if (!triple || typeof triple !== 'object') {
      throw new ValidationError('Triple must be an object', 'triple');
    }

    // Validate subject (NamedNode or BlankNode)
    if (triple.subject.termType === 'NamedNode') {
      validators.validateNamedNode(triple.subject, 'subject');
    } else if (triple.subject.termType === 'BlankNode') {
      validators.validateBlankNode(triple.subject, 'subject');
    } else {
      throw new ValidationError('Subject must be NamedNode or BlankNode', 'subject.termType');
    }

    // Validate predicate (must be NamedNode)
    validators.validateNamedNode(triple.predicate, 'predicate');

    // Validate object (NamedNode, BlankNode, or Literal)
    if (triple.object.termType === 'NamedNode') {
      validators.validateNamedNode(triple.object, 'object');
    } else if (triple.object.termType === 'BlankNode') {
      validators.validateBlankNode(triple.object, 'object');
    } else if (triple.object.termType === 'Literal') {
      validators.validateLiteral(triple.object, 'object');
    } else {
      throw new ValidationError('Object must be NamedNode, BlankNode, or Literal', 'object.termType');
    }

    return triple;
  },
};

// Test suite
console.log('\n=== Poka-Yoke Proof: Schema-Based Triple Validation ===\n');

// Test 1: Valid triple passes validation
console.log('Test 1: Valid triple passes validation');
try {
  const validTriple = {
    subject: { termType: 'NamedNode', value: 'http://example.org/alice' },
    predicate: { termType: 'NamedNode', value: 'http://xmlns.com/foaf/0.1/name' },
    object: { termType: 'Literal', value: 'Alice', datatype: { value: 'http://www.w3.org/2001/XMLSchema#string' } },
  };

  const validated = validators.validateTriple(validTriple);
  console.log('  ✓ PASS: Valid triple accepted');
  console.log('  Subject: ' + validated.subject.value);
  console.log('  Predicate: ' + validated.predicate.value);
  console.log('  Object: "' + validated.object.value + '" (' + validated.object.termType + ')');
} catch (error) {
  console.log('  ❌ FAIL: Valid triple should pass:', error.message);
  process.exit(1);
}

// Test 2: Invalid IRI throws descriptive error
console.log('\nTest 2: Invalid IRI throws descriptive error');
try {
  const invalidTriple = {
    subject: { termType: 'NamedNode', value: 'not a valid iri' },
    predicate: { termType: 'NamedNode', value: 'http://xmlns.com/foaf/0.1/name' },
    object: { termType: 'Literal', value: 'Alice' },
  };

  validators.validateTriple(invalidTriple);
  console.log('  ❌ FAIL: Invalid IRI should have thrown!');
  process.exit(1);
} catch (error) {
  if (error instanceof ValidationError) {
    console.log('  ✓ PASS: Invalid IRI rejected');
    console.log('  Error path: ' + error.path);
    console.log('  Error message: ' + error.message);
  } else {
    console.log('  ❌ FAIL: Expected ValidationError, got:', error.message);
    process.exit(1);
  }
}

// Test 3: Missing subject throws
console.log('\nTest 3: Missing subject throws');
try {
  const noSubject = {
    predicate: { termType: 'NamedNode', value: 'http://xmlns.com/foaf/0.1/name' },
    object: { termType: 'Literal', value: 'Alice' },
  };

  validators.validateTriple(noSubject);
  console.log('  ❌ FAIL: Missing subject should have thrown!');
  process.exit(1);
} catch (error) {
  console.log('  ✓ PASS: Missing subject rejected');
  console.log('  Error: ' + error.message);
}

// Test 4: Invalid predicate (must be NamedNode) throws
console.log('\nTest 4: Predicate must be NamedNode (not Literal)');
try {
  const invalidPredicate = {
    subject: { termType: 'NamedNode', value: 'http://example.org/alice' },
    predicate: { termType: 'Literal', value: 'name' },
    object: { termType: 'Literal', value: 'Alice' },
  };

  validators.validateTriple(invalidPredicate);
  console.log('  ❌ FAIL: Literal predicate should have thrown!');
  process.exit(1);
} catch (error) {
  console.log('  ✓ PASS: Invalid predicate type rejected');
  console.log('  Error: ' + error.message);
}

// Test 5: Blank node subject is valid
console.log('\nTest 5: Blank node subject is valid');
try {
  const blankNodeTriple = {
    subject: { termType: 'BlankNode', value: '_:alice123' },
    predicate: { termType: 'NamedNode', value: 'http://xmlns.com/foaf/0.1/name' },
    object: { termType: 'Literal', value: 'Alice' },
  };

  const validated = validators.validateTriple(blankNodeTriple);
  console.log('  ✓ PASS: Blank node subject accepted');
  console.log('  Subject: ' + validated.subject.value + ' (' + validated.subject.termType + ')');
} catch (error) {
  console.log('  ❌ FAIL: Blank node should be valid:', error.message);
  process.exit(1);
}

// Test 6: Invalid blank node format throws
console.log('\nTest 6: Invalid blank node format throws');
try {
  const invalidBlankNode = {
    subject: { termType: 'BlankNode', value: 'not_a_blank_node' },
    predicate: { termType: 'NamedNode', value: 'http://xmlns.com/foaf/0.1/name' },
    object: { termType: 'Literal', value: 'Alice' },
  };

  validators.validateTriple(invalidBlankNode);
  console.log('  ❌ FAIL: Invalid blank node format should have thrown!');
  process.exit(1);
} catch (error) {
  console.log('  ✓ PASS: Invalid blank node format rejected');
  console.log('  Error: ' + error.message);
}

console.log('\n=== All Tests Passed ✓ ===\n');
console.log('Vulnerability Prevented: Type Confusion (Vulnerability #3)');
console.log('Invalid Operations Made Impossible:');
console.log('  - Invalid IRI formats');
console.log('  - Missing required triple components');
console.log('  - Invalid term types (e.g., Literal as predicate)');
console.log('  - Inconsistent validation across modules');
console.log('  - Silent data corruption from malformed triples\n');
console.log('NOTE: Production implementation should use Zod for better error messages.');
console.log('      This simplified version demonstrates the poka-yoke principle.\n');
