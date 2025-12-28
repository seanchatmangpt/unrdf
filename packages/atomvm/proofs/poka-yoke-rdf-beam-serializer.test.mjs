/**
 * Poka-Yoke Proof: RDF-BEAM Serializer with State Machine
 *
 * Demonstrates bulletproof RDF <-> BEAM serialization with state machine guards
 * that make invalid operations impossible by design.
 *
 * Run: node proofs/poka-yoke-rdf-beam-serializer.test.mjs
 *
 * NOTE: This is a simplified proof without Zod dependency.
 * Production implementation would use Zod for better error messages.
 */

// IRI regex
const IRI_REGEX = /^[a-zA-Z][a-zA-Z0-9+.-]*:/;
const BLANK_NODE_REGEX = /^_:/;

// Validation error
class ValidationError extends Error {
  constructor(message, path) {
    super(message);
    this.name = 'ValidationError';
    this.path = path || 'unknown';
  }
}

// Validators
function validateTriple(triple) {
  if (!triple || typeof triple !== 'object') {
    throw new ValidationError('Triple must be an object', 'triple');
  }
  if (!triple.subject || !triple.subject.termType) {
    throw new ValidationError('Subject is required', 'subject');
  }
  if (!triple.predicate || triple.predicate.termType !== 'NamedNode') {
    throw new ValidationError('Predicate must be NamedNode', 'predicate');
  }
  if (!triple.predicate.value || !IRI_REGEX.test(triple.predicate.value)) {
    throw new ValidationError('Predicate must be valid IRI', 'predicate.value');
  }
  if (!triple.object || !triple.object.termType) {
    throw new ValidationError('Object is required', 'object');
  }
  return triple;
}

// RDF-BEAM Serializer with state machine (Improvement #3)
class RDFBeamSerializer {
  #state = 'Idle';
  #lastSerialized = null;

  get state() {
    return this.#state;
  }

  serializeToBeam(triple) {
    if (this.#state === 'Error') {
      throw new Error('Cannot serialize: serializer in error state. Call reset() first.');
    }

    try {
      this.#state = 'Serializing';
      
      // Validate triple structure
      this.#state = 'Validating';
      const validated = validateTriple(triple);
      
      // Serialize to BEAM tuple format: {triple, Subject, Predicate, Object}
      const beamTuple = {
        type: 'tuple',
        elements: [
          { type: 'atom', value: 'triple' },
          this.#termToBeam(validated.subject),
          this.#termToBeam(validated.predicate),
          this.#termToBeam(validated.object),
        ],
      };
      
      this.#state = 'Complete';
      this.#lastSerialized = beamTuple;
      return beamTuple;
    } catch (error) {
      this.#state = 'Error';
      throw new Error('Serialization failed: ' + error.message);
    } finally {
      if (this.#state === 'Complete') {
        this.#state = 'Idle';
      }
    }
  }

  deserializeFromBeam(beamTuple) {
    if (this.#state === 'Error') {
      throw new Error('Cannot deserialize: serializer in error state. Call reset() first.');
    }

    try {
      this.#state = 'Validating';
      
      // Validate BEAM tuple structure
      if (!beamTuple || beamTuple.type !== 'tuple') {
        throw new Error('Invalid BEAM tuple: must be type "tuple"');
      }
      if (beamTuple.elements.length !== 4) {
        throw new Error('Invalid triple tuple: must have 4 elements');
      }
      if (beamTuple.elements[0].value !== 'triple') {
        throw new Error('Invalid triple tuple: first element must be atom "triple"');
      }
      
      // Deserialize terms
      const triple = {
        subject: this.#beamToTerm(beamTuple.elements[1]),
        predicate: this.#beamToTerm(beamTuple.elements[2]),
        object: this.#beamToTerm(beamTuple.elements[3]),
      };
      
      // Validate deserialized triple
      const validated = validateTriple(triple);
      
      this.#state = 'Complete';
      return validated;
    } catch (error) {
      this.#state = 'Error';
      throw new Error('Deserialization failed: ' + error.message);
    } finally {
      if (this.#state === 'Complete') {
        this.#state = 'Idle';
      }
    }
  }

  #termToBeam(term) {
    if (term.termType === 'NamedNode') {
      return { type: 'binary', value: term.value };
    }
    if (term.termType === 'Literal') {
      return {
        type: 'tuple',
        elements: [
          { type: 'atom', value: 'literal' },
          { type: 'binary', value: term.value },
          term.datatype ? { type: 'binary', value: term.datatype.value } : { type: 'atom', value: 'nil' },
          term.language ? { type: 'binary', value: term.language } : { type: 'atom', value: 'nil' },
        ],
      };
    }
    if (term.termType === 'BlankNode') {
      return { type: 'binary', value: term.value };
    }
    throw new Error('Unsupported term type: ' + term.termType);
  }

  #beamToTerm(beamTerm) {
    if (beamTerm.type === 'binary') {
      // Check if it's a blank node or IRI
      if (beamTerm.value.startsWith('_:')) {
        return { termType: 'BlankNode', value: beamTerm.value };
      }
      return { termType: 'NamedNode', value: beamTerm.value };
    }
    if (beamTerm.type === 'tuple' && beamTerm.elements[0].value === 'literal') {
      return {
        termType: 'Literal',
        value: beamTerm.elements[1].value,
        datatype: beamTerm.elements[2].value !== 'nil' ? { value: beamTerm.elements[2].value } : undefined,
        language: beamTerm.elements[3].value !== 'nil' ? beamTerm.elements[3].value : undefined,
      };
    }
    throw new Error('Unsupported BEAM term: ' + JSON.stringify(beamTerm));
  }

  reset() {
    this.#state = 'Idle';
    this.#lastSerialized = null;
  }
}

// Test suite
console.log('\n=== Poka-Yoke Proof: RDF-BEAM Serializer ===\n');

// Test 1: Valid triple serializes correctly
console.log('Test 1: Valid triple serializes to BEAM format');
try {
  const serializer = new RDFBeamSerializer();
  const triple = {
    subject: { termType: 'NamedNode', value: 'http://example.org/alice' },
    predicate: { termType: 'NamedNode', value: 'http://xmlns.com/foaf/0.1/name' },
    object: { termType: 'Literal', value: 'Alice' },
  };

  const beamTuple = serializer.serializeToBeam(triple);
  
  console.log('  ✓ PASS: Triple serialized successfully');
  console.log('  BEAM tuple type:', beamTuple.type);
  console.log('  BEAM tuple tag:', beamTuple.elements[0].value);
  console.log('  Subject:', beamTuple.elements[1].value);
  console.log('  Predicate:', beamTuple.elements[2].value);
  console.log('  Object:', JSON.stringify(beamTuple.elements[3]));
} catch (error) {
  console.log('  ❌ FAIL: Valid triple should serialize:', error.message);
  process.exit(1);
}

// Test 2: Invalid IRI throws before serialization
console.log('\nTest 2: Invalid predicate throws descriptive error');
try {
  const serializer = new RDFBeamSerializer();
  const invalidTriple = {
    subject: { termType: 'NamedNode', value: 'http://example.org/s' },
    predicate: { termType: 'NamedNode', value: 'not a valid iri' },
    object: { termType: 'Literal', value: 'Alice' },
  };

  serializer.serializeToBeam(invalidTriple);
  console.log('  ❌ FAIL: Invalid predicate should have thrown!');
  process.exit(1);
} catch (error) {
  console.log('  ✓ PASS: Invalid predicate rejected at boundary');
  console.log('  Error:', error.message);
  console.log('  Serializer state:', new RDFBeamSerializer().state);
}

// Test 3: Roundtrip preserves all data
console.log('\nTest 3: Roundtrip serialization preserves data');
try {
  const serializer = new RDFBeamSerializer();
  const original = {
    subject: { termType: 'NamedNode', value: 'http://example.org/alice' },
    predicate: { termType: 'NamedNode', value: 'http://xmlns.com/foaf/0.1/age' },
    object: { 
      termType: 'Literal', 
      value: '30',
      datatype: { value: 'http://www.w3.org/2001/XMLSchema#integer' }
    },
  };

  const beamTuple = serializer.serializeToBeam(original);
  const deserialized = serializer.deserializeFromBeam(beamTuple);
  
  // Verify roundtrip
  if (deserialized.subject.value !== original.subject.value) {
    throw new Error('Subject mismatch after roundtrip');
  }
  if (deserialized.predicate.value !== original.predicate.value) {
    throw new Error('Predicate mismatch after roundtrip');
  }
  if (deserialized.object.value !== original.object.value) {
    throw new Error('Object value mismatch after roundtrip');
  }
  if (deserialized.object.datatype.value !== original.object.datatype.value) {
    throw new Error('Datatype mismatch after roundtrip');
  }

  console.log('  ✓ PASS: Roundtrip preserves all data');
  console.log('  Original subject:', original.subject.value);
  console.log('  Deserialized subject:', deserialized.subject.value);
  console.log('  Original datatype:', original.object.datatype.value);
  console.log('  Deserialized datatype:', deserialized.object.datatype.value);
} catch (error) {
  console.log('  ❌ FAIL: Roundtrip should preserve data:', error.message);
  process.exit(1);
}

// Test 4: State machine prevents operations in error state
console.log('\nTest 4: State machine prevents operations in error state');
try {
  const serializer = new RDFBeamSerializer();
  
  // Trigger error state
  try {
    serializer.serializeToBeam({ invalid: 'triple' });
  } catch (e) {
    // Expected error
  }
  
  console.log('  Serializer state after error:', serializer.state);
  
  // Attempt operation in error state
  try {
    serializer.serializeToBeam({
      subject: { termType: 'NamedNode', value: 'http://example.org/test' },
      predicate: { termType: 'NamedNode', value: 'http://example.org/p' },
      object: { termType: 'Literal', value: 'test' },
    });
    console.log('  ❌ FAIL: Operation in error state should throw!');
    process.exit(1);
  } catch (error) {
    console.log('  ✓ PASS: Operation blocked in error state');
    console.log('  Error:', error.message);
  }
  
  // Reset and verify recovery
  serializer.reset();
  console.log('  Serializer state after reset:', serializer.state);
  
  const validTriple = {
    subject: { termType: 'NamedNode', value: 'http://example.org/test' },
    predicate: { termType: 'NamedNode', value: 'http://example.org/p' },
    object: { termType: 'Literal', value: 'test' },
  };
  
  serializer.serializeToBeam(validTriple);
  console.log('  ✓ PASS: Serializer recovered after reset');
} catch (error) {
  console.log('  ❌ FAIL: State machine should prevent invalid operations:', error.message);
  process.exit(1);
}

// Test 5: Deserialization validates BEAM format
console.log('\nTest 5: Deserialization validates BEAM tuple format');
try {
  const serializer = new RDFBeamSerializer();
  
  // Invalid BEAM tuple (wrong type)
  try {
    serializer.deserializeFromBeam({ type: 'list', elements: [] });
    console.log('  ❌ FAIL: Invalid BEAM type should throw!');
    process.exit(1);
  } catch (error) {
    console.log('  ✓ PASS: Invalid BEAM type rejected');
  }
  
  // Invalid BEAM tuple (wrong element count)
  serializer.reset();
  try {
    serializer.deserializeFromBeam({ 
      type: 'tuple', 
      elements: [
        { type: 'atom', value: 'triple' },
        { type: 'binary', value: 'http://ex.org/s' }
      ] 
    });
    console.log('  ❌ FAIL: Wrong element count should throw!');
    process.exit(1);
  } catch (error) {
    console.log('  ✓ PASS: Wrong element count rejected');
  }
  
  // Invalid BEAM tuple (wrong tag)
  serializer.reset();
  try {
    serializer.deserializeFromBeam({ 
      type: 'tuple', 
      elements: [
        { type: 'atom', value: 'not_triple' },
        { type: 'binary', value: 'http://ex.org/s' },
        { type: 'binary', value: 'http://ex.org/p' },
        { type: 'binary', value: 'http://ex.org/o' }
      ] 
    });
    console.log('  ❌ FAIL: Wrong tuple tag should throw!');
    process.exit(1);
  } catch (error) {
    console.log('  ✓ PASS: Wrong tuple tag rejected');
  }
} catch (error) {
  console.log('  ❌ FAIL: Deserialization validation failed:', error.message);
  process.exit(1);
}

console.log('\n=== All Tests Passed ✓ ===\n');
console.log('Vulnerability Prevented:');
console.log('  - Invalid RDF data serialized to BEAM');
console.log('  - Malformed BEAM tuples deserialized');
console.log('  - State corruption from partial operations');
console.log('  - Operations in invalid states\n');
console.log('Features Demonstrated:');
console.log('  - Schema validation at boundaries');
console.log('  - State machine prevents invalid transitions');
console.log('  - Roundtrip data preservation');
console.log('  - Error recovery with reset()');
console.log('  - BEAM tuple format enforcement\n');
console.log('NOTE: Production implementation should use Zod for better error messages.');
console.log('      This simplified version demonstrates the poka-yoke principle.\n');
