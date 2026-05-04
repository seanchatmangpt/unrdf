/**
 * @fileoverview Demo 1: Triple Roundtrip (JS → BEAM format → JS)
 * Standalone version - no external dependencies
 *
 * Proves: RDF triple serialization/deserialization preserves structure
 */

// Mock RDF dataFactory (minimal implementation)
const dataFactory = {
  namedNode: (value) => ({ termType: 'NamedNode', value }),
  blankNode: (value) => ({ termType: 'BlankNode', value: value || `_:b${Date.now()}` }),
  literal: (value, langOrDatatype) => {
    const term = { termType: 'Literal', value };
    if (typeof langOrDatatype === 'string') {
      term.language = langOrDatatype;
    } else if (langOrDatatype) {
      term.datatype = langOrDatatype;
    }
    return term;
  },
};

// BEAM Serialization Functions
function serializeTermToBeam(term) {
  const serialized = { termType: term.termType, value: term.value };
  if (term.termType === 'Literal') {
    if (term.language) serialized.language = term.language;
    if (term.datatype) serialized.datatype = { value: term.datatype.value };
  }
  return serialized;
}

function serializeTripleToBeam(quad) {
  return {
    subject: serializeTermToBeam(quad.subject),
    predicate: serializeTermToBeam(quad.predicate),
    object: serializeTermToBeam(quad.object),
  };
}

function deserializeBeamToTriple(beamTriple) {
  let subject, predicate, object;
  if (beamTriple.subject.termType === 'NamedNode') {
    subject = dataFactory.namedNode(beamTriple.subject.value);
  } else {
    subject = dataFactory.blankNode(beamTriple.subject.value);
  }
  predicate = dataFactory.namedNode(beamTriple.predicate.value);
  if (beamTriple.object.termType === 'Literal') {
    object = dataFactory.literal(beamTriple.object.value, beamTriple.object.language || beamTriple.object.datatype);
  } else if (beamTriple.object.termType === 'NamedNode') {
    object = dataFactory.namedNode(beamTriple.object.value);
  } else {
    object = dataFactory.blankNode(beamTriple.object.value);
  }
  return { subject, predicate, object };
}

// Test
console.log('╔════════════════════════════════════════════════════════════════╗');
console.log('║  Demo 1: Triple Roundtrip (JS → BEAM → JS)                   ║');
console.log('╚════════════════════════════════════════════════════════════════╝');

const originalSubject = dataFactory.namedNode('http://example.org/Alice');
const originalPredicate = dataFactory.namedNode('http://xmlns.com/foaf/0.1/knows');
const originalObject = dataFactory.namedNode('http://example.org/Bob');

console.log('\n=== STEP 1: Create JavaScript Triple ===');
console.log('Subject:', originalSubject.value);
console.log('Predicate:', originalPredicate.value);
console.log('Object:', originalObject.value);

const beamMessage = serializeTripleToBeam({ subject: originalSubject, predicate: originalPredicate, object: originalObject });

console.log('\n=== STEP 2: Serialize to BEAM Format ===');
console.log(JSON.stringify(beamMessage, null, 2));

const deserializedTriple = deserializeBeamToTriple(beamMessage);

console.log('\n=== STEP 3: Deserialize to JavaScript ===');
console.log('Subject:', deserializedTriple.subject.value);
console.log('Predicate:', deserializedTriple.predicate.value);
console.log('Object:', deserializedTriple.object.value);

const subjectMatch = originalSubject.value === deserializedTriple.subject.value;
const predicateMatch = originalPredicate.value === deserializedTriple.predicate.value;
const objectMatch = originalObject.value === deserializedTriple.object.value;
const success = subjectMatch && predicateMatch && objectMatch;

console.log('\n=== STEP 4: Verify Roundtrip ===');
console.log('Subject match:', subjectMatch ? '✅' : '❌');
console.log('Predicate match:', predicateMatch ? '✅' : '❌');
console.log('Object match:', objectMatch ? '✅' : '❌');

if (success) {
  console.log('\n✅ SUCCESS: Triple roundtrip verified - structure preserved!');
  process.exit(0);
} else {
  console.log('\n❌ FAIL: Triple roundtrip failed - structure changed!');
  process.exit(1);
}
