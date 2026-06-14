/**
 * @file Analysis Functions Example
 * @description Demonstrates explainQuery, getGraphStatistics, and checkSchemaConformance
 *   from @unrdf/core.
 *
 * API demonstrated:
 *   packages/core/src/viz/query-explainer.mjs — explainQuery
 *   packages/core/src/debug/rdf-inspector.mjs — getGraphStatistics, checkSchemaConformance
 *
 * Docs: packages/core/README.md — "Analysis" section (added by this triple)
 *
 * EXPECTED OUTPUT:
 *   [explainQuery] returns array of steps with {step, operation, description, cost}
 *   [explainQuery] throws TypeError for empty/non-string query
 *   [getGraphStatistics] returns triple/subject/predicate/object counts for a store
 *   [checkSchemaConformance] detects missing required predicates; reports violations
 */

import { explainQuery } from '../packages/core/src/viz/query-explainer.mjs';
import {
  getGraphStatistics,
  checkSchemaConformance,
} from '../packages/core/src/debug/rdf-inspector.mjs';

// ─── Minimal duck-typed RDF store ────────────────────────────────────────────

const RDF_TYPE = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type';
const FOAF = 'http://xmlns.com/foaf/0.1/';
const EX = 'https://example.org/';

function makeQuad(s, p, o, termType = 'NamedNode') {
  return {
    subject: { termType: 'NamedNode', value: s },
    predicate: { termType: 'NamedNode', value: p },
    object: { termType, value: o },
  };
}

const quads = [
  makeQuad(`${EX}Alice`, RDF_TYPE, `${FOAF}Person`),
  makeQuad(`${EX}Bob`, RDF_TYPE, `${FOAF}Person`),
  makeQuad(`${EX}Alice`, `${FOAF}knows`, `${EX}Bob`),
  makeQuad(`${EX}Alice`, `${FOAF}name`, 'Alice', 'Literal'),
  makeQuad(`${EX}Bob`, `${FOAF}name`, 'Bob', 'Literal'),
];

// Filtering mock store — match() respects subject/predicate arguments
const mockStore = {
  match: (s, p, _o, _g) => quads.filter(q => {
    if (s && s.value && q.subject.value !== s.value) return false;
    if (p && p.value && q.predicate.value !== p.value) return false;
    return true;
  }),
};

// ─── 1. explainQuery() — SPARQL execution plan ────────────────────────────────

const sparql = `
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?name WHERE {
    ?person a foaf:Person .
    ?person foaf:name ?name .
    FILTER (STRLEN(?name) > 2)
  }
`;

// Returns {queryType, steps, estimatedCost, optimizations}
const explanation = explainQuery(sparql);

if (typeof explanation !== 'object' || explanation === null) throw new Error('explainQuery() must return an object');
if (typeof explanation.queryType !== 'string') throw new Error('result must have .queryType string');
if (!Array.isArray(explanation.steps)) throw new Error('result must have .steps array');
if (typeof explanation.estimatedCost !== 'number') throw new Error('result must have .estimatedCost number');

const { steps } = explanation;

if (steps.length === 0) throw new Error('explainQuery() must return at least 1 step for non-trivial query');

const firstStep = steps[0];
if (typeof firstStep.step !== 'number') throw new Error('Each step must have a .step number');
if (typeof firstStep.operation !== 'string') throw new Error('Each step must have a .operation string');
if (typeof firstStep.description !== 'string') throw new Error('Each step must have a .description string');
if (typeof firstStep.cost !== 'number') throw new Error('Each step must have a .cost number');

console.log(`[explainQuery] ✅ queryType=${explanation.queryType}, estimatedCost=${explanation.estimatedCost}, ${steps.length} step(s)`);
for (const s of steps) {
  console.log(`  step ${s.step}: ${s.operation} — ${s.description} (cost=${s.cost})`);
}

// explainQuery must throw TypeError for invalid input
let threwOnEmpty = false;
try { explainQuery(''); } catch (e) {
  if (e instanceof TypeError) threwOnEmpty = true;
}
if (!threwOnEmpty) throw new Error('explainQuery("") must throw TypeError');

let threwOnNonString = false;
try { explainQuery(42); } catch (e) {
  if (e instanceof TypeError) threwOnNonString = true;
}
if (!threwOnNonString) throw new Error('explainQuery(42) must throw TypeError');

console.log(`[explainQuery] ✅ throws TypeError for empty string and non-string input`);

// ─── 2. getGraphStatistics() — triple, subject, predicate, object counts ─────

const stats = getGraphStatistics(mockStore);

if (typeof stats !== 'object' || stats === null) throw new Error('getGraphStatistics() must return an object');
if (typeof stats.tripleCount !== 'number') throw new Error('stats must include tripleCount');
if (typeof stats.subjectCount !== 'number') throw new Error('stats must include subjectCount');
if (typeof stats.predicateCount !== 'number') throw new Error('stats must include predicateCount');
if (typeof stats.objectCount !== 'number') throw new Error('stats must include objectCount');

if (stats.tripleCount !== quads.length) {
  throw new Error(`Expected tripleCount=${quads.length}, got ${stats.tripleCount}`);
}

// 2 subjects: Alice and Bob
if (stats.subjectCount < 1) throw new Error(`Expected at least 1 subject, got ${stats.subjectCount}`);

console.log(`[getGraphStatistics] ✅ tripleCount=${stats.tripleCount}, subjectCount=${stats.subjectCount}, predicateCount=${stats.predicateCount}`);

// getGraphStatistics must throw TypeError for non-store input
let threwOnBadStore = false;
try { getGraphStatistics({}); } catch (e) {
  if (e instanceof TypeError) threwOnBadStore = true;
}
if (!threwOnBadStore) throw new Error('getGraphStatistics({}) (no .match) must throw TypeError');

console.log(`[getGraphStatistics] ✅ throws TypeError when store lacks .match()`);

// ─── 3. checkSchemaConformance() — required predicate validation ──────────────

// Schema: both Alice and Bob must have foaf:name
const schema = {
  requiredPredicates: [`${FOAF}name`],
};
const conformResult = checkSchemaConformance(mockStore, schema);

if (typeof conformResult.conforms !== 'boolean') throw new Error('result must have .conforms boolean');
if (typeof conformResult.violationCount !== 'number') throw new Error('result must have .violationCount number');
if (!Array.isArray(conformResult.violations)) throw new Error('result must have .violations array');

// Both subjects have foaf:name, so should conform
if (!conformResult.conforms) {
  throw new Error(`Expected conforms=true; violations: ${JSON.stringify(conformResult.violations)}`);
}
if (conformResult.violationCount !== 0) throw new Error(`Expected violationCount=0, got ${conformResult.violationCount}`);

console.log(`[checkSchemaConformance] ✅ conforms=${conformResult.conforms}, violations=${conformResult.violationCount}`);

// Non-conforming schema — require a predicate no subject has
const strictSchema = {
  requiredPredicates: [`${EX}requiredProp`],
};
const violations = checkSchemaConformance(mockStore, strictSchema);

if (violations.conforms) throw new Error('Expected conforms=false when required predicate is missing');
if (violations.violationCount === 0) throw new Error('Expected at least 1 violation');

const v0 = violations.violations[0];
if (v0.type !== 'MISSING_REQUIRED_PREDICATE') throw new Error(`Expected MISSING_REQUIRED_PREDICATE violation, got ${v0.type}`);
if (typeof v0.subject !== 'string') throw new Error('Violation must include subject');
if (typeof v0.predicate !== 'string') throw new Error('Violation must include predicate');
if (typeof v0.message !== 'string') throw new Error('Violation must include message');

console.log(`[checkSchemaConformance] ✅ detected ${violations.violationCount} violations for missing required predicate`);
console.log(`  first violation: ${v0.message}`);

// minTriples threshold violation
const smallSchema = { minTriples: 999 };
const sizeViolation = checkSchemaConformance(mockStore, smallSchema);
if (sizeViolation.conforms) throw new Error('Expected conforms=false when minTriples threshold not met');
const minTripleViolation = sizeViolation.violations.find(v => v.type === 'MIN_TRIPLES_VIOLATION');
if (!minTripleViolation) throw new Error('Expected MIN_TRIPLES_VIOLATION type in violations');

console.log(`[checkSchemaConformance] ✅ detected MIN_TRIPLES_VIOLATION when graph has fewer than minTriples`);

console.log('\n✅ All analysis function assertions passed.');
