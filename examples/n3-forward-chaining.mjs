/**
 * @file N3 Forward-Chaining Example
 * @description Demonstrates N3 forward-chaining inference via the EYE reasoner
 *   using evaluateCondition with kind:'n3' from @unrdf/hooks.
 *
 * API demonstrated:
 *   packages/hooks/src/hooks/condition-evaluator.mjs — evaluateCondition (kind:'n3')
 *   packages/oxigraph/src/index.mjs — createStore, dataFactory
 *
 * Docs: packages/hooks/README.md — "N3 Forward-Chaining Inference" section
 *
 * Source fix applied: condition-evaluator.mjs imported eyereasoner's default
 * export as a callable function but the API is an object — the correct callable
 * is eyereasoner.n3reasoner. This example both witnesses the fix and would fail
 * if the regression is reintroduced.
 *
 * N3 forward-chaining fires EYE reasoner over the graph, entails new triples
 * per the declared rules, loads them into a temporary store, then evaluates a
 * SPARQL ASK over the entailed graph.
 *
 * EXPECTED OUTPUT:
 *   [N3] true — Alice is RestrictedClass, rule fires, requiresApproval entailed
 *   [N3] false — Bob not in store, rule does not fire, no entailment
 *   [N3] false — Carol is a different class, rule does not match
 *   [forgery witness] throws when rules or askQuery missing
 */

import { evaluateCondition } from '../packages/hooks/src/hooks/condition-evaluator.mjs';
import { createStore, dataFactory } from '../packages/oxigraph/src/index.mjs';

const { namedNode, quad, defaultGraph } = dataFactory;
const EX = 'http://example.org/';
const RDF_TYPE = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type';

// ─── 1. Basic N3 inference — class membership triggers rule ──────────────────

const store = createStore();

// Alice is a RestrictedClass member
store.add(quad(
  namedNode(`${EX}Alice`),
  namedNode(RDF_TYPE),
  namedNode(`${EX}RestrictedClass`),
  defaultGraph()
));

// Carol is a different class — should NOT trigger the rule
store.add(quad(
  namedNode(`${EX}Carol`),
  namedNode(RDF_TYPE),
  namedNode(`${EX}RegularClass`),
  defaultGraph()
));

const n3Rules = `
@prefix ex: <${EX}> .
{ ?x a ex:RestrictedClass } => { ?x ex:requiresApproval true } .
`;

// ─── 2. Positive case: rule fires for Alice ────────────────────────────────────

const resultAlice = await evaluateCondition(
  {
    kind: 'n3',
    rules: n3Rules,
    askQuery: `ASK { <${EX}Alice> <${EX}requiresApproval> true }`,
  },
  store
);

if (resultAlice !== true) {
  throw new Error(`Expected true (Alice triggers rule), got ${resultAlice}`);
}
console.log(`[N3] ✅ true — Alice ∈ RestrictedClass; rule entailed ex:requiresApproval`);

// ─── 3. Negative case: Bob not in store — no entailment ───────────────────────

const resultBob = await evaluateCondition(
  {
    kind: 'n3',
    rules: n3Rules,
    askQuery: `ASK { <${EX}Bob> <${EX}requiresApproval> true }`,
  },
  store
);

if (resultBob !== false) {
  throw new Error(`Expected false (Bob absent from store), got ${resultBob}`);
}
console.log(`[N3] ✅ false — Bob not in store; rule does not fire`);

// ─── 4. Negative case: Carol is wrong class — rule does not match ─────────────

const resultCarol = await evaluateCondition(
  {
    kind: 'n3',
    rules: n3Rules,
    askQuery: `ASK { <${EX}Carol> <${EX}requiresApproval> true }`,
  },
  store
);

if (resultCarol !== false) {
  throw new Error(`Expected false (Carol ∈ RegularClass, not RestrictedClass), got ${resultCarol}`);
}
console.log(`[N3] ✅ false — Carol ∈ RegularClass; RestrictedClass rule does not match`);

// ─── 5. Chained rules — multi-step inference ─────────────────────────────────

const chainedRules = `
@prefix ex: <${EX}> .
{ ?x a ex:RestrictedClass } => { ?x ex:requiresApproval true } .
{ ?x ex:requiresApproval true } => { ?x ex:requiresAudit true } .
`;

const resultChained = await evaluateCondition(
  {
    kind: 'n3',
    rules: chainedRules,
    askQuery: `ASK { <${EX}Alice> <${EX}requiresAudit> true }`,
  },
  store
);

if (resultChained !== true) {
  throw new Error(`Expected true for chained inference (Alice → requiresApproval → requiresAudit), got ${resultChained}`);
}
console.log(`[N3] ✅ true — chained rules: RestrictedClass → requiresApproval → requiresAudit`);

// ─── 6. Forgery witness — missing rules or askQuery throws ───────────────────

let threwMissingRules = false;
try {
  await evaluateCondition({ kind: 'n3', askQuery: 'ASK { ?s ?p ?o }' }, store);
} catch (e) {
  threwMissingRules = true;
}
if (!threwMissingRules) throw new Error('Missing rules must throw');

let threwMissingAsk = false;
try {
  await evaluateCondition({ kind: 'n3', rules: n3Rules }, store);
} catch (e) {
  threwMissingAsk = true;
}
if (!threwMissingAsk) throw new Error('Missing askQuery must throw');

console.log(`[forgery witness] ✅ missing rules throws; missing askQuery throws`);

// ─── 7. Source bug witness — eyereasoner must be callable (fixed in source) ──

// If eyereasoner's n3reasoner is not imported correctly, all assertions above
// would throw "reasoner is not a function". The fact that we reached here
// proves the fix in condition-evaluator.mjs is present and correct.
console.log(`[source fix] ✅ eyereasoner.n3reasoner callable — "reasoner is not a function" bug is fixed`);

console.log('\n✅ All N3 forward-chaining assertions passed.');
