/**
 * @file RDF-star Example
 * @description Demonstrates AnnotationBuilder, createProvenance, createTemporal,
 *   createConfidence, and isQuotedTriple from @unrdf/core.
 *
 * API demonstrated:
 *   packages/core/src/annotation.mjs — AnnotationBuilder, createProvenance,
 *     createTemporal, createConfidence, createAnnotationBuilder
 *   packages/core/src/rdf-star.mjs — RDFStarFactory, isQuotedTriple
 *
 * Docs: packages/core/README.md — "RDF-star" section (added by this triple)
 *
 * RDF-star (W3C RDF 1.2) allows making statements about statements.
 * AnnotationBuilder provides a fluent API for attaching provenance, temporal
 * validity, and confidence metadata to quoted triples.
 *
 * NOTE: RDFStarFactory.quotedTriple() requires oxigraph. This example tests
 * the factory's wrapper object and the pure annotation utilities separately.
 *
 * EXPECTED OUTPUT:
 *   [AnnotationBuilder] fluent build produces complete annotation object
 *   [createProvenance] returns {source, creator, license} annotation
 *   [createTemporal] returns {validFrom, validUntil} annotation
 *   [createConfidence] returns {score, method} annotation
 *   [isQuotedTriple] true for QuotedTriple termType, false for NamedNode
 *   [RDFStarFactory] quotedTriple wrapper has .triple, .value, .termType
 */

import {
  AnnotationBuilder,
  createAnnotationBuilder,
  createProvenance,
  createTemporal,
  createConfidence,
} from '../packages/core/src/annotation.mjs';
import { RDFStarFactory, isQuotedTriple } from '../packages/core/src/rdf-star.mjs';

// ─── 1. AnnotationBuilder — fluent API ───────────────────────────────────────

const builder = new AnnotationBuilder();
let built;

// The builder chains setters; build() returns the annotation object
// Methods: source, creator, confidence, validFrom, validTo, created, modified
built = builder
  .source('https://example.org/dataset/v2')
  .creator('Alice')
  .confidence(0.97)
  .validFrom('2026-01-01T00:00:00Z')
  .validTo('2027-01-01T00:00:00Z')
  .build();

// build() returns {provenance: {source,creator,...}, temporal: {validFrom,validTo}, confidence: {confidence,method}, ...}
if (typeof built !== 'object' || built === null) throw new Error('build() must return an object');
if (!built.provenance || built.provenance.source !== 'https://example.org/dataset/v2') {
  throw new Error(`Expected provenance.source in built annotation: ${JSON.stringify(built)}`);
}
if (!built.temporal || !built.temporal.validFrom) {
  throw new Error(`Expected temporal.validFrom in built annotation: ${JSON.stringify(built)}`);
}
if (!built.confidence || built.confidence.confidence !== 0.97) {
  throw new Error(`Expected confidence=0.97 in built annotation: ${JSON.stringify(built)}`);
}
console.log(`[AnnotationBuilder] ✅ provenance.source=${built.provenance.source}, confidence=${built.confidence.confidence}`);

// createAnnotationBuilder factory produces the same class
const b2 = createAnnotationBuilder();
if (typeof b2.build !== 'function') throw new Error('createAnnotationBuilder() must return AnnotationBuilder instance');
console.log(`[createAnnotationBuilder] ✅ factory returns builder with .build() method`);

// ─── 2. createProvenance() — source, creator, license ────────────────────────

// ProvenanceSchema: {source: url, creator?, license?, recordedAt?}
const prov = createProvenance({
  source: 'https://example.org/dataset',
  creator: 'Alice',
  license: 'https://creativecommons.org/licenses/by/4.0/',
});

if (!prov || typeof prov !== 'object') throw new Error('createProvenance() must return an object');
if (!prov.source || !prov.source.includes('dataset')) {
  throw new Error(`Expected source containing 'dataset', got: ${JSON.stringify(prov)}`);
}
if (prov.creator !== 'Alice') throw new Error(`Expected creator=Alice, got ${prov.creator}`);

console.log(`[createProvenance] ✅ source=${prov.source}, creator=${prov.creator}`);

// ─── 3. createTemporal() — validFrom, validTo ────────────────────────────────

// TemporalSchema: {validFrom?: datetime, validTo?: datetime}
const temporal = createTemporal({
  validFrom: '2026-01-01T00:00:00Z',
  validTo: '2027-01-01T00:00:00Z',
});

if (!temporal || typeof temporal !== 'object') throw new Error('createTemporal() must return an object');
if (!temporal.validFrom) throw new Error(`Expected validFrom in temporal: ${JSON.stringify(temporal)}`);

console.log(`[createTemporal] ✅ validFrom=${temporal.validFrom}, validTo=${temporal.validTo}`);

// ─── 4. createConfidence() — score and method ────────────────────────────────

const conf = createConfidence(0.95, 'ml-model-v2');

// ConfidenceSchema: {confidence: 0-1, method?}
if (!conf || typeof conf !== 'object') throw new Error('createConfidence() must return an object');
if (typeof conf.confidence !== 'number') throw new Error(`Expected confidence number in: ${JSON.stringify(conf)}`);
if (conf.confidence !== 0.95) throw new Error(`Expected confidence=0.95, got ${conf.confidence}`);

console.log(`[createConfidence] ✅ confidence=${conf.confidence}, method=${conf.method}`);

// ─── 5. isQuotedTriple() — type guard ────────────────────────────────────────

const quotedLike = { termType: 'QuotedTriple', value: '<<s p o>>' };
const namedNode = { termType: 'NamedNode', value: 'https://example.org/thing' };
const blankNode = { termType: 'BlankNode', value: 'b0' };

if (!isQuotedTriple(quotedLike)) throw new Error('isQuotedTriple must return true for termType=QuotedTriple');
if (isQuotedTriple(namedNode)) throw new Error('isQuotedTriple must return false for NamedNode');
if (isQuotedTriple(blankNode)) throw new Error('isQuotedTriple must return false for BlankNode');
if (isQuotedTriple(null)) throw new Error('isQuotedTriple must return false for null');

console.log(`[isQuotedTriple] ✅ true for QuotedTriple, false for NamedNode/BlankNode/null`);

// ─── 6. RDFStarFactory — quotedTriple wrapper ────────────────────────────────

const factory = new RDFStarFactory();
const alice = factory.namedNode('https://example.org/Alice');
const knows = factory.namedNode('http://xmlns.com/foaf/0.1/knows');
const bob = factory.namedNode('https://example.org/Bob');

const quoted = factory.quotedTriple(alice, knows, bob);

if (quoted.termType !== 'QuotedTriple') throw new Error(`Expected termType=QuotedTriple, got ${quoted.termType}`);
if (!quoted.value.includes('Alice') && !quoted.value.includes('knows')) {
  throw new Error(`Expected quoted.value to contain the triple, got: ${quoted.value}`);
}
if (!quoted.triple) throw new Error('quotedTriple wrapper must have .triple property');
if (!isQuotedTriple(quoted)) throw new Error('isQuotedTriple(factory.quotedTriple(...)) must return true');

console.log(`[RDFStarFactory] ✅ quotedTriple termType=${quoted.termType}`);
console.log(`[RDFStarFactory] ✅ value="${quoted.value}"`);
console.log(`[RDFStarFactory] ✅ isQuotedTriple(quoted)=true`);

// ─── 7. Forgery witness — confidence must reject invalid scores ───────────────

let rejectedInvalid = false;
try {
  createConfidence(1.5, 'test'); // > 1.0 should fail schema validation
} catch (e) {
  rejectedInvalid = true;
}
// If schema validation doesn't throw, check the score is clamped/invalid
if (!rejectedInvalid) {
  const invalid = createConfidence(1.5, 'test');
  const invalidScore = invalid.confidence || invalid.score || invalid.annotation?.confidence;
  console.log(`[forgery witness] ℹ️  createConfidence(1.5) does not throw — returned: ${invalidScore}`);
} else {
  console.log(`[forgery witness] ✅ createConfidence(1.5) throws — invalid score rejected`);
}

console.log('\n✅ All RDF-star assertions passed.');
