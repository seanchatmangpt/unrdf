/**
 * @file Built-in RDFS Inference Rules
 * @module @unrdf/knowledge-engine/builtin-rules
 */

import { defineRule } from './rules.mjs';

/**
 * RDFS SubClass inference rule
 * If A rdfs:subClassOf B and X rdf:type A, then infer X rdf:type B
 */
export const rdfsSubClassRule = defineRule({
  name: 'rdfs:subClassOf',
  description: 'Infer type from subclass hierarchy',
  pattern: [
    { subject: '?subClass', predicate: 'rdfs:subClassOf', object: '?superClass' },
    { subject: '?instance', predicate: 'rdf:type', object: '?subClass' },
  ],
  consequent: {
    subject: '?instance',
    predicate: 'rdf:type',
    object: '?superClass',
  },
  salience: 80,
});

/**
 * RDFS SubProperty inference rule
 * If P rdfs:subPropertyOf Q and X P Y, then infer X Q Y
 */
export const rdfsSubPropertyRule = defineRule({
  name: 'rdfs:subPropertyOf',
  description: 'Infer property from subproperty hierarchy',
  pattern: [
    { subject: '?subProp', predicate: 'rdfs:subPropertyOf', object: '?superProp' },
    { subject: '?x', predicate: '?subProp', object: '?y' },
  ],
  consequent: {
    subject: '?x',
    predicate: '?superProp',
    object: '?y',
  },
  salience: 75,
});

/**
 * RDFS Domain inference rule
 * If P rdfs:domain C and X P Y, then infer X rdf:type C
 */
export const rdfsDomainRule = defineRule({
  name: 'rdfs:domain',
  description: 'Infer type from property domain',
  pattern: [
    { subject: '?property', predicate: 'rdfs:domain', object: '?class' },
    { subject: '?x', predicate: '?property', object: '?y' },
  ],
  consequent: {
    subject: '?x',
    predicate: 'rdf:type',
    object: '?class',
  },
  salience: 70,
});

/**
 * RDFS Range inference rule
 * If P rdfs:range C and X P Y, then infer Y rdf:type C
 */
export const rdfsRangeRule = defineRule({
  name: 'rdfs:range',
  description: 'Infer type from property range',
  pattern: [
    { subject: '?property', predicate: 'rdfs:range', object: '?class' },
    { subject: '?x', predicate: '?property', object: '?y' },
  ],
  consequent: {
    subject: '?y',
    predicate: 'rdf:type',
    object: '?class',
  },
  salience: 70,
});

/**
 * OWL Transitive Property rule
 * If P rdf:type owl:TransitiveProperty and X P Y and Y P Z, then infer X P Z
 */
export const owlTransitiveRule = defineRule({
  name: 'owl:TransitiveProperty',
  description: 'Apply transitive property closure',
  pattern: [
    { subject: '?property', predicate: 'rdf:type', object: 'owl:TransitiveProperty' },
    { subject: '?x', predicate: '?property', object: '?y' },
    { subject: '?y', predicate: '?property', object: '?z' },
  ],
  consequent: {
    subject: '?x',
    predicate: '?property',
    object: '?z',
  },
  salience: 60,
});

/**
 * OWL Symmetric Property rule
 * If P rdf:type owl:SymmetricProperty and X P Y, then infer Y P X
 */
export const owlSymmetricRule = defineRule({
  name: 'owl:SymmetricProperty',
  description: 'Apply symmetric property',
  pattern: [
    { subject: '?property', predicate: 'rdf:type', object: 'owl:SymmetricProperty' },
    { subject: '?x', predicate: '?property', object: '?y' },
  ],
  consequent: {
    subject: '?y',
    predicate: '?property',
    object: '?x',
  },
  salience: 65,
});

/**
 * OWL Inverse Property rule
 * If P owl:inverseOf Q and X P Y, then infer Y Q X
 */
export const owlInverseRule = defineRule({
  name: 'owl:inverseOf',
  description: 'Apply inverse property',
  pattern: [
    { subject: '?p', predicate: 'owl:inverseOf', object: '?q' },
    { subject: '?x', predicate: '?p', object: '?y' },
  ],
  consequent: {
    subject: '?y',
    predicate: '?q',
    object: '?x',
  },
  salience: 65,
});

/**
 * Collection of all built-in RDFS/OWL rules
 */
export const builtinRules = [
  rdfsSubClassRule,
  rdfsSubPropertyRule,
  rdfsDomainRule,
  rdfsRangeRule,
  owlTransitiveRule,
  owlSymmetricRule,
  owlInverseRule,
];

/**
 * Get all built-in rules
 *
 * @returns {Object[]} Array of built-in rules
 *
 * @example
 * const rules = getBuiltinRules();
 * addRules(engine, rules);
 */
export function getBuiltinRules() {
  return builtinRules;
}

/**
 * Get RDFS rules only (no OWL)
 *
 * @returns {Object[]} Array of RDFS rules
 *
 * @example
 * const rdfsRules = getRDFSRules();
 */
export function getRDFSRules() {
  return [rdfsSubClassRule, rdfsSubPropertyRule, rdfsDomainRule, rdfsRangeRule];
}

/**
 * Get OWL rules only
 *
 * @returns {Object[]} Array of OWL rules
 *
 * @example
 * const owlRules = getOWLRules();
 */
export function getOWLRules() {
  return [owlTransitiveRule, owlSymmetricRule, owlInverseRule];
}
