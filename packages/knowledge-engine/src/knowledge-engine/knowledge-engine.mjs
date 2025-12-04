/**
 * @file Knowledge Engine Class
 * @module @unrdf/knowledge-engine/knowledge-engine
 */

import { z } from 'zod';
import { UnrdfDataFactory as DataFactory } from '@unrdf/core/rdf/n3-justified-only';
import { getBuiltinRules } from './builtin-rules.mjs';

const { namedNode } = DataFactory;

/**
 * @typedef {Object} KnowledgeEngineOptions
 * @property {import('n3').Store} store - N3 Store for RDF data
 * @property {Array<Rule>} [rules] - Additional custom rules
 */

/**
 * @typedef {Object} InferenceStats
 * @property {number} triplesInferred - Number of triples inferred
 * @property {number} iterations - Number of inference iterations
 * @property {number} duration - Duration in milliseconds
 */

// Namespace prefixes
const NAMESPACES = {
  'rdf:': 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  'rdfs:': 'http://www.w3.org/2000/01/rdf-schema#',
  'owl:': 'http://www.w3.org/2002/07/owl#',
};

/**
 * Knowledge Engine - Rule-based inference for RDF graphs
 */
export class KnowledgeEngine {
  /**
   * Create a new Knowledge Engine
   * @param {KnowledgeEngineOptions} options
   */
  constructor(options) {
    const schema = z.object({
      store: z.any(),
      rules: z.array(z.any()).optional(),
    });

    const validated = schema.parse(options);

    // Validate store has required methods
    if (
      !validated.store ||
      typeof validated.store.addQuad !== 'function' ||
      typeof validated.store.getQuads !== 'function'
    ) {
      throw new Error('Store must have addQuad and getQuads methods');
    }

    this.store = validated.store;
    this.rules = validated.rules || getBuiltinRules();
  }

  /**
   * Expand prefixed URIs to full URIs
   * @param {string} value - Prefixed or full URI
   * @returns {string} Full URI
   */
  expandPrefix(value) {
    for (const [prefix, namespace] of Object.entries(NAMESPACES)) {
      if (value.startsWith(prefix)) {
        return value.replace(prefix, namespace);
      }
    }
    return value;
  }

  /**
   * Run inference and materialize inferred triples into the store
   * @returns {InferenceStats} Inference statistics
   */
  materialize() {
    const startTime = Date.now();
    let triplesInferred = 0;
    let iterations = 0;
    const maxIterations = 100;

    let changed = true;
    while (changed && iterations < maxIterations) {
      changed = false;
      iterations++;

      const beforeCount = this.store.size;

      for (const rule of this.rules) {
        const newTriples = this.applyRule(rule);
        triplesInferred += newTriples;
        if (newTriples > 0) {
          changed = true;
        }
      }

      const afterCount = this.store.size;
      const actualNew = afterCount - beforeCount;

      if (actualNew === 0) {
        changed = false;
      }
    }

    const duration = Date.now() - startTime;

    return {
      triplesInferred,
      iterations,
      duration,
    };
  }

  /**
   * Apply a single rule to the store
   * @param {Object} rule - Rule to apply
   * @returns {number} Number of new triples inferred
   */
  applyRule(rule) {
    let inferredCount = 0;
    const patterns = Array.isArray(rule.pattern) ? rule.pattern : [rule.pattern];
    const consequents = Array.isArray(rule.consequent) ? rule.consequent : [rule.consequent];

    // Find all binding sets that match the pattern
    const bindingSets = this.findBindings(patterns);

    // For each binding set, generate consequent triples
    for (const bindings of bindingSets) {
      for (const consequent of consequents) {
        const triple = this.instantiateConsequent(consequent, bindings);
        if (triple && this.addTriple(triple)) {
          inferredCount++;
        }
      }
    }

    return inferredCount;
  }

  /**
   * Find all binding sets that match the patterns
   * @param {Array<Object>} patterns - Patterns to match
   * @returns {Array<Object>} Array of variable bindings
   */
  findBindings(patterns) {
    if (patterns.length === 0) return [];
    if (patterns.length === 1) {
      return this.matchPattern(patterns[0]);
    }

    // Match first pattern
    let bindings = this.matchPattern(patterns[0]);

    // Join with remaining patterns
    for (let i = 1; i < patterns.length; i++) {
      const nextBindings = [];
      for (const binding of bindings) {
        const matches = this.matchPatternWithBindings(patterns[i], binding);
        nextBindings.push(...matches);
      }
      bindings = nextBindings;
    }

    return bindings;
  }

  /**
   * Match a single pattern against the store
   * @param {Object} pattern - Pattern to match
   * @returns {Array<Object>} Array of variable bindings
   */
  matchPattern(pattern) {
    const bindings = [];
    const subj = this.expandPrefix(pattern.subject);
    const pred = this.expandPrefix(pattern.predicate);
    const obj = this.expandPrefix(pattern.object);

    const subjVar = subj.startsWith('?');
    const predVar = pred.startsWith('?');
    const objVar = obj.startsWith('?');

    const quads = this.store.getQuads(
      subjVar ? null : namedNode(subj),
      predVar ? null : namedNode(pred),
      objVar ? null : namedNode(obj),
      null
    );

    for (const quad of quads) {
      const binding = {};
      if (subjVar) binding[subj] = quad.subject;
      if (predVar) binding[pred] = quad.predicate;
      if (objVar) binding[obj] = quad.object;
      bindings.push(binding);
    }

    return bindings;
  }

  /**
   * Match a pattern with existing bindings
   * @param {Object} pattern - Pattern to match
   * @param {Object} existingBindings - Existing variable bindings
   * @returns {Array<Object>} Array of extended variable bindings
   */
  matchPatternWithBindings(pattern, existingBindings) {
    const bindings = [];
    const subj = this.expandPrefix(pattern.subject);
    const pred = this.expandPrefix(pattern.predicate);
    const obj = this.expandPrefix(pattern.object);

    const subjVal = subj.startsWith('?') ? existingBindings[subj] : namedNode(subj);
    const predVal = pred.startsWith('?') ? existingBindings[pred] : namedNode(pred);
    const objVal = obj.startsWith('?') ? existingBindings[obj] : namedNode(obj);

    const quads = this.store.getQuads(subjVal || null, predVal || null, objVal || null, null);

    for (const quad of quads) {
      const binding = { ...existingBindings };
      let consistent = true;

      if (subj.startsWith('?')) {
        if (existingBindings[subj] && !quad.subject.equals(existingBindings[subj])) {
          consistent = false;
        } else {
          binding[subj] = quad.subject;
        }
      }

      if (pred.startsWith('?')) {
        if (existingBindings[pred] && !quad.predicate.equals(existingBindings[pred])) {
          consistent = false;
        } else {
          binding[pred] = quad.predicate;
        }
      }

      if (obj.startsWith('?')) {
        if (existingBindings[obj] && !quad.object.equals(existingBindings[obj])) {
          consistent = false;
        } else {
          binding[obj] = quad.object;
        }
      }

      if (consistent) {
        bindings.push(binding);
      }
    }

    return bindings;
  }

  /**
   * Instantiate a consequent with variable bindings
   * @param {Object} consequent - Consequent pattern
   * @param {Object} bindings - Variable bindings
   * @returns {Object|null} Instantiated triple or null
   */
  instantiateConsequent(consequent, bindings) {
    const subj = this.expandPrefix(consequent.subject);
    const pred = this.expandPrefix(consequent.predicate);
    const obj = this.expandPrefix(consequent.object);

    const subjTerm = subj.startsWith('?') ? bindings[subj] : namedNode(subj);
    const predTerm = pred.startsWith('?') ? bindings[pred] : namedNode(pred);
    const objTerm = obj.startsWith('?') ? bindings[obj] : namedNode(obj);

    if (!subjTerm || !predTerm || !objTerm) {
      return null;
    }

    return { subject: subjTerm, predicate: predTerm, object: objTerm };
  }

  /**
   * Add a triple to the store if it doesn't already exist
   * @param {Object} triple - Triple to add
   * @returns {boolean} True if added, false if already exists
   */
  addTriple(triple) {
    const existing = this.store.getQuads(triple.subject, triple.predicate, triple.object, null);
    if (existing.length > 0) {
      return false;
    }
    this.store.addQuad(triple.subject, triple.predicate, triple.object);
    return true;
  }

  /**
   * Add a custom rule to the engine
   * @param {Rule} rule - Rule to add
   */
  addRule(rule) {
    this.rules.push(rule);
  }

  /**
   * Get all rules in the engine
   * @returns {Array<Rule>} All rules
   */
  getRules() {
    return this.rules;
  }

  /**
   * Clear all inferred triples from the store
   */
  clear() {
    // Note: This would need additional tracking to distinguish
    // inferred triples from asserted triples
    // For now, this is a no-op
  }
}
