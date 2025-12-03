/**
 * @file Inference Engine - Forward-chaining rule execution
 * @module @unrdf/knowledge-engine/inference-engine
 */

import { addQuad, quad, namedNode, literal, blankNode, countQuads, getQuads } from '@unrdf/core';
import { compileRule } from './rules.mjs';

/**
 * @typedef {import('n3').Store} Store
 * @typedef {import('n3').Quad} Quad
 */

/**
 * Create an inference engine
 *
 * @param {Store} store - RDF store for inference
 * @returns {Object} Inference engine instance
 *
 * @example
 * const engine = createInferenceEngine(store);
 * addRules(engine, [rdfsSubClassRule, rdfsSubPropertyRule]);
 * runInference(engine);
 */
export function createInferenceEngine(store) {
  return {
    store,
    rules: [],
    inferredQuads: [],
    iterations: 0,
  };
}

/**
 * Add rules to the inference engine
 *
 * @param {Object} engine - Inference engine
 * @param {Object[]} rules - Array of rules to add
 * @returns {void}
 *
 * @example
 * addRules(engine, [rule1, rule2, rule3]);
 */
export function addRules(engine, rules) {
  for (const rule of rules) {
    const compiled = compileRule(rule);
    engine.rules.push(compiled);
  }

  engine.rules.sort((a, b) => b.salience - a.salience);
}

/**
 * Run forward-chaining inference until fixpoint
 *
 * @param {Object} engine - Inference engine
 * @param {number} [maxIterations=100] - Maximum iterations to prevent infinite loops
 * @returns {Object} Inference results
 *
 * @example
 * const results = runInference(engine, 50);
 * console.log(`Inferred ${results.inferredCount} new facts in ${results.iterations} iterations`);
 */
export function runInference(engine, maxIterations = 100) {
  engine.iterations = 0;
  engine.inferredQuads = [];

  let previousCount = countQuads(engine.store);
  let iteration = 0;

  while (iteration < maxIterations) {
    iteration++;
    engine.iterations = iteration;

    let newFactsThisIteration = 0;

    for (const rule of engine.rules) {
      const newFacts = applyRule(engine, rule);
      newFactsThisIteration += newFacts;
    }

    const currentCount = countQuads(engine.store);

    if (currentCount === previousCount) {
      break;
    }

    previousCount = currentCount;
  }

  return {
    iterations: engine.iterations,
    inferredCount: engine.inferredQuads.length,
    fixpointReached: engine.iterations < maxIterations,
  };
}

/**
 * Apply a single rule to the store
 *
 * @param {Object} engine - Inference engine
 * @param {Object} rule - Compiled rule
 * @returns {number} Number of new facts inferred
 */
function applyRule(engine, rule) {
  const bindings = matchRulePatterns(engine.store, rule.pattern);
  let newFactsCount = 0;

  for (const binding of bindings) {
    for (const consequentPattern of rule.consequent) {
      const newQuad = instantiateConsequent(consequentPattern, binding);

      if (newQuad && !quadExists(engine.store, newQuad)) {
        try {
          addQuad(engine.store, newQuad);
          engine.inferredQuads.push(newQuad);
          newFactsCount++;
        } catch (err) {
          // Ignore duplicate quads
        }
      }
    }
  }

  return newFactsCount;
}

/**
 * Match rule patterns (handles compiled patterns with type/value objects)
 *
 * @param {Store} store - RDF store
 * @param {Object[]} patterns - Array of compiled patterns
 * @returns {Object[]} Array of binding sets
 */
function matchRulePatterns(store, patterns) {
  if (patterns.length === 0) {
    return [];
  }

  if (patterns.length === 1) {
    return matchSinglePattern(store, patterns[0]);
  }

  let currentBindings = matchSinglePattern(store, patterns[0]);

  for (let i = 1; i < patterns.length; i++) {
    const pattern = patterns[i];
    const newBindings = [];

    for (const binding of currentBindings) {
      const instantiatedPattern = instantiatePatternForMatching(pattern, binding);
      const matches = matchSinglePattern(store, instantiatedPattern);

      for (const match of matches) {
        const merged = { ...binding, ...match };
        newBindings.push(merged);
      }
    }

    currentBindings = newBindings;

    if (currentBindings.length === 0) {
      break;
    }
  }

  return currentBindings;
}

/**
 * Match a single pattern against the store
 *
 * @param {Store} store - RDF store
 * @param {Object} pattern - Pattern with type/value objects
 * @returns {Object[]} Array of bindings
 */
function matchSinglePattern(store, pattern) {
  const subject = patternElementToTerm(pattern.subject);
  const predicate = patternElementToTerm(pattern.predicate);
  const object = patternElementToTerm(pattern.object);
  const graph = pattern.graph ? patternElementToTerm(pattern.graph) : null;

  const quads = getQuads(store, subject, predicate, object, graph);
  const bindings = [];

  for (const quad of quads) {
    const binding = {};

    if (pattern.subject && pattern.subject.type === 'variable') {
      const varName = pattern.subject.value.replace(/^\?/, '');
      binding[varName] = quad.subject;
    }

    if (pattern.predicate && pattern.predicate.type === 'variable') {
      const varName = pattern.predicate.value.replace(/^\?/, '');
      binding[varName] = quad.predicate;
    }

    if (pattern.object && pattern.object.type === 'variable') {
      const varName = pattern.object.value.replace(/^\?/, '');
      binding[varName] = quad.object;
    }

    if (pattern.graph && pattern.graph.type === 'variable') {
      const varName = pattern.graph.value.replace(/^\?/, '');
      binding[varName] = quad.graph;
    }

    bindings.push(binding);
  }

  return bindings;
}

/**
 * Convert a pattern element to an N3 term (null for variables)
 *
 * @param {Object} element - Pattern element with type and value
 * @returns {Object|null} N3 term or null (for variables)
 */
function patternElementToTerm(element) {
  if (!element) return null;

  if (element.type === 'variable') {
    return null;
  }

  if (element.type === 'namedNode') {
    return namedNode(element.value);
  }

  if (element.type === 'prefixed') {
    return namedNode(expandPrefix(element.value));
  }

  if (element.type === 'literal') {
    return literal(element.value);
  }

  if (typeof element === 'object' && element.value) {
    return element;
  }

  return null;
}

/**
 * Instantiate a pattern with bindings for matching
 *
 * @param {Object} pattern - Pattern template
 * @param {Object} bindings - Variable bindings
 * @returns {Object} Pattern with variables replaced
 */
function instantiatePatternForMatching(pattern, bindings) {
  return {
    subject: instantiatePatternElement(pattern.subject, bindings),
    predicate: instantiatePatternElement(pattern.predicate, bindings),
    object: instantiatePatternElement(pattern.object, bindings),
    graph: pattern.graph ? instantiatePatternElement(pattern.graph, bindings) : null,
  };
}

/**
 * Instantiate a pattern element
 *
 * @param {Object} element - Pattern element
 * @param {Object} bindings - Variable bindings
 * @returns {Object} Instantiated element
 */
function instantiatePatternElement(element, bindings) {
  if (!element) return null;

  if (element.type === 'variable') {
    const varName = element.value.replace(/^\?/, '');
    const boundValue = bindings[varName];
    if (boundValue) {
      return boundValue;
    }
    return element;
  }

  return element;
}

/**
 * Instantiate a consequent pattern with variable bindings
 *
 * @param {Object} pattern - Consequent pattern
 * @param {Object} bindings - Variable bindings
 * @returns {Quad|null} Instantiated quad or null
 */
function instantiateConsequent(pattern, bindings) {
  const subject = instantiateTerm(pattern.subject, bindings);
  const predicate = instantiateTerm(pattern.predicate, bindings);
  const object = instantiateTerm(pattern.object, bindings);

  if (!subject || !predicate || !object) {
    return null;
  }

  return quad(subject, predicate, object);
}

/**
 * Instantiate a term with variable bindings
 *
 * @param {string|Object} term - Term or variable
 * @param {Object} bindings - Variable bindings
 * @returns {Object|null} Instantiated term or null
 */
function instantiateTerm(term, bindings) {
  if (!term) return null;

  if (typeof term === 'object' && term.type === 'variable') {
    const varName = term.value.replace(/^\?/, '');
    const boundValue = bindings[varName];
    if (boundValue && typeof boundValue === 'object' && boundValue.value) {
      return boundValue;
    }
    return null;
  }

  if (typeof term === 'string' && term.startsWith('?')) {
    const varName = term.replace(/^\?/, '');
    const boundValue = bindings[varName];
    if (boundValue && typeof boundValue === 'object' && boundValue.value) {
      return boundValue;
    }
    return null;
  }

  if (typeof term === 'object' && term.termType) {
    return term;
  }

  if (typeof term === 'object' && term.type === 'namedNode' && term.value) {
    return namedNode(term.value);
  }

  if (typeof term === 'object' && term.type === 'literal' && term.value) {
    return literal(term.value);
  }

  if (typeof term === 'object' && term.type === 'prefixed' && term.value) {
    return namedNode(expandPrefix(term.value));
  }

  return null;
}

/**
 * Check if a quad already exists in the store
 *
 * @param {Store} store - RDF store
 * @param {Quad} quadToCheck - Quad to check
 * @returns {boolean} True if quad exists
 */
function quadExists(store, quadToCheck) {
  if (!quadToCheck || !quadToCheck.subject || !quadToCheck.predicate || !quadToCheck.object) {
    return false;
  }

  const matches = store.getQuads(
    quadToCheck.subject,
    quadToCheck.predicate,
    quadToCheck.object,
    quadToCheck.graph || null
  );
  return matches.length > 0;
}

/**
 * Expand a prefixed URI (basic implementation)
 *
 * @param {string} prefixed - Prefixed URI (e.g., 'rdf:type')
 * @returns {string} Expanded URI
 */
function expandPrefix(prefixed) {
  const prefixMap = {
    rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
    rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
    owl: 'http://www.w3.org/2002/07/owl#',
    xsd: 'http://www.w3.org/2001/XMLSchema#',
    foaf: 'http://xmlns.com/foaf/0.1/',
    skos: 'http://www.w3.org/2004/02/skos/core#',
  };

  const [prefix, local] = prefixed.split(':');
  const baseUri = prefixMap[prefix];

  if (!baseUri) {
    return prefixed;
  }

  return baseUri + local;
}

/**
 * Get all inferred quads from the engine
 *
 * @param {Object} engine - Inference engine
 * @returns {Quad[]} Array of inferred quads
 *
 * @example
 * const inferred = getInferredQuads(engine);
 * console.log(`${inferred.length} facts were inferred`);
 */
export function getInferredQuads(engine) {
  return engine.inferredQuads;
}

/**
 * Reset the inference engine (clear inferred quads)
 *
 * @param {Object} engine - Inference engine
 * @returns {void}
 */
export function resetEngine(engine) {
  engine.inferredQuads = [];
  engine.iterations = 0;
}
