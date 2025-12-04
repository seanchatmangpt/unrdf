/**
 * @file Rule Definition and Compilation
 * @module @unrdf/knowledge-engine/rules
 */

import { z } from 'zod';

/**
 * @typedef {import('n3').Quad} Quad
 * @typedef {import('n3').Term} Term
 */

/**
 * Rule pattern schema - describes what to match
 */
const PatternSchema = z.object({
  subject: z.union([z.string(), z.object({ value: z.string() })]),
  predicate: z.union([z.string(), z.object({ value: z.string() })]),
  object: z.union([z.string(), z.object({ value: z.string() })]),
  graph: z.union([z.string(), z.object({ value: z.string() })]).optional(),
});

/**
 * Consequent schema - describes what to infer
 */
const ConsequentSchema = z.object({
  subject: z.union([z.string(), z.object({ value: z.string() })]),
  predicate: z.union([z.string(), z.object({ value: z.string() })]),
  object: z.union([z.string(), z.object({ value: z.string() })]),
  graph: z.union([z.string(), z.object({ value: z.string() })]).optional(),
});

/**
 * Rule configuration schema
 */
const RuleConfigSchema = z.object({
  name: z.string().min(1, 'Rule name is required'),
  description: z.string().optional(),
  pattern: z.union([PatternSchema, z.array(PatternSchema)]),
  consequent: z.union([ConsequentSchema, z.array(ConsequentSchema)]),
  salience: z.number().int().min(0).max(100).default(50),
});

/**
 * In-memory rule registry
 */
const ruleRegistry = new Map();

/**
 * Define an inference rule
 *
 * @param {Object} config - Rule configuration
 * @param {string} config.name - Rule identifier
 * @param {string} [config.description] - Rule description
 * @param {Object|Object[]} config.pattern - SPARQL-like pattern to match
 * @param {Object|Object[]} config.consequent - Quads to infer when pattern matches
 * @param {number} [config.salience=50] - Priority (0-100, higher = earlier execution)
 * @returns {Object} Compiled rule
 *
 * @example
 * const rule = defineRule({
 *   name: 'rdfs:subClassOf',
 *   pattern: [
 *     { subject: '?class', predicate: 'rdfs:subClassOf', object: '?superClass' },
 *     { subject: '?instance', predicate: 'rdf:type', object: '?class' }
 *   ],
 *   consequent: { subject: '?instance', predicate: 'rdf:type', object: '?superClass' },
 *   salience: 80
 * });
 */
export function defineRule(config) {
  const validated = RuleConfigSchema.parse(config);

  const rule = {
    name: validated.name,
    description: validated.description || '',
    pattern: Array.isArray(validated.pattern) ? validated.pattern : [validated.pattern],
    consequent: Array.isArray(validated.consequent) ? validated.consequent : [validated.consequent],
    salience: validated.salience,
    compiled: false,
  };

  ruleRegistry.set(rule.name, rule);
  return rule;
}

/**
 * Compile a rule for execution (prepare pattern matching)
 *
 * @param {Object} rule - Rule to compile
 * @returns {Object} Compiled rule with pattern matchers
 *
 * @example
 * const rule = defineRule({ name: 'test', pattern: {...}, consequent: {...} });
 * const compiled = compileRule(rule);
 */
export function compileRule(rule) {
  if (rule.compiled) {
    return rule;
  }

  const compiledPatterns = rule.pattern.map(p => ({
    subject: normalizePattern(p.subject),
    predicate: normalizePattern(p.predicate),
    object: normalizePattern(p.object),
    graph: p.graph ? normalizePattern(p.graph) : null,
  }));

  const compiledConsequents = rule.consequent.map(c => ({
    subject: normalizePattern(c.subject),
    predicate: normalizePattern(c.predicate),
    object: normalizePattern(c.object),
    graph: c.graph ? normalizePattern(c.graph) : null,
  }));

  return {
    ...rule,
    pattern: compiledPatterns,
    consequent: compiledConsequents,
    compiled: true,
  };
}

/**
 * Normalize a pattern element (convert strings to terms, detect variables)
 *
 * @param {string|Object} element - Pattern element
 * @returns {Object} Normalized element with type and value
 */
function normalizePattern(element) {
  if (typeof element === 'object' && element.value) {
    return element;
  }

  const str = String(element);

  if (str.startsWith('?')) {
    return { type: 'variable', value: str };
  }

  if (str.startsWith('http://') || str.startsWith('https://')) {
    return { type: 'namedNode', value: str };
  }

  if (str.includes(':')) {
    return { type: 'prefixed', value: str };
  }

  return { type: 'literal', value: str };
}

/**
 * Retrieve a rule by name
 *
 * @param {string} name - Rule name
 * @returns {Object|null} Rule or null if not found
 *
 * @example
 * const rule = getRule('rdfs:subClassOf');
 */
export function getRule(name) {
  return ruleRegistry.get(name) || null;
}

/**
 * Get all registered rules
 *
 * @returns {Object[]} Array of all rules
 *
 * @example
 * const allRules = getAllRules();
 */
export function getAllRules() {
  return Array.from(ruleRegistry.values());
}

/**
 * Clear all registered rules (useful for testing)
 *
 * @returns {void}
 */
export function clearRules() {
  ruleRegistry.clear();
}
