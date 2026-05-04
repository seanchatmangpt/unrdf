/**
 * @file Disruption Arithmetic Rules
 * @module knowledge-engine/chatman/disruption-arithmetic
 *
 * @description
 * Disruption arithmetic rules for Chatman Equation analysis.
 * Defines patterns for disruption vectors, innovation barriers,
 * adoption triggers, technology convergence, and resistance patterns.
 */

import { z } from 'zod';

/**
 * Disruption arithmetic rule schema
 */
export const DisruptionArithmeticRuleSchema = z.object({
  id: z.string(),
  name: z.string(),
  category: z.enum([
    'disruption_vectors',
    'innovation_barriers',
    'adoption_triggers',
    'technology_convergence',
    'resistance_patterns',
  ]),
  pattern: z.string(),
  darkFieldMultiplier: z.number().positive().default(19),
  confidence: z.number().min(0).max(1).default(0.8),
});

/**
 * Disruption arithmetic rules
 */
export const DISRUPTION_ARITHMETIC_RULES = [
  {
    id: 'da-001',
    name: 'Latent Disruption Vectors',
    category: 'disruption_vectors',
    pattern: 'latent_disruption_vectors',
    darkFieldMultiplier: 19,
    confidence: 0.85,
  },
  {
    id: 'da-002',
    name: 'Hidden Innovation Barriers',
    category: 'innovation_barriers',
    pattern: 'hidden_innovation_barriers',
    darkFieldMultiplier: 19,
    confidence: 0.83,
  },
  {
    id: 'da-003',
    name: 'Implicit Adoption Triggers',
    category: 'adoption_triggers',
    pattern: 'implicit_adoption_triggers',
    darkFieldMultiplier: 19,
    confidence: 0.84,
  },
  {
    id: 'da-004',
    name: 'Emergent Technology Convergence',
    category: 'technology_convergence',
    pattern: 'emergent_technology_convergence',
    darkFieldMultiplier: 19,
    confidence: 0.86,
  },
  {
    id: 'da-005',
    name: 'Tacit Resistance Patterns',
    category: 'resistance_patterns',
    pattern: 'tacit_resistance_patterns',
    darkFieldMultiplier: 19,
    confidence: 0.82,
  },
  {
    id: 'da-006',
    name: 'Invisible Tipping Points',
    category: 'disruption_vectors',
    pattern: 'invisible_tipping_points',
    darkFieldMultiplier: 19,
    confidence: 0.87,
  },
  {
    id: 'da-007',
    name: 'Unrecognized Substitutes',
    category: 'innovation_barriers',
    pattern: 'unrecognized_substitutes',
    darkFieldMultiplier: 19,
    confidence: 0.8,
  },
  {
    id: 'da-008',
    name: 'Hidden Value Chain Vulnerabilities',
    category: 'disruption_vectors',
    pattern: 'hidden_value_chain_vulnerabilities',
    darkFieldMultiplier: 19,
    confidence: 0.81,
  },
];

/**
 * Get disruption arithmetic rules by category
 * @param {string} category - Rule category
 * @returns {Array<Object>} Filtered rules
 */
export function getRulesByCategory(category) {
  return DISRUPTION_ARITHMETIC_RULES.filter(rule => rule.category === category);
}

/**
 * Get disruption arithmetic rule by ID
 * @param {string} id - Rule ID
 * @returns {Object|undefined} Rule or undefined
 */
export function getRuleById(id) {
  return DISRUPTION_ARITHMETIC_RULES.find(rule => rule.id === id);
}

/**
 * Validate disruption arithmetic rules
 * @param {Array<Object>} rules - Rules to validate
 * @returns {Object} Validation result
 */
export function validateRules(rules) {
  const errors = [];
  for (const rule of rules) {
    const result = DisruptionArithmeticRuleSchema.safeParse(rule);
    if (!result.success) {
      errors.push({
        rule: rule.id || 'unknown',
        errors: result.error.errors,
      });
    }
  }

  return {
    valid: errors.length === 0,
    errors,
  };
}
