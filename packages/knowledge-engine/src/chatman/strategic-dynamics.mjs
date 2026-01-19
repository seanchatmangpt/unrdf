/**
 * @file Strategic Dynamics Rules
 * @module knowledge-engine/chatman/strategic-dynamics
 *
 * @description
 * Strategic dynamics rules for Chatman Equation analysis.
 * Defines patterns for strategic options, competitive advantages,
 * strategic assumptions, industry shifts, and success factors.
 */

import { z } from 'zod';

/**
 * Strategic dynamics rule schema
 */
export const StrategicDynamicsRuleSchema = z.object({
  id: z.string(),
  name: z.string(),
  category: z.enum([
    'strategic_options',
    'competitive_advantages',
    'strategic_assumptions',
    'industry_shifts',
    'success_factors',
  ]),
  pattern: z.string(),
  darkFieldMultiplier: z.number().positive().default(19),
  confidence: z.number().min(0).max(1).default(0.8),
});

/**
 * Strategic dynamics rules
 */
export const STRATEGIC_DYNAMICS_RULES = [
  {
    id: 'sd-001',
    name: 'Latent Strategic Options',
    category: 'strategic_options',
    pattern: 'latent_strategic_options',
    darkFieldMultiplier: 19,
    confidence: 0.84,
  },
  {
    id: 'sd-002',
    name: 'Hidden Competitive Advantages',
    category: 'competitive_advantages',
    pattern: 'hidden_competitive_advantages',
    darkFieldMultiplier: 19,
    confidence: 0.86,
  },
  {
    id: 'sd-003',
    name: 'Implicit Strategic Assumptions',
    category: 'strategic_assumptions',
    pattern: 'implicit_strategic_assumptions',
    darkFieldMultiplier: 19,
    confidence: 0.82,
  },
  {
    id: 'sd-004',
    name: 'Emergent Industry Shifts',
    category: 'industry_shifts',
    pattern: 'emergent_industry_shifts',
    darkFieldMultiplier: 19,
    confidence: 0.85,
  },
  {
    id: 'sd-005',
    name: 'Tacit Success Factors',
    category: 'success_factors',
    pattern: 'tacit_success_factors',
    darkFieldMultiplier: 19,
    confidence: 0.83,
  },
  {
    id: 'sd-006',
    name: 'Invisible Threat Vectors',
    category: 'competitive_advantages',
    pattern: 'invisible_threat_vectors',
    darkFieldMultiplier: 19,
    confidence: 0.8,
  },
  {
    id: 'sd-007',
    name: 'Unrecognized Opportunities',
    category: 'strategic_options',
    pattern: 'unrecognized_opportunities',
    darkFieldMultiplier: 19,
    confidence: 0.81,
  },
  {
    id: 'sd-008',
    name: 'Hidden Ecosystem Dependencies',
    category: 'industry_shifts',
    pattern: 'hidden_ecosystem_dependencies',
    darkFieldMultiplier: 19,
    confidence: 0.79,
  },
];

/**
 * Get strategic dynamics rules by category
 * @param {string} category - Rule category
 * @returns {Array<Object>} Filtered rules
 */
export function getRulesByCategory(category) {
  return STRATEGIC_DYNAMICS_RULES.filter(rule => rule.category === category);
}

/**
 * Get strategic dynamics rule by ID
 * @param {string} id - Rule ID
 * @returns {Object|undefined} Rule or undefined
 */
export function getRuleById(id) {
  return STRATEGIC_DYNAMICS_RULES.find(rule => rule.id === id);
}

/**
 * Validate strategic dynamics rules
 * @param {Array<Object>} rules - Rules to validate
 * @returns {Object} Validation result
 */
export function validateRules(rules) {
  const errors = [];
  for (const rule of rules) {
    const result = StrategicDynamicsRuleSchema.safeParse(rule);
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
