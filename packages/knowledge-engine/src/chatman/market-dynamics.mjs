/**
 * @file Market Dynamics Rules
 * @module knowledge-engine/chatman/market-dynamics
 *
 * @description
 * Market dynamics rules for Chatman Equation analysis.
 * Defines patterns for customer needs, market segments, value drivers,
 * competition, and demand patterns.
 */

import { z } from 'zod';

/**
 * Market dynamics rule schema
 */
export const MarketDynamicsRuleSchema = z.object({
  id: z.string(),
  name: z.string(),
  category: z.enum([
    'customer_needs',
    'market_segments',
    'value_drivers',
    'competition',
    'demand_patterns',
  ]),
  pattern: z.string(),
  darkFieldMultiplier: z.number().positive().default(19),
  confidence: z.number().min(0).max(1).default(0.8),
});

/**
 * Market dynamics rules
 */
export const MARKET_DYNAMICS_RULES = [
  {
    id: 'md-001',
    name: 'Hidden Customer Needs Detection',
    category: 'customer_needs',
    pattern: 'hidden_customer_needs',
    darkFieldMultiplier: 19,
    confidence: 0.85,
  },
  {
    id: 'md-002',
    name: 'Latent Market Segments',
    category: 'market_segments',
    pattern: 'latent_market_segments',
    darkFieldMultiplier: 19,
    confidence: 0.82,
  },
  {
    id: 'md-003',
    name: 'Unspoken Value Drivers',
    category: 'value_drivers',
    pattern: 'unspoken_value_drivers',
    darkFieldMultiplier: 19,
    confidence: 0.8,
  },
  {
    id: 'md-004',
    name: 'Invisible Competition',
    category: 'competition',
    pattern: 'invisible_competition',
    darkFieldMultiplier: 19,
    confidence: 0.78,
  },
  {
    id: 'md-005',
    name: 'Emergent Demand Patterns',
    category: 'demand_patterns',
    pattern: 'emergent_demand_patterns',
    darkFieldMultiplier: 19,
    confidence: 0.83,
  },
  {
    id: 'md-006',
    name: 'Tacit Buying Criteria',
    category: 'customer_needs',
    pattern: 'tacit_buying_criteria',
    darkFieldMultiplier: 19,
    confidence: 0.81,
  },
  {
    id: 'md-007',
    name: 'Underground Distribution Channels',
    category: 'market_segments',
    pattern: 'underground_distribution_channels',
    darkFieldMultiplier: 19,
    confidence: 0.77,
  },
  {
    id: 'md-008',
    name: 'Implicit Price Expectations',
    category: 'value_drivers',
    pattern: 'implicit_price_expectations',
    darkFieldMultiplier: 19,
    confidence: 0.79,
  },
];

/**
 * Get market dynamics rules by category
 * @param {string} category - Rule category
 * @returns {Array<Object>} Filtered rules
 */
export function getRulesByCategory(category) {
  return MARKET_DYNAMICS_RULES.filter(rule => rule.category === category);
}

/**
 * Get market dynamics rule by ID
 * @param {string} id - Rule ID
 * @returns {Object|undefined} Rule or undefined
 */
export function getRuleById(id) {
  return MARKET_DYNAMICS_RULES.find(rule => rule.id === id);
}

/**
 * Validate market dynamics rules
 * @param {Array<Object>} rules - Rules to validate
 * @returns {Object} Validation result
 */
export function validateRules(rules) {
  const errors = [];
  for (const rule of rules) {
    const result = MarketDynamicsRuleSchema.safeParse(rule);
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
