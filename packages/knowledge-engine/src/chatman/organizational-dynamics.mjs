/**
 * @file Organizational Dynamics Rules
 * @module knowledge-engine/chatman/organizational-dynamics
 *
 * @description
 * Organizational dynamics rules for Chatman Equation analysis.
 * Defines patterns for power structures, knowledge networks, resource flows,
 * decision criteria, and cultural norms.
 */

import { z } from 'zod';

/**
 * Organizational dynamics rule schema
 */
export const OrganizationalDynamicsRuleSchema = z.object({
  id: z.string(),
  name: z.string(),
  category: z.enum([
    'power_structures',
    'knowledge_networks',
    'resource_flows',
    'decision_criteria',
    'cultural_norms',
  ]),
  pattern: z.string(),
  darkFieldMultiplier: z.number().positive().default(19),
  confidence: z.number().min(0).max(1).default(0.8),
});

/**
 * Organizational dynamics rules
 */
export const ORGANIZATIONAL_DYNAMICS_RULES = [
  {
    id: 'od-001',
    name: 'Informal Power Structures',
    category: 'power_structures',
    pattern: 'informal_power_structures',
    darkFieldMultiplier: 19,
    confidence: 0.86,
  },
  {
    id: 'od-002',
    name: 'Tacit Knowledge Networks',
    category: 'knowledge_networks',
    pattern: 'tacit_knowledge_networks',
    darkFieldMultiplier: 19,
    confidence: 0.84,
  },
  {
    id: 'od-003',
    name: 'Hidden Resource Flows',
    category: 'resource_flows',
    pattern: 'hidden_resource_flows',
    darkFieldMultiplier: 19,
    confidence: 0.82,
  },
  {
    id: 'od-004',
    name: 'Implicit Decision Criteria',
    category: 'decision_criteria',
    pattern: 'implicit_decision_criteria',
    darkFieldMultiplier: 19,
    confidence: 0.8,
  },
  {
    id: 'od-005',
    name: 'Unwritten Cultural Norms',
    category: 'cultural_norms',
    pattern: 'unwritten_cultural_norms',
    darkFieldMultiplier: 19,
    confidence: 0.85,
  },
  {
    id: 'od-006',
    name: 'Latent Capability Reserves',
    category: 'resource_flows',
    pattern: 'latent_capability_reserves',
    darkFieldMultiplier: 19,
    confidence: 0.79,
  },
  {
    id: 'od-007',
    name: 'Invisible Coordination Mechanisms',
    category: 'power_structures',
    pattern: 'invisible_coordination_mechanisms',
    darkFieldMultiplier: 19,
    confidence: 0.81,
  },
  {
    id: 'od-008',
    name: 'Emergent Collaboration Patterns',
    category: 'knowledge_networks',
    pattern: 'emergent_collaboration_patterns',
    darkFieldMultiplier: 19,
    confidence: 0.83,
  },
];

/**
 * Get organizational dynamics rules by category
 * @param {string} category - Rule category
 * @returns {Array<Object>} Filtered rules
 */
export function getRulesByCategory(category) {
  return ORGANIZATIONAL_DYNAMICS_RULES.filter(rule => rule.category === category);
}

/**
 * Get organizational dynamics rule by ID
 * @param {string} id - Rule ID
 * @returns {Object|undefined} Rule or undefined
 */
export function getRuleById(id) {
  return ORGANIZATIONAL_DYNAMICS_RULES.find(rule => rule.id === id);
}

/**
 * Validate organizational dynamics rules
 * @param {Array<Object>} rules - Rules to validate
 * @returns {Object} Validation result
 */
export function validateRules(rules) {
  const errors = [];
  for (const rule of rules) {
    const result = OrganizationalDynamicsRuleSchema.safeParse(rule);
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
