/**
 * Meta-Capabilities - Higher-order capabilities that create capabilities
 *
 * Theory:
 *   Meta-capability M: C → C' (transforms capability space)
 *   Self-improving: M(M) → M' where effectiveness(M') > effectiveness(M)
 *   Recursive enhancement: M^n → M^∞ (fixed point)
 *
 * Categories:
 *   1. Capability generators (create new capabilities)
 *   2. Capability enhancers (improve existing capabilities)
 *   3. Capability composers (combine capabilities programmatically)
 *   4. Capability validators (verify capability correctness)
 *
 * @module @unrdf/kgc-claude/capabilities/meta-capabilities
 */

import { z } from 'zod';
import { blake3 } from 'hash-wasm';
import { now } from '@unrdf/kgc-4d';

/**
 * Meta-capability schema
 */
export const MetaCapabilitySchema = z.object({
  id: z.string(),
  name: z.string(),
  type: z.enum(['generator', 'enhancer', 'composer', 'validator', 'self_improver']),
  input_space: z.array(z.string()),
  output_space: z.array(z.string()),
  transformation: z.string(),
  recursive: z.boolean().default(false),
  fixed_point_reached: z.boolean().default(false),
});

/**
 * @typedef {z.infer<typeof MetaCapabilitySchema>} MetaCapability
 */

/**
 * Capability generation result schema
 */
export const GenerationResultSchema = z.object({
  id: z.string(),
  meta_capability_id: z.string(),
  input_capabilities: z.array(z.string()),
  output_capability: z.object({
    id: z.string(),
    name: z.string(),
    properties: z.array(z.string()),
    effectiveness: z.number(),
  }),
  generation_method: z.string(),
  timestamp: z.bigint(),
  hash: z.string(),
});

/**
 * @typedef {z.infer<typeof GenerationResultSchema>} GenerationResult
 */

/**
 * Enhancement result schema
 */
export const EnhancementResultSchema = z.object({
  id: z.string(),
  capability_id: z.string(),
  before_effectiveness: z.number(),
  after_effectiveness: z.number(),
  improvement_factor: z.number(),
  enhancements_applied: z.array(z.string()),
  timestamp: z.bigint(),
});

/**
 * @typedef {z.infer<typeof EnhancementResultSchema>} EnhancementResult
 */

/**
 * Meta-Capability Engine - Higher-order capability operations
 */
export class MetaCapabilityEngine {
  constructor() {
    /** @type {Map<string, MetaCapability>} */
    this.metaCapabilities = new Map();
    /** @type {GenerationResult[]} */
    this.generations = [];
    /** @type {EnhancementResult[]} */
    this.enhancements = [];
    this.iterationCount = 0;
  }

  /**
   * Register meta-capability
   * @param {MetaCapability} metaCap
   */
  registerMetaCapability(metaCap) {
    const validated = MetaCapabilitySchema.parse(metaCap);
    this.metaCapabilities.set(validated.id, validated);
  }

  /**
   * Generate new capability from existing ones
   * M_gen: C × C → C'
   * @param {string} metaCapId
   * @param {Object[]} inputCapabilities
   * @param {Object} context
   * @returns {Promise<GenerationResult>}
   */
  async generateCapability(metaCapId, inputCapabilities, context = {}) {
    const metaCap = this.metaCapabilities.get(metaCapId);
    if (!metaCap || metaCap.type !== 'generator') {
      throw new Error(`Generator meta-capability not found: ${metaCapId}`);
    }

    // Simulate capability generation
    const inputIds = inputCapabilities.map(c => c.id);
    const outputId = `generated-${inputIds.join('-')}-${this.generations.length}`;

    // Combine properties from inputs
    const inputProperties = inputCapabilities.flatMap(c => c.properties || []);
    const emergentProperties = this.deriveEmergentProperties(inputProperties, context);

    // Calculate effectiveness
    const baseEffectiveness = inputCapabilities.reduce((sum, c) =>
      sum + (c.effectiveness || 50), 0) / inputCapabilities.length;
    const generationBonus = emergentProperties.length * 10;
    const effectiveness = Math.min(100, baseEffectiveness + generationBonus);

    const outputCapability = {
      id: outputId,
      name: `${inputCapabilities.map(c => c.name).join(' + ')} Composite`,
      properties: [...new Set([...inputProperties, ...emergentProperties])],
      effectiveness,
    };

    const t_ns = now();
    const resultData = {
      id: `gen-${metaCapId}-${t_ns}`,
      meta_capability_id: metaCapId,
      input_capabilities: inputIds,
      output_capability: outputCapability,
      generation_method: metaCap.transformation,
      timestamp: t_ns,
    };

    const hash = await blake3(JSON.stringify(resultData, (key, value) =>
      typeof value === 'bigint' ? value.toString() : value
    ));

    const result = GenerationResultSchema.parse({
      ...resultData,
      hash,
    });

    this.generations.push(result);
    return result;
  }

  /**
   * Enhance existing capability
   * M_enh: C → C' where effectiveness(C') > effectiveness(C)
   * @param {string} capabilityId
   * @param {Object} capability
   * @param {string[]} enhancements
   * @returns {Promise<EnhancementResult>}
   */
  async enhanceCapability(capabilityId, capability, enhancements) {
    const beforeEffectiveness = capability.effectiveness || 50;

    // Apply enhancements
    const enhancementBoost = enhancements.length * 5;
    const afterEffectiveness = Math.min(100, beforeEffectiveness + enhancementBoost);
    const improvementFactor = afterEffectiveness / beforeEffectiveness;

    const t_ns = now();

    const result = EnhancementResultSchema.parse({
      id: `enh-${capabilityId}-${t_ns}`,
      capability_id: capabilityId,
      before_effectiveness: beforeEffectiveness,
      after_effectiveness: afterEffectiveness,
      improvement_factor: improvementFactor,
      enhancements_applied: enhancements,
      timestamp: t_ns,
    });

    this.enhancements.push(result);
    return result;
  }

  /**
   * Compose capabilities programmatically
   * M_comp: [C₁, C₂, ..., Cₙ] → C'
   * @param {Object[]} capabilities
   * @param {string} compositionRule
   * @returns {Promise<Object>}
   */
  async composeCapabilities(capabilities, compositionRule) {
    // Different composition rules
    const rules = {
      'serial': this.composeSerial.bind(this),
      'parallel': this.composeParallel.bind(this),
      'conditional': this.composeConditional.bind(this),
      'feedback_loop': this.composeFeedbackLoop.bind(this),
    };

    const composeFn = rules[compositionRule] || rules.serial;
    return await composeFn(capabilities);
  }

  /**
   * Serial composition: C₁ → C₂ → ... → Cₙ
   * @param {Object[]} capabilities
   * @returns {Promise<Object>}
   */
  async composeSerial(capabilities) {
    return {
      type: 'serial',
      capabilities: capabilities.map(c => c.id),
      properties: capabilities.flatMap(c => c.properties || []),
      effectiveness: capabilities.reduce((sum, c) =>
        sum + (c.effectiveness || 50), 0) / capabilities.length,
      latency_ms: capabilities.reduce((sum, c) =>
        sum + (c.metrics?.latency_ms || 0), 0),
    };
  }

  /**
   * Parallel composition: C₁ ∥ C₂ ∥ ... ∥ Cₙ
   * @param {Object[]} capabilities
   * @returns {Promise<Object>}
   */
  async composeParallel(capabilities) {
    return {
      type: 'parallel',
      capabilities: capabilities.map(c => c.id),
      properties: [...new Set(capabilities.flatMap(c => c.properties || []))],
      effectiveness: capabilities.reduce((sum, c) =>
        sum + (c.effectiveness || 50), 0) / capabilities.length * 1.5, // Parallel bonus
      latency_ms: Math.max(...capabilities.map(c => c.metrics?.latency_ms || 0)),
      throughput_multiplier: capabilities.length,
    };
  }

  /**
   * Conditional composition: if P then C₁ else C₂
   * @param {Object[]} capabilities
   * @returns {Promise<Object>}
   */
  async composeConditional(capabilities) {
    return {
      type: 'conditional',
      capabilities: capabilities.map(c => c.id),
      properties: capabilities.flatMap(c => c.properties || []),
      effectiveness: Math.max(...capabilities.map(c => c.effectiveness || 50)),
      adaptive: true,
    };
  }

  /**
   * Feedback loop composition: C → M(C) → M²(C) → ...
   * @param {Object[]} capabilities
   * @returns {Promise<Object>}
   */
  async composeFeedbackLoop(capabilities) {
    return {
      type: 'feedback_loop',
      capabilities: capabilities.map(c => c.id),
      properties: capabilities.flatMap(c => c.properties || []),
      effectiveness: capabilities.reduce((sum, c) =>
        sum + (c.effectiveness || 50), 0) / capabilities.length * 2, // Self-improvement bonus
      self_improving: true,
      recursive: true,
    };
  }

  /**
   * Self-improve meta-capability
   * M(M) → M' where effectiveness(M') > effectiveness(M)
   * @param {string} metaCapId
   * @returns {Promise<Object>}
   */
  async selfImprove(metaCapId) {
    const metaCap = this.metaCapabilities.get(metaCapId);
    if (!metaCap) {
      throw new Error(`Meta-capability not found: ${metaCapId}`);
    }

    this.iterationCount++;

    // Check if fixed point reached
    if (this.iterationCount > 10) {
      const updatedMetaCap = {
        ...metaCap,
        fixed_point_reached: true,
      };
      this.metaCapabilities.set(metaCapId, updatedMetaCap);

      return {
        meta_capability_id: metaCapId,
        iteration: this.iterationCount,
        fixed_point_reached: true,
        improvement: 0,
      };
    }

    // Simulate improvement
    const improvement = 1 / (this.iterationCount + 1); // Diminishing returns

    return {
      meta_capability_id: metaCapId,
      iteration: this.iterationCount,
      fixed_point_reached: false,
      improvement,
      next_effectiveness: 100 * (1 - Math.exp(-this.iterationCount / 3)),
    };
  }

  /**
   * Validate capability correctness
   * M_val: C → {valid: bool, issues: []}
   * @param {Object} capability
   * @param {Object} constraints
   * @returns {Object}
   */
  validateCapability(capability, constraints = {}) {
    const issues = [];

    // Check required properties
    if (constraints.required_properties) {
      for (const prop of constraints.required_properties) {
        if (!capability.properties?.includes(prop)) {
          issues.push(`Missing required property: ${prop}`);
        }
      }
    }

    // Check effectiveness threshold
    if (constraints.min_effectiveness) {
      if ((capability.effectiveness || 0) < constraints.min_effectiveness) {
        issues.push(`Effectiveness below threshold: ${capability.effectiveness} < ${constraints.min_effectiveness}`);
      }
    }

    // Check for anti-patterns
    if (constraints.forbidden_properties) {
      for (const prop of constraints.forbidden_properties) {
        if (capability.properties?.includes(prop)) {
          issues.push(`Forbidden property detected: ${prop}`);
        }
      }
    }

    return {
      capability_id: capability.id,
      valid: issues.length === 0,
      issues,
      constraints_checked: Object.keys(constraints).length,
    };
  }

  /**
   * Derive emergent properties from composition
   * @param {string[]} inputProperties
   * @param {Object} context
   * @returns {string[]}
   */
  deriveEmergentProperties(inputProperties, context) {
    const emergent = [];

    // Pattern detection
    if (inputProperties.includes('parallel') && inputProperties.includes('policy')) {
      emergent.push('policy-scoped-parallelism');
    }

    if (inputProperties.includes('checkpointing') && inputProperties.includes('recovery')) {
      emergent.push('time-travel-debugging');
    }

    if (inputProperties.includes('automation') && inputProperties.includes('verification')) {
      emergent.push('verified-automation');
    }

    if (inputProperties.includes('distribution') && inputProperties.includes('composition')) {
      emergent.push('portable-workflows');
    }

    // Context-based properties
    if (context.enables_new_workflow) {
      emergent.push('workflow-enablement');
    }

    if (context.reduces_operator_steps && context.reduces_operator_steps > 2) {
      emergent.push('high-automation');
    }

    return emergent;
  }

  /**
   * Export meta-capability state
   * @returns {Object}
   */
  export() {
    return {
      meta_capabilities: Array.from(this.metaCapabilities.values()),
      generations: this.generations,
      enhancements: this.enhancements,
      stats: {
        total_meta_capabilities: this.metaCapabilities.size,
        generators: Array.from(this.metaCapabilities.values())
          .filter(m => m.type === 'generator').length,
        enhancers: Array.from(this.metaCapabilities.values())
          .filter(m => m.type === 'enhancer').length,
        composers: Array.from(this.metaCapabilities.values())
          .filter(m => m.type === 'composer').length,
        validators: Array.from(this.metaCapabilities.values())
          .filter(m => m.type === 'validator').length,
        capabilities_generated: this.generations.length,
        capabilities_enhanced: this.enhancements.length,
        self_improvement_iterations: this.iterationCount,
      },
    };
  }
}

/**
 * Create meta-capability engine
 * @returns {MetaCapabilityEngine}
 */
export function createMetaCapabilityEngine() {
  return new MetaCapabilityEngine();
}

export default MetaCapabilityEngine;
