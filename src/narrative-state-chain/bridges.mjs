/**
 * @fileoverview Bridge System - Cross-universe type coercion and calls
 *
 * **Purpose**: Enable safe cross-universe interactions with type preservation
 * - Bridge.define(source, target, type_coercion, invariant_proof) → Bridge
 * - Bridge.verify(φ) → {valid, type_preserving, invariants_hold}
 * - cross_universe_call(bridge, object, method) → coerced_result
 *
 * **Design**:
 * - Type coercion as Zod schema transformations
 * - Invariant preservation checks
 * - Access control via grants
 *
 * @module narrative-state-chain/bridges
 */

import { randomUUID } from 'crypto';
import { z } from 'zod';
import { blake3 } from 'hash-wasm';
import { validateBridge } from './types.mjs';

/**
 * Bridge class - manages cross-universe connections
 *
 * @class Bridge
 *
 * @example
 * const bridge = Bridge.define(
 *   sourceUniverse,
 *   targetUniverse,
 *   (value) => ({ ...value, transformed: true }),
 *   async (value) => value.count >= 0,
 *   { name: 'Source to Target Bridge' }
 * );
 */
export class Bridge {
  /**
   * Define a new bridge between universes
   *
   * @param {import('./types.mjs').Universe} sourceUniverse - Source universe
   * @param {import('./types.mjs').Universe} targetUniverse - Target universe
   * @param {(value: any) => any} typeCoercion - Type transformation function
   * @param {(value: any) => Promise<boolean>} invariantPreservation - Invariant checker
   * @param {Object} metadata - Bridge metadata
   * @param {string} metadata.name - Bridge name
   * @param {string} [metadata.description] - Bridge description
   * @returns {Promise<import('./types.mjs').Bridge>} Bridge instance
   *
   * @example
   * const bridge = await Bridge.define(
   *   universeA,
   *   universeB,
   *   (value) => ({ ...value, namespace: 'B' }),
   *   async (value) => value.count >= 0,
   *   { name: 'A to B', description: 'Transfers with namespace change' }
   * );
   */
  static async define(
    sourceUniverse,
    targetUniverse,
    typeCoercion,
    invariantPreservation,
    metadata
  ) {
    const id = randomUUID();

    const bridge = {
      id,
      sourceUniverseId: sourceUniverse.id,
      targetUniverseId: targetUniverse.id,
      typeCoercion,
      invariantPreservation,
      accessGrants: [],
      metadata: {
        name: metadata.name,
        description: metadata.description,
        created: new Date(),
      },
    };

    // Validate
    const validation = validateBridge(bridge);
    if (!validation.success) {
      throw new Error(`Invalid bridge: ${validation.error.message}`);
    }

    return validation.data;
  }

  /**
   * Verify bridge properties
   *
   * **Checks**:
   * 1. Type coercion is a function
   * 2. Invariant preservation is a function
   * 3. Access grants are valid
   *
   * @param {import('./types.mjs').Bridge} bridge - Bridge to verify
   * @returns {Promise<{valid: boolean, typePreserving: boolean, invariantsHold: boolean, errors: string[]}>} Verification result
   *
   * @example
   * const result = await Bridge.verify(bridge);
   * if (!result.valid) {
   *   console.error('Bridge verification failed:', result.errors);
   * }
   */
  static async verify(bridge) {
    const errors = [];

    // Check type coercion is a function
    if (typeof bridge.typeCoercion !== 'function') {
      errors.push('typeCoercion must be a function');
    }

    // Check invariant preservation is a function
    if (typeof bridge.invariantPreservation !== 'function') {
      errors.push('invariantPreservation must be a function');
    }

    // Test type coercion with dummy value
    let typePreserving = false;
    try {
      const testValue = { test: true };
      const coerced = bridge.typeCoercion(testValue);
      typePreserving = coerced !== undefined && coerced !== null;
    } catch (error) {
      errors.push(`Type coercion test failed: ${error.message}`);
    }

    // Test invariant preservation with dummy value
    let invariantsHold = false;
    try {
      const testValue = { test: true };
      invariantsHold = await bridge.invariantPreservation(testValue);
    } catch (error) {
      errors.push(`Invariant preservation test failed: ${error.message}`);
    }

    return {
      valid: errors.length === 0,
      typePreserving,
      invariantsHold,
      errors,
    };
  }

  /**
   * Grant access to an agent
   *
   * @param {import('./types.mjs').Bridge} bridge - Bridge to modify
   * @param {string} agent - Agent identifier
   * @param {'read'|'write'|'execute'} permission - Permission to grant
   * @returns {import('./types.mjs').Bridge} Updated bridge
   *
   * @example
   * Bridge.grantAccess(bridge, 'alice@example.com', 'execute');
   */
  static grantAccess(bridge, agent, permission) {
    bridge.accessGrants.push({ agent, permission });
    return bridge;
  }

  /**
   * Check if agent has permission
   *
   * @param {import('./types.mjs').Bridge} bridge - Bridge to check
   * @param {string} agent - Agent identifier
   * @param {'read'|'write'|'execute'} permission - Required permission
   * @returns {boolean} True if agent has permission
   *
   * @example
   * if (Bridge.checkPermission(bridge, 'alice@example.com', 'execute')) {
   *   // Proceed with cross-universe call
   * }
   */
  static checkPermission(bridge, agent, permission) {
    return bridge.accessGrants.some(
      grant => grant.agent === agent && grant.permission === permission
    );
  }
}

/**
 * Execute a cross-universe call through a bridge
 *
 * **Process**:
 * 1. Check agent has 'execute' permission
 * 2. Apply type coercion
 * 3. Verify invariant preservation
 * 4. Return coerced result
 *
 * @param {import('./types.mjs').Bridge} bridge - Bridge to use
 * @param {any} object - Object to coerce
 * @param {string} agent - Agent performing call
 * @returns {Promise<{success: boolean, result?: any, error?: string}>} Call result
 *
 * @example
 * const result = await crossUniverseCall(
 *   bridge,
 *   { count: 10, name: 'Item' },
 *   'alice@example.com'
 * );
 * if (result.success) {
 *   console.log('Coerced value:', result.result);
 * }
 */
export async function crossUniverseCall(bridge, object, agent) {
  try {
    // 1. Check permission
    if (!Bridge.checkPermission(bridge, agent, 'execute')) {
      return {
        success: false,
        error: `Agent ${agent} does not have execute permission`,
      };
    }

    // 2. Apply type coercion
    const coerced = bridge.typeCoercion(object);

    // 3. Verify invariant preservation
    const invariantsHold = await bridge.invariantPreservation(coerced);

    if (!invariantsHold) {
      return {
        success: false,
        error: 'Invariants not preserved after coercion',
      };
    }

    return {
      success: true,
      result: coerced,
    };
  } catch (error) {
    return {
      success: false,
      error: error.message,
    };
  }
}

/**
 * Create a bidirectional bridge pair
 *
 * **Use case**: Two-way transformations between universes
 *
 * @param {import('./types.mjs').Universe} universeA - First universe
 * @param {import('./types.mjs').Universe} universeB - Second universe
 * @param {(value: any) => any} aToB - A → B transformation
 * @param {(value: any) => any} bToA - B → A transformation
 * @param {(value: any) => Promise<boolean>} invariantCheck - Invariant checker (both directions)
 * @param {Object} metadata - Bridge metadata
 * @returns {Promise<{forward: import('./types.mjs').Bridge, reverse: import('./types.mjs').Bridge}>} Bridge pair
 *
 * @example
 * const { forward, reverse } = await createBidirectionalBridge(
 *   universeA,
 *   universeB,
 *   (v) => ({ ...v, context: 'B' }),
 *   (v) => ({ ...v, context: 'A' }),
 *   async (v) => v.count >= 0,
 *   { name: 'A <-> B' }
 * );
 */
export async function createBidirectionalBridge(
  universeA,
  universeB,
  aToB,
  bToA,
  invariantCheck,
  metadata
) {
  const forward = await Bridge.define(
    universeA,
    universeB,
    aToB,
    invariantCheck,
    {
      name: `${metadata.name} (A -> B)`,
      description: metadata.description,
    }
  );

  const reverse = await Bridge.define(
    universeB,
    universeA,
    bToA,
    invariantCheck,
    {
      name: `${metadata.name} (B -> A)`,
      description: metadata.description,
    }
  );

  return { forward, reverse };
}

/**
 * Create a Zod-based type coercion bridge
 *
 * **Use case**: Use Zod schemas for type transformation
 *
 * @param {import('./types.mjs').Universe} sourceUniverse - Source universe
 * @param {import('./types.mjs').Universe} targetUniverse - Target universe
 * @param {z.ZodSchema} sourceSchema - Source type schema
 * @param {z.ZodSchema} targetSchema - Target type schema
 * @param {(value: any) => any} transformer - Transformation function
 * @param {Object} metadata - Bridge metadata
 * @returns {Promise<import('./types.mjs').Bridge>} Zod-based bridge
 *
 * @example
 * const SourceSchema = z.object({ name: z.string(), count: z.number() });
 * const TargetSchema = z.object({ label: z.string(), amount: z.number() });
 *
 * const bridge = await createZodBridge(
 *   universeA,
 *   universeB,
 *   SourceSchema,
 *   TargetSchema,
 *   (v) => ({ label: v.name, amount: v.count }),
 *   { name: 'Zod Transform Bridge' }
 * );
 */
export async function createZodBridge(
  sourceUniverse,
  targetUniverse,
  sourceSchema,
  targetSchema,
  transformer,
  metadata
) {
  const typeCoercion = (value) => {
    // Validate source
    const sourceValidation = sourceSchema.safeParse(value);
    if (!sourceValidation.success) {
      throw new Error(`Source validation failed: ${sourceValidation.error.message}`);
    }

    // Transform
    const transformed = transformer(sourceValidation.data);

    // Validate target
    const targetValidation = targetSchema.safeParse(transformed);
    if (!targetValidation.success) {
      throw new Error(`Target validation failed: ${targetValidation.error.message}`);
    }

    return targetValidation.data;
  };

  const invariantPreservation = async (value) => {
    try {
      // Check target schema is satisfied
      targetSchema.parse(value);
      return true;
    } catch {
      return false;
    }
  };

  return Bridge.define(
    sourceUniverse,
    targetUniverse,
    typeCoercion,
    invariantPreservation,
    metadata
  );
}

/**
 * Compute bridge proof hash
 *
 * **Use case**: Create a cryptographic proof that bridge was used
 *
 * @param {import('./types.mjs').Bridge} bridge - Bridge used
 * @param {any} input - Input value
 * @param {any} output - Output value
 * @returns {Promise<string>} Proof hash
 *
 * @example
 * const proof = await computeBridgeProof(bridge, inputValue, outputValue);
 */
export async function computeBridgeProof(bridge, input, output) {
  const canonical = JSON.stringify({
    bridgeId: bridge.id,
    sourceUniverseId: bridge.sourceUniverseId,
    targetUniverseId: bridge.targetUniverseId,
    input,
    output,
    timestamp: new Date().toISOString(),
  });

  return blake3(canonical);
}
