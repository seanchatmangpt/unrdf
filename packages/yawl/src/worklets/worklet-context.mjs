/**
 * @file Worklet Context - Data passing between parent and worklet
 * @module @unrdf/yawl/worklets/context
 *
 * @description
 * Manages data flow between parent workflow and worklet:
 * - Input mapping (parent data → worklet)
 * - Output mapping (worklet result → parent)
 * - Isolation boundaries (what worklet can/cannot access)
 * - Context serialization for receipt logging
 */

import { z } from 'zod';

// ============================================================================
// SCHEMAS
// ============================================================================

/**
 * Context isolation mode
 */
export const IsolationModeSchema = z.enum([
  'full',      // Worklet gets full parent case data
  'filtered',  // Worklet gets filtered subset of data
  'none',      // Worklet gets only exception data (no case data)
]);

/**
 * Data mapping rule
 */
export const MappingRuleSchema = z.object({
  source: z.string().min(1),           // Source field path
  target: z.string().min(1),           // Target field path
  transform: z.function().optional(),  // Optional transformation function
  required: z.boolean().default(false),
});

/**
 * Context configuration
 */
export const ContextConfigSchema = z.object({
  isolationMode: IsolationModeSchema.default('filtered'),
  inputMappings: z.array(MappingRuleSchema).default([]),
  outputMappings: z.array(MappingRuleSchema).default([]),
  allowedFields: z.array(z.string()).optional(),
  deniedFields: z.array(z.string()).optional(),
});

/**
 * @typedef {z.infer<typeof ContextConfigSchema>} ContextConfig
 * @typedef {z.infer<typeof MappingRuleSchema>} MappingRule
 */

// ============================================================================
// WORKLET CONTEXT MANAGER
// ============================================================================

/**
 * Worklet Context Manager - Controls data flow
 *
 * @example
 * const contextMgr = new WorkletContextManager({
 *   isolationMode: 'filtered',
 *   inputMappings: [
 *     { source: 'amount', target: 'requestAmount' },
 *     { source: 'approver', target: 'originalApprover' }
 *   ],
 *   outputMappings: [
 *     { source: 'approver', target: 'escalatedApprover' },
 *     { source: 'approvalDate', target: 'approvedAt' }
 *   ],
 *   allowedFields: ['amount', 'approver', 'customer']
 * });
 *
 * // Create worklet context
 * const context = contextMgr.createWorkletContext(
 *   parentCase,
 *   exception
 * );
 *
 * // Map worklet output to parent
 * const updatedParentData = contextMgr.mapOutputToParent(
 *   parentCase.data,
 *   workletResult.outputData
 * );
 */
export class WorkletContextManager {
  /**
   * Create context manager
   * @param {ContextConfig} [config] - Context configuration
   */
  constructor(config = {}) {
    this.config = ContextConfigSchema.parse(config);
  }

  /**
   * Create worklet context from parent case and exception
   *
   * @param {Object} parentCase - Parent case instance
   * @param {Object} exception - Exception details
   * @returns {Object} Worklet context
   *
   * @example
   * const context = contextMgr.createWorkletContext(
   *   { id: 'case-1', data: { amount: 5000, approver: 'mgr-1' } },
   *   { type: 'timeout', durationMs: 120000 }
   * );
   * // Returns: {
   * //   caseData: { requestAmount: 5000, originalApprover: 'mgr-1' },
   * //   exceptionData: { type: 'timeout', durationMs: 120000 }
   * // }
   */
  createWorkletContext(parentCase, exception) {
    // Filter parent case data based on isolation mode
    let caseData = {};

    switch (this.config.isolationMode) {
      case 'full':
        caseData = { ...parentCase.data };
        break;

      case 'filtered':
        caseData = this._filterData(parentCase.data);
        break;

      case 'none':
        // No case data passed
        break;
    }

    // Apply input mappings
    caseData = this._applyMappings(caseData, this.config.inputMappings);

    return {
      caseData,
      exceptionData: exception,
    };
  }

  /**
   * Map worklet output back to parent case data
   *
   * @param {Object} parentData - Current parent case data
   * @param {Object} workletOutput - Worklet output data
   * @returns {Object} Updated parent case data
   *
   * @example
   * const updatedData = contextMgr.mapOutputToParent(
   *   { amount: 5000, approver: 'mgr-1' },
   *   { approver: 'vp-1', approvalDate: '2026-01-11' }
   * );
   * // Returns: {
   * //   amount: 5000,
   * //   escalatedApprover: 'vp-1',
   * //   approvedAt: '2026-01-11'
   * // }
   */
  mapOutputToParent(parentData, workletOutput) {
    if (!workletOutput) {
      return parentData;
    }

    const updatedData = { ...parentData };

    // Apply output mappings
    for (const mapping of this.config.outputMappings) {
      const value = this._getFieldValue(workletOutput, mapping.source);

      if (value !== undefined) {
        const transformedValue = mapping.transform
          ? mapping.transform(value)
          : value;

        this._setFieldValue(updatedData, mapping.target, transformedValue);
      } else if (mapping.required) {
        throw new Error(`Required output field missing: ${mapping.source}`);
      }
    }

    return updatedData;
  }

  /**
   * Filter data based on allowed/denied fields
   * @private
   */
  _filterData(data) {
    const filtered = {};

    for (const [key, value] of Object.entries(data)) {
      // Check denied fields first
      if (this.config.deniedFields?.includes(key)) {
        continue;
      }

      // Check allowed fields
      if (this.config.allowedFields) {
        if (this.config.allowedFields.includes(key)) {
          filtered[key] = value;
        }
      } else {
        // No allowed list = allow all (except denied)
        filtered[key] = value;
      }
    }

    return filtered;
  }

  /**
   * Apply data mappings
   * @private
   */
  _applyMappings(data, mappings) {
    const mapped = { ...data };

    for (const mapping of mappings) {
      const value = this._getFieldValue(data, mapping.source);

      if (value !== undefined) {
        const transformedValue = mapping.transform
          ? mapping.transform(value)
          : value;

        this._setFieldValue(mapped, mapping.target, transformedValue);
      } else if (mapping.required) {
        throw new Error(`Required input field missing: ${mapping.source}`);
      }
    }

    return mapped;
  }

  /**
   * Get field value by path (supports nested paths like "user.profile.name")
   * @private
   */
  _getFieldValue(data, path) {
    const parts = path.split('.');
    let value = data;

    for (const part of parts) {
      if (value === null || value === undefined) {
        return undefined;
      }
      value = value[part];
    }

    return value;
  }

  /**
   * Set field value by path
   * @private
   */
  _setFieldValue(data, path, value) {
    const parts = path.split('.');
    let current = data;

    for (let i = 0; i < parts.length - 1; i++) {
      const part = parts[i];
      if (!(part in current)) {
        current[part] = {};
      }
      current = current[part];
    }

    current[parts[parts.length - 1]] = value;
  }

  /**
   * Serialize context for receipt logging
   * @param {Object} context - Worklet context
   * @returns {Object} Serialized context (safe for logging)
   */
  serializeContext(context) {
    return {
      caseData: this._sanitizeForLogging(context.caseData),
      exceptionData: this._sanitizeForLogging(context.exceptionData),
      isolationMode: this.config.isolationMode,
    };
  }

  /**
   * Sanitize data for logging (remove sensitive fields)
   * @private
   */
  _sanitizeForLogging(data) {
    const sensitiveFields = ['password', 'token', 'secret', 'apiKey', 'ssn', 'creditCard'];
    const sanitized = { ...data };

    for (const field of sensitiveFields) {
      if (field in sanitized) {
        sanitized[field] = '[REDACTED]';
      }
    }

    return sanitized;
  }

  /**
   * Validate context data against schema
   * @param {Object} context - Context to validate
   * @param {Object} schema - Zod schema
   * @returns {boolean} True if valid
   * @throws {Error} If validation fails
   */
  validateContext(context, schema) {
    schema.parse(context);
    return true;
  }
}

/**
 * Create worklet context manager
 * @param {ContextConfig} [config] - Context configuration
 * @returns {WorkletContextManager} Context manager instance
 */
export function createWorkletContext(config) {
  return new WorkletContextManager(config);
}
