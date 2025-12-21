/**
 * Hook Registry - Generic Validation System
 *
 * Extensible predicate-based validation hooks for field-level data governance.
 * Maps field identifiers to validation functions that return { valid, reason? }.
 *
 * Usage:
 * ```javascript
 * const registry = new HookRegistry();
 * registry.register('budget', { validate: (v) => v <= 100000 });
 * const result = registry.validate('budget', 50000);
 * ```
 */

/**
 *
 */
export class HookRegistry {
  /**
   *
   */
  constructor() {
    this.hooks = new Map();
  }

  /**
   * Register a validation hook for a field
   * @param {string} fieldId - Unique field identifier
   * @param {Object} hook - Hook definition
   * @param {Function} hook.validate - Validation function: (value) => { valid: boolean, reason?: string }
   * @example
   * registry.register('status', {
   *   validate: (value) => {
   *     const allowed = ['active', 'inactive'];
   *     return allowed.includes(value)
   *       ? { valid: true }
   *       : { valid: false, reason: `Must be one of: ${allowed.join(', ')}` };
   *   }
   * });
   */
  register(fieldId, hook) {
    if (!hook.validate || typeof hook.validate !== 'function') {
      throw new Error(`Hook for field "${fieldId}" must have a validate function`);
    }
    this.hooks.set(fieldId, hook);
  }

  /**
   * Unregister a validation hook
   * @param {string} fieldId
   */
  unregister(fieldId) {
    this.hooks.delete(fieldId);
  }

  /**
   * Get registered hook for a field
   * @param {string} fieldId
   * @returns {Object|null}
   */
  get(fieldId) {
    return this.hooks.get(fieldId) || null;
  }

  /**
   * List all registered field IDs
   * @returns {string[]}
   */
  listFields() {
    return Array.from(this.hooks.keys());
  }

  /**
   * Validate a single field value
   * @param {string} fieldId
   * @param {*} value - Value to validate
   * @returns {Object} { valid: boolean, reason?: string }
   */
  validate(fieldId, value) {
    const hook = this.hooks.get(fieldId);
    if (!hook) {
      // No hook registered = valid by default
      return { valid: true };
    }

    try {
      const result = hook.validate(value);

      // Ensure result has valid property
      if (typeof result.valid !== 'boolean') {
        throw new Error(`Hook for field "${fieldId}" must return { valid: boolean }`);
      }

      return result;
    } catch (error) {
      return {
        valid: false,
        reason: `Validation error for field "${fieldId}": ${error.message}`,
      };
    }
  }

  /**
   * Validate multiple field values
   * @param {Object} values - Key-value pairs of fieldId -> value
   * @returns {Object} { valid: boolean, errors?: Object }
   */
  validateBatch(values) {
    const errors = {};

    for (const [fieldId, value] of Object.entries(values)) {
      const result = this.validate(fieldId, value);
      if (!result.valid) {
        errors[fieldId] = result.reason;
      }
    }

    return {
      valid: Object.keys(errors).length === 0,
      errors: Object.keys(errors).length > 0 ? errors : undefined,
    };
  }

  /**
   * Create a batch validator function
   * @returns {Function} (values) => { valid, errors? }
   */
  createValidator() {
    return (values) => this.validateBatch(values);
  }
}
