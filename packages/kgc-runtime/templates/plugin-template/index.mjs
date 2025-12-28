/**
 * Example KGC Runtime Plugin
 *
 * This is a template for creating custom KGC plugins.
 * Copy this template and modify to create your own plugin.
 */

import { z } from 'zod';
import { PluginReceiptSchema } from '@unrdf/kgc-runtime/schemas';

/**
 * Input validation schema
 */
const InputSchema = z.object({
  operation: z.string().min(1).max(100),
  data: z.record(z.any()).optional(),
});

/**
 * Plugin entry point
 *
 * @param {Object} runtime - KGC Runtime API (whitelisted methods only)
 * @returns {Object} Plugin interface
 */
export default function examplePlugin(runtime) {
  // Private state (encapsulated)
  let initialized = false;
  let callCount = 0;

  return {
    name: 'example-plugin',
    version: '1.0.0',

    /**
     * Initialize plugin
     * Called when plugin transitions to LOADED state
     */
    async initialize() {
      console.log('[example-plugin] Initializing...');

      // Perform any setup here
      // - Validate runtime API
      // - Initialize state
      // - Connect to resources (within capability limits)

      initialized = true;
      console.log('[example-plugin] Initialized successfully');
    },

    /**
     * Generate custom receipt
     *
     * @param {string} operation - Operation name
     * @param {Object} inputs - Operation inputs
     * @param {Object} outputs - Operation outputs
     * @returns {Promise<Object>} Custom receipt
     */
    async generateCustomReceipt(operation, inputs = {}, outputs = {}) {
      if (!initialized) {
        throw new Error('Plugin not initialized');
      }

      // Validate inputs
      InputSchema.parse({ operation, data: inputs });

      // Use runtime API to generate base receipt
      const baseReceipt = await runtime.generateReceipt(
        operation,
        inputs,
        outputs
      );

      callCount++;

      // Extend with custom metadata
      const customReceipt = {
        ...baseReceipt,
        pluginMetadata: {
          pluginName: 'example-plugin',
          pluginVersion: '1.0.0',
          receiptType: 'custom-example',
          customFields: {
            callCount,
            timestamp: Date.now(),
            processingNotes: 'Processed by example plugin',
          },
        },
      };

      // Validate against plugin receipt schema
      return PluginReceiptSchema.parse(customReceipt);
    },

    /**
     * Validate custom receipt
     *
     * @param {Object} receipt - Receipt to validate
     * @returns {Promise<Object>} Validation result
     */
    async validateCustomReceipt(receipt) {
      if (!initialized) {
        throw new Error('Plugin not initialized');
      }

      try {
        // Validate schema
        const validated = PluginReceiptSchema.parse(receipt);

        // Custom validation logic
        if (validated.pluginMetadata?.pluginName !== 'example-plugin') {
          return {
            valid: false,
            errors: ['Receipt not from example-plugin'],
          };
        }

        // Use runtime validation
        const baseValidation = await runtime.validateReceipt(receipt);

        return {
          valid: baseValidation.success,
          errors: baseValidation.errors || [],
          metadata: validated.pluginMetadata,
        };
      } catch (error) {
        return {
          valid: false,
          errors: [error.message],
        };
      }
    },

    /**
     * Get plugin statistics
     *
     * @returns {Object} Plugin stats
     */
    getStats() {
      return {
        initialized,
        callCount,
        version: '1.0.0',
      };
    },

    /**
     * Cleanup on unload
     * Called when plugin transitions to UNLOADED state
     */
    async cleanup() {
      console.log('[example-plugin] Cleaning up...');

      // Perform cleanup:
      // - Close connections
      // - Release resources
      // - Save state if needed

      initialized = false;
      console.log('[example-plugin] Cleanup complete');
    },
  };
}
