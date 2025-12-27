/**
 * @fileoverview Deterministic CLI extension registry for UNRDF workspace packages.
 *
 * Core responsibilities:
 * - Manage extension contracts (Zod schemas)
 * - Register extensions with collision detection
 * - Build deterministic command tree
 * - Enforce poka-yoke boundaries (validation before side effects)
 *
 * Extension loading order (Λ) is ≺-total (strictly ordered, no ambiguity).
 * If collisions occur, registry fails closed unless manifest declares override.
 */

import { z } from 'zod';

/**
 * Extension contract schema - every extension must satisfy this.
 * @type {z.ZodSchema}
 */
export const ExtensionSchema = z.object({
  id: z.string().describe('Package name (e.g., @unrdf/kgc-4d)'),
  nouns: z.record(
    z.string().describe('Noun (e.g., snapshot, receipt)'),
    z.object({
      description: z.string().optional(),
      verbs: z.record(
        z.string().describe('Verb (e.g., create, restore)'),
        z.object({
          description: z.string(),
          handler: z.function().describe('Handler function (args) => Promise<any>'),
          argsSchema: z.custom((val) => true).optional().describe('Zod schema for args validation'),
          meta: z.record(z.any()).optional().describe('Command metadata')
        })
      )
    })
  ),
  priority: z.number().default(100).describe('Precedence for collision resolution (lower wins)'),
  guards: z.object({
    refusals: z.array(z.string()).optional().describe('Patterns this extension refuses'),
    preconditions: z.function().optional().describe('Validate before loading')
  }).optional(),
  receipts: z.object({
    success: z.record(z.any()).optional().describe('Shape of successful response'),
    error: z.record(z.any()).optional().describe('Shape of error response')
  }).optional()
});

/**
 * Registry manages extension lifecycle.
 *
 * @class Registry
 */
export class Registry {
  /**
   * @param {Object} options - Registry configuration
   * @param {Array<{rule: string, package: string, winner: string}>} options.overrides - Collision overrides from manifest
   * @param {number} options.failOnCollision - Fail if collision not explicitly overridden (default: true)
   */
  constructor(options = {}) {
    /** @type {Map<string, any>} Extensions keyed by id */
    this.extensions = new Map();

    /** @type {Map<string, Array<{ext: any, noun: string, verb: string}>>} Collision tracking */
    this.collisions = new Map();

    /** @type {Map<string, string>} Noun ownership for poka-yoke */
    this.ownership = new Map();

    /** @type {Array<{rule: string, package: string, winner: string}>} Override rules */
    this.overrides = options.overrides || [];

    /** @type {boolean} Fail on unresolved collisions */
    this.failOnCollision = options.failOnCollision !== false;
  }

  /**
   * Register a single extension.
   *
   * Validates against contract, checks for collisions, applies overrides.
   * FAILS CLOSED if collision detected and not overridden.
   *
   * @param {any} extension - Extension object (will be validated)
   * @param {number} loadOrder - Load sequence (for stable deterministic ordering)
   * @throws {Error} If extension invalid, guards fail, or collision unresolved
   */
  registerExtension(extension, loadOrder = Infinity) {
    // Validate contract
    const validation = ExtensionSchema.safeParse(extension);
    if (!validation.success) {
      throw new Error(
        `Invalid extension ${extension?.id}: ${validation.error.message}`
      );
    }

    const ext = validation.data;
    const fullId = `${ext.id}@${loadOrder}`;

    // Check guards (preconditions)
    if (ext.guards?.preconditions) {
      try {
        ext.guards.preconditions();
      } catch (e) {
        throw new Error(
          `Extension ${ext.id} guard failed: ${e.message}`
        );
      }
    }

    // Check for collisions in noun/verb space
    for (const [noun, nounData] of Object.entries(ext.nouns)) {
      for (const verb of Object.keys(nounData.verbs)) {
        const key = `${noun}:${verb}`;

        if (this.ownership.has(key)) {
          const existing = this.ownership.get(key);
          const collision = { key, existing, new: ext.id, newLoadOrder: loadOrder };

          // Check if override rule exists
          const override = this._findOverride(collision);
          if (!override) {
            if (this.failOnCollision) {
              throw new Error(
                `Collision: ${key} claimed by both ${existing} and ${ext.id}. ` +
                `Add override rule to manifest.`
              );
            } else {
              // Track but don't fail
              if (!this.collisions.has(key)) {
                this.collisions.set(key, []);
              }
              this.collisions.get(key).push({ ext, noun, verb });
              continue;
            }
          }

          // If override says new wins, update ownership
          if (override.winner === ext.id) {
            this.ownership.set(key, ext.id);
          }
        } else {
          this.ownership.set(key, ext.id);
        }
      }
    }

    // Store extension
    this.extensions.set(ext.id, { ...ext, _loadOrder: loadOrder });
  }

  /**
   * Find override rule for collision.
   * @private
   */
  _findOverride(collision) {
    return this.overrides.find(
      o => o.rule === collision.key &&
           (o.package === collision.existing || o.package === collision.new)
    );
  }

  /**
   * Build the command tree from registered extensions.
   *
   * Returns structure:
   * ```
   * {
   *   nouns: {
   *     snapshot: {
   *       description: "...",
   *       verbs: {
   *         create: { handler, argsSchema, meta },
   *         restore: { handler, argsSchema, meta }
   *       }
   *     }
   *   }
   * }
   * ```
   *
   * @returns {Object} Command tree ready for Citty registration
   */
  buildCommandTree() {
    const tree = { nouns: {} };

    // Process extensions in load order (stable, deterministic)
    const sorted = Array.from(this.extensions.values()).sort(
      (a, b) => (a._loadOrder || Infinity) - (b._loadOrder || Infinity)
    );

    for (const ext of sorted) {
      for (const [noun, nounData] of Object.entries(ext.nouns)) {
        if (!tree.nouns[noun]) {
          tree.nouns[noun] = {
            description: nounData.description || `${noun} commands`,
            verbs: {}
          };
        }

        for (const [verb, verbData] of Object.entries(nounData.verbs)) {
          tree.nouns[noun].verbs[verb] = {
            handler: verbData.handler,
            argsSchema: verbData.argsSchema,
            meta: verbData.meta || {},
            _source: ext.id // Track which package provided this
          };
        }
      }
    }

    return tree;
  }

  /**
   * Get collision summary (for error reporting).
   *
   * @returns {Object} Map of collisions with their status
   */
  getCollisionSummary() {
    return Object.fromEntries(this.collisions);
  }

  /**
   * List all registered noun:verb pairs.
   *
   * @returns {Array<string>} Stable-sorted list of all commands
   */
  listCommands() {
    return Array.from(this.ownership.keys()).sort();
  }

  /**
   * Get the source extension for a specific command.
   *
   * @param {string} noun
   * @param {string} verb
   * @returns {string|undefined} Extension id that provides this command
   */
  getCommandSource(noun, verb) {
    return this.ownership.get(`${noun}:${verb}`);
  }

  /**
   * Validate that all commands have required schemas and handlers.
   * Used by test suite for contract verification.
   *
   * @returns {Array<{noun:string, verb:string, issue:string}>} Validation errors
   */
  validateContracts() {
    const errors = [];

    for (const ext of this.extensions.values()) {
      for (const [noun, nounData] of Object.entries(ext.nouns)) {
        for (const [verb, verbData] of Object.entries(nounData.verbs)) {
          if (!verbData.handler || typeof verbData.handler !== 'function') {
            errors.push({
              noun, verb,
              source: ext.id,
              issue: 'Missing or invalid handler'
            });
          }
          if (!verbData.description) {
            errors.push({
              noun, verb,
              source: ext.id,
              issue: 'Missing description'
            });
          }
          // argsSchema is optional but if present must be Zod schema
          if (verbData.argsSchema && typeof verbData.argsSchema.parse !== 'function') {
            errors.push({
              noun, verb,
              source: ext.id,
              issue: 'argsSchema must be Zod schema (missing .parse method)'
            });
          }
        }
      }
    }

    return errors;
  }
}

/**
 * JSON envelope for deterministic --json output.
 *
 * Success: { ok: true, data, meta }
 * Error:   { ok: false, code, message, details?, hint? }
 *
 * @param {boolean} ok - Success/failure
 * @param {Object} payload - Data or error details
 * @param {Object} meta - Command metadata (optional)
 * @returns {Object} Envelope ready for JSON serialization
 */
export function createEnvelope(ok, payload, meta = {}) {
  if (ok) {
    return {
      ok: true,
      data: payload,
      meta: {
        timestamp: new Date().toISOString(),
        ...meta
      }
    };
  } else {
    return {
      ok: false,
      code: payload.code || 'COMMAND_ERROR',
      message: payload.message || 'Unknown error',
      ...(payload.details && { details: payload.details }),
      ...(payload.hint && { hint: payload.hint }),
      meta: {
        timestamp: new Date().toISOString(),
        ...meta
      }
    };
  }
}

export default Registry;
