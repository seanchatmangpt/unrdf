/**
 * @fileoverview V6 CLI Spine Builder
 *
 * Integrates canonical nouns/verbs with existing citty-based registry.
 * Builds unified command tree ensuring all commands emit JSON envelopes or receipts.
 */

import { CANONICAL_NOUNS, getNoun } from './nouns.mjs';
import { CANONICAL_VERBS, getVerbsForNoun, isValidCombination } from './verbs.mjs';

/**
 * Build V6 spine for existing registry.
 *
 * Augments registry with V6 canonical noun-verb structure.
 * Validates that registered commands align with V6 ontology.
 *
 * @param {Registry} registry - Existing kgc-cli registry
 * @returns {Object} V6-enhanced command tree
 */
export function buildV6Spine(registry) {
  const tree = registry.buildCommandTree();
  const validationErrors = [];
  const coverage = {};

  // Initialize coverage map
  for (const noun of CANONICAL_NOUNS) {
    coverage[noun.name] = {
      expected: getVerbsForNoun(noun.name).map(v => v.name),
      registered: [],
      missing: []
    };
  }

  // Validate existing registrations against V6 ontology
  for (const [noun, nounData] of Object.entries(tree.nouns)) {
    const nounDef = getNoun(noun);

    if (!nounDef) {
      validationErrors.push({
        type: 'UNKNOWN_NOUN',
        noun,
        message: `Noun '${noun}' not in V6 canonical set`
      });
      continue;
    }

    for (const [verb, verbData] of Object.entries(nounData.verbs)) {
      if (!isValidCombination(noun, verb)) {
        validationErrors.push({
          type: 'INVALID_COMBINATION',
          noun,
          verb,
          message: `Verb '${verb}' not applicable to noun '${noun}'`
        });
      } else {
        coverage[noun].registered.push(verb);
      }
    }
  }

  // Calculate missing verbs
  for (const noun of Object.keys(coverage)) {
    coverage[noun].missing = coverage[noun].expected.filter(
      v => !coverage[noun].registered.includes(v)
    );
  }

  return {
    tree,
    validationErrors,
    coverage,
    stats: {
      totalNouns: CANONICAL_NOUNS.length,
      registeredNouns: Object.keys(tree.nouns).length,
      totalVerbs: CANONICAL_VERBS.length,
      registeredCommands: Object.values(coverage).reduce(
        (sum, n) => sum + n.registered.length,
        0
      ),
      missingCommands: Object.values(coverage).reduce(
        (sum, n) => sum + n.missing.length,
        0
      )
    }
  };
}

/**
 * Generate V6 spine report.
 *
 * Human-readable summary of V6 coverage and validation.
 *
 * @param {Object} spine - Result from buildV6Spine
 * @returns {string} Formatted report
 */
export function generateSpineReport(spine) {
  const lines = [];

  lines.push('=== V6 CLI Spine Report ===\n');

  // Stats
  lines.push('Statistics:');
  lines.push(`  Total Nouns: ${spine.stats.totalNouns}`);
  lines.push(`  Registered Nouns: ${spine.stats.registeredNouns}`);
  lines.push(`  Total Verbs: ${spine.stats.totalVerbs}`);
  lines.push(`  Registered Commands: ${spine.stats.registeredCommands}`);
  lines.push(`  Missing Commands: ${spine.stats.missingCommands}\n`);

  // Validation errors
  if (spine.validationErrors.length > 0) {
    lines.push('Validation Errors:');
    for (const err of spine.validationErrors) {
      lines.push(`  [${err.type}] ${err.message}`);
    }
    lines.push('');
  }

  // Coverage by noun
  lines.push('Coverage by Noun:');
  for (const [noun, cov] of Object.entries(spine.coverage)) {
    const coverage = cov.expected.length > 0
      ? ((cov.registered.length / cov.expected.length) * 100).toFixed(1)
      : '0.0';

    lines.push(`  ${noun}: ${coverage}% (${cov.registered.length}/${cov.expected.length})`);

    if (cov.missing.length > 0) {
      lines.push(`    Missing: ${cov.missing.join(', ')}`);
    }
  }

  return lines.join('\n');
}

/**
 * Get noun-verb matrix for documentation.
 *
 * Returns complete mapping of all noun-verb combinations.
 *
 * @returns {Object} Matrix with metadata
 */
export function getNounVerbMatrix() {
  const matrix = {};

  for (const noun of CANONICAL_NOUNS) {
    const verbs = getVerbsForNoun(noun.name);
    matrix[noun.name] = {
      description: noun.description,
      package: noun.package,
      verbs: verbs.map(v => ({
        name: v.name,
        description: v.description,
        emitsReceipt: v.emitsReceipt,
        sideEffects: v.sideEffects,
        requiresAuth: v.requiresAuth
      }))
    };
  }

  return matrix;
}

/**
 * Validate that command emits proper receipt.
 *
 * Wraps command handler to ensure receipt emission.
 *
 * @param {string} noun - Noun name
 * @param {string} verb - Verb name
 * @param {Function} handler - Original handler
 * @returns {Function} Wrapped handler
 */
export function wrapWithReceiptValidation(noun, verb, handler) {
  return async (args, ctx) => {
    // Use context timestamp if available for determinism
    const startTime = ctx?.t_ns ? Number(ctx.t_ns / 1_000_000n) : Date.now();

    try {
      const result = await handler(args, ctx);
      const endTime = ctx?.t_ns_end ? Number(ctx.t_ns_end / 1_000_000n) : Date.now();
      const duration = endTime - startTime;

      // Ensure result includes receipt metadata
      return {
        ...result,
        _receipt: {
          noun,
          verb,
          timestamp: new Date(startTime).toISOString(),
          duration,
          status: 'success'
        }
      };
    } catch (error) {
      const endTime = ctx?.t_ns_end ? Number(ctx.t_ns_end / 1_000_000n) : Date.now();
      const duration = endTime - startTime;

      throw {
        ...error,
        _receipt: {
          noun,
          verb,
          timestamp: new Date(startTime).toISOString(),
          duration,
          status: 'error',
          error: error.message
        }
      };
    }
  };
}

/**
 * Create extension definition for V6 commands.
 *
 * Helper to create registry-compatible extensions following V6 patterns.
 *
 * @param {Object} config - Extension configuration
 * @returns {Object} Registry extension
 */
export function createV6Extension({ id, nouns }) {
  const extension = {
    id,
    nouns: {},
    priority: 100,
    receipts: {
      success: { ok: true, data: {}, _receipt: {} },
      error: { ok: false, code: '', message: '', _receipt: {} }
    }
  };

  for (const [nounName, nounConfig] of Object.entries(nouns)) {
    const nounDef = getNoun(nounName);
    if (!nounDef) {
      throw new Error(`Unknown noun: ${nounName}`);
    }

    extension.nouns[nounName] = {
      description: nounConfig.description || nounDef.description,
      verbs: {}
    };

    for (const [verbName, verbConfig] of Object.entries(nounConfig.verbs)) {
      if (!isValidCombination(nounName, verbName)) {
        throw new Error(`Invalid combination: ${nounName}:${verbName}`);
      }

      extension.nouns[nounName].verbs[verbName] = {
        description: verbConfig.description,
        handler: wrapWithReceiptValidation(nounName, verbName, verbConfig.handler),
        argsSchema: verbConfig.argsSchema,
        meta: verbConfig.meta || {}
      };
    }
  }

  return extension;
}

export default {
  buildV6Spine,
  generateSpineReport,
  getNounVerbMatrix,
  wrapWithReceiptValidation,
  createV6Extension
};
