/**
 * @fileoverview useValidator composable - SHACL validation helpers
 *
 * Provides a thin, opinionated wrapper around the shared RdfEngine so
 * contributors always reach for the same validation workflow.
 */

import { Parser, Store } from '@unrdf/core/rdf/n3-justified-only';
import { useStoreContext } from '../context/index.mjs';

/**
 * Normalise incoming data into an N3.Store without mutating the shared store.
 * @param {string|import('n3').Store|Array} input - Data to coerce into a store
 * @param {import('n3').Store} fallback - Default store when input is falsy
 * @param {string} baseIRI - Base IRI for Turtle parsing
 * @returns {import('n3').Store}
 */
function toStore(input, fallback, baseIRI) {
  if (!input) {
    return fallback;
  }

  if (typeof input === 'string') {
    const parser = new Parser({ baseIRI });
    return new Store(parser.parse(input));
  }

  if (typeof input === 'object') {
    if (typeof input.getQuads === 'function') {
      return input;
    }

    if (Array.isArray(input)) {
      return new Store(input);
    }
  }

  throw new TypeError('[useValidator] Expected Turtle string, Store, or quad array');
}

/**
 * Create a SHACL validator composable bound to the active store context.
 *
 * @param {Object} [options] - Validator options (reserved for future use)
 * @returns {Object} Validator interface
 */
export function useValidator(_options = {}) {
  const storeContext = useStoreContext();
  const { engine, store } = storeContext;
  const baseIRI = engine?.baseIRI || 'http://example.org/';

  function runValidation(dataInput, shapesInput, validateOptions = {}) {
    if (!shapesInput) {
      throw new TypeError('[useValidator] Shapes are required for validation');
    }

    const dataStore = toStore(dataInput, store, baseIRI);
    const shapesStore = toStore(shapesInput, null, baseIRI);
    if (!shapesStore) {
      throw new TypeError('[useValidator] Unable to create shapes store');
    }

    const report = engine.validateShacl(dataStore, shapesStore);

    if (validateOptions.asJson) {
      return {
        conforms: report.conforms,
        results: report.results.map(entry => ({
          message: entry.message,
          path: entry.path,
          focusNode: entry.focusNode,
        })),
      };
    }

    return report;
  }

  return {
    /**
     * Validate data against SHACL shapes.
     *
     * @param {import('n3').Store|string|Array} data - Data store or Turtle string. Defaults to current store.
     * @param {import('n3').Store|string|Array} shapes - SHACL shapes definition.
     * @param {Object} [validateOptions] - Optional tweaks (e.g., `{ asJson: true }`).
     * @returns {Object} Validation report.
     */
    validate(data, shapes, validateOptions = {}) {
      if (shapes === undefined) {
        return runValidation(store, data, validateOptions);
      }
      return runValidation(data || store, shapes, validateOptions);
    },

    /**
     * Validate data and throw when the report does not conform.
     * @returns {Object} Validation report when successful.
     * @throws {Error} When validation fails.
     */
    validateOrThrow(data, shapes, validateOptions = {}) {
      const report = this.validate(data, shapes, validateOptions);
      if (!report.conforms) {
        const messages = report.results
          .map(entry => entry.message)
          .filter(Boolean)
          .join('; ');
        throw new Error(
          messages
            ? `[useValidator] SHACL validation failed: ${messages}`
            : '[useValidator] SHACL validation failed'
        );
      }
      return report;
    },

    /**
     * Expose the underlying RdfEngine for advanced scenarios.
     */
    getEngine() {
      return engine;
    },
  };
}
