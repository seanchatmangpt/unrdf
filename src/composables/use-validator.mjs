/**
 * @fileoverview useValidator composable - SHACL validation operations with context
 * 
 * This composable provides SHACL validation capabilities for RDF graphs.
 * It enforces the "One Validator Rule" - SHACL is the only validation method.
 * Now uses unctx for default store access.
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { useStoreContext } from "../context/index.mjs";

/**
 * Create a SHACL validator composable
 * 
 * @param {Object} [options] - Validator options
 * @param {number} [options.timeoutMs=30000] - Validation timeout
 * @param {Function} [options.onMetric] - Metrics callback
 * @returns {Object} Validator interface
 * 
 * @example
 * // Initialize store context first
 * const runApp = initStore();
 * 
 * runApp(() => {
 *   const validator = useValidator();
 *   
 *   // Validate the context store against SHACL shapes
 *   const report = await validator.validate(shapesStore);
 *   
 *   // Validate and throw on failure
 *   await validator.validateOrThrow(shapesTurtle);
 * });
 * 
 * @throws {Error} If store context is not initialized
 */
export function useValidator(options = {}) {
  const {
    timeoutMs = 30_000,
    onMetric
  } = options;

  // Get the engine from context
  const storeContext = useStoreContext();
  const engine = storeContext.engine;

  return {
    /**
     * The underlying RDF engine
     * @type {RdfEngine}
     */
    get engine() {
      return engine;
    },

    /**
     * Validate the context store against SHACL shapes
     * @param {string|Store|Object} shapesInput - SHACL shapes as Turtle string or Store
     * @returns {Promise<Object>} Validation report
     * 
     * @example
     * const report = await validator.validate(shapesStore);
     * if (report.conforms) {
     *   console.log("✅ Validation passed");
     * } else {
     *   console.log("❌ Validation failed:", report.results);
     * }
     */
    async validate(shapesInput) {
      const storeContext = useStoreContext();
      const store = storeContext.store;
      return engine.validateShacl(store, shapesInput);
    },

    /**
     * Validate the context store against SHACL shapes, throw on failure
     * @param {string|Store|Object} shapesInput - SHACL shapes
     * @returns {Promise<Object>} Validation report
     * @throws {Error} If validation fails
     * 
     * @example
     * try {
     *   await validator.validateOrThrow(shapesStore);
     *   console.log("✅ Validation passed");
     * } catch (error) {
     *   console.log("❌ Validation failed:", error.message);
     * }
     */
    async validateOrThrow(shapesInput) {
      const storeContext = useStoreContext();
      const store = storeContext.store;
      return engine.validateShaclOrThrow(store, shapesInput);
    },

    /**
     * Validate a specific data store against SHACL shapes
     * @param {Store|Object} dataStore - Data store to validate
     * @param {string|Store|Object} shapesInput - SHACL shapes as Turtle string or Store
     * @returns {Promise<Object>} Validation report
     * 
     * @example
     * const report = await validator.validateStore(dataStore, shapesStore);
     */
    async validateStore(dataStore, shapesInput) {
      const store = dataStore.store || dataStore;
      return engine.validateShacl(store, shapesInput);
    },

    /**
     * Validate a specific data store against SHACL shapes, throw on failure
     * @param {Store|Object} dataStore - Data store to validate
     * @param {string|Store|Object} shapesInput - SHACL shapes
     * @returns {Promise<Object>} Validation report
     * @throws {Error} If validation fails
     */
    async validateStoreOrThrow(dataStore, shapesInput) {
      const store = dataStore.store || dataStore;
      return engine.validateShaclOrThrow(store, shapesInput);
    },

    /**
     * Get a summary of validation results
     * @param {Object} report - Validation report
     * @returns {Object} Summary object
     * 
     * @example
     * const report = await validator.validate(store, shapes);
     * const summary = validator.summarize(report);
     * console.log(`Conforms: ${summary.conforms}, Errors: ${summary.errorCount}`);
     */
    summarize(report) {
      const errorCount = report.results.filter(r => r.severity === "http://www.w3.org/ns/shacl#Violation").length;
      const warningCount = report.results.filter(r => r.severity === "http://www.w3.org/ns/shacl#Warning").length;
      const infoCount = report.results.filter(r => r.severity === "http://www.w3.org/ns/shacl#Info").length;
      
      return {
        conforms: report.conforms,
        totalResults: report.results.length,
        errorCount,
        warningCount,
        infoCount,
        hasErrors: errorCount > 0,
        hasWarnings: warningCount > 0,
        hasInfo: infoCount > 0
      };
    },

    /**
     * Filter validation results by severity
     * @param {Object} report - Validation report
     * @param {string} severity - Severity level to filter by
     * @returns {Array<Object>} Filtered results
     * 
     * @example
     * const report = await validator.validate(store, shapes);
     * const errors = validator.filterBySeverity(report, "http://www.w3.org/ns/shacl#Violation");
     */
    filterBySeverity(report, severity) {
      return report.results.filter(r => r.severity === severity);
    },

    /**
     * Get validation results grouped by focus node
     * @param {Object} report - Validation report
     * @returns {Map<string, Array<Object>>} Results grouped by focus node
     * 
     * @example
     * const report = await validator.validate(store, shapes);
     * const grouped = validator.groupByFocusNode(report);
     * for (const [node, results] of grouped) {
     *   console.log(`Node ${node} has ${results.length} violations`);
     * }
     */
    groupByFocusNode(report) {
      const grouped = new Map();
      for (const result of report.results) {
        const node = result.focusNode || "unknown";
        if (!grouped.has(node)) {
          grouped.set(node, []);
        }
        grouped.get(node).push(result);
      }
      return grouped;
    },

    /**
     * Get validation results grouped by property path
     * @param {Object} report - Validation report
     * @returns {Map<string, Array<Object>>} Results grouped by property path
     * 
     * @example
     * const report = await validator.validate(store, shapes);
     * const grouped = validator.groupByPath(report);
     * for (const [path, results] of grouped) {
     *   console.log(`Path ${path} has ${results.length} violations`);
     * }
     */
    groupByPath(report) {
      const grouped = new Map();
      for (const result of report.results) {
        const path = result.path || "unknown";
        if (!grouped.has(path)) {
          grouped.set(path, []);
        }
        grouped.get(path).push(result);
      }
      return grouped;
    }
  };
}
