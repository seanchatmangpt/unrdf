/**
 * @fileoverview useZod composable - Zod validation for RDF data with context
 * 
 * This composable provides Zod validation capabilities for RDF-derived data.
 * It bridges the gap between RDF graphs and application-level type safety.
 * Now uses unctx for global validation management.
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { z } from "zod";
import { useStoreContext } from "../context/index.mjs";

/**
 * Create a Zod validation composable
 * 
 * @param {Object} [options] - Zod options
 * @param {boolean} [options.strict=true] - Enable strict validation
 * @param {Function} [options.onError] - Error callback
 * @returns {Object} Zod validation interface
 * 
 * @example
 * // Initialize store context first
 * const runApp = initStore();
 * 
 * runApp(() => {
 *   const zod = useZod();
 * 
 *   // Define schema for Person
 *   const PersonSchema = z.object({
 *     id: z.string().url(),
 *     name: z.string(),
 *     age: z.number().int().min(0)
 *   });
 * 
 *   // Validate SPARQL results
 *   const validated = await zod.validateResults(results, PersonSchema);
 * });
 * 
 * @throws {Error} If store context is not initialized
 */
export function useZod(options = {}) {
  // Get the engine from context
  const storeContext = useStoreContext();
  const engine = storeContext.engine;
  const store = storeContext.store;
  
  const {
    strict = true,
    onError
  } = options;

  return {
    /**
     * The underlying RDF engine
     * @type {RdfEngine}
     */
    get engine() {
      return engine;
    },

    /**
     * The underlying store
     * @type {Store}
     */
    get store() {
      return store;
    },

    /**
     * Validate SPARQL SELECT results against a Zod schema
     * @param {Array<Object>} results - SPARQL SELECT results
     * @param {z.ZodSchema} schema - Zod schema to validate against
     * @param {Object} [options] - Validation options
     * @param {boolean} [options.partial=false] - Use partial validation
     * @param {boolean} [options.strict] - Override strict mode
     * @returns {Array<Object>} Validated results
     * 
     * @example
     * const results = await graph.select(`
     *   PREFIX foaf: <http://xmlns.com/foaf/0.1/>
     *   SELECT ?id ?name ?age WHERE {
     *     ?id a foaf:Person ;
     *         foaf:name ?name ;
     *         foaf:age ?age .
     *   }
     * `);
     * 
     * const PersonSchema = z.object({
     *   id: z.string().url(),
     *   name: z.string(),
     *   age: z.number().int().min(0)
     * });
     * 
     * const validated = await zod.validateResults(results, PersonSchema);
     */
    async validateResults(results, schema, options = {}) {
      const { partial = false, strict: strictOverride } = options;
      const useStrict = strictOverride === undefined ? strict : strictOverride;
      
      const validationSchema = partial ? schema.partial() : schema;
      const validated = [];
      const errors = [];
      
      for (const [i, result] of results.entries()) {
        try {
          const validatedResult = useStrict 
            ? validationSchema.parse(result)
            : validationSchema.safeParse(result);
          
          if (useStrict) {
            validated.push(validatedResult);
          } else if (validatedResult.success) {
            validated.push(validatedResult.data);
          } else {
            errors.push({ index: i, error: validatedResult.error, data: result });
          }
        } catch (error) {
          errors.push({ index: i, error, data: result });
        }
      }
      
      if (errors.length > 0 && onError) {
        onError(errors);
      }
      
      return { validated, errors };
    },

    /**
     * Validate a single SPARQL result against a Zod schema
     * @param {Object} result - Single SPARQL result
     * @param {z.ZodSchema} schema - Zod schema to validate against
     * @param {Object} [options] - Validation options
     * @returns {Object} Validation result
     * 
     * @example
     * const result = results[0];
     * const validation = await zod.validateResult(result, PersonSchema);
     * if (validation.success) {
     *   console.log("Valid:", validation.data);
     * } else {
     *   console.log("Invalid:", validation.error);
     * }
     */
    async validateResult(result, schema, options = {}) {
      const { strict: strictOverride } = options;
      const useStrict = strictOverride === undefined ? strict : strictOverride;
      
      try {
        if (useStrict) {
          const validated = schema.parse(result);
          return { success: true, data: validated, error: null };
        } else {
          const validation = schema.safeParse(result);
          return {
            success: validation.success,
            data: validation.success ? validation.data : null,
            error: validation.success ? null : validation.error
          };
        }
      } catch (error) {
        return { success: false, data: null, error };
      }
    },

    /**
     * Validate JSON-LD data against a Zod schema
     * @param {Object} jsonld - JSON-LD document
     * @param {z.ZodSchema} schema - Zod schema to validate against
     * @param {Object} [options] - Validation options
     * @returns {Object} Validation result
     * 
     * @example
     * const jsonld = await graph.toJSONLD();
     * const validation = await zod.validateJSONLD(jsonld, DocumentSchema);
     */
    async validateJSONLD(jsonld, schema, options = {}) {
      return this.validateResult(jsonld, schema, options);
    },

    /**
     * Create a Zod schema for RDF terms
     * @param {Object} [options] - Schema options
     * @param {boolean} [options.allowBlankNodes=true] - Allow blank nodes
     * @param {boolean} [options.allowLiterals=true] - Allow literals
     * @param {boolean} [options.allowNamedNodes=true] - Allow named nodes
     * @returns {z.ZodSchema} Zod schema for RDF terms
     * 
     * @example
     * const TermSchema = zod.createTermSchema();
     * const validation = await zod.validateResult(term, TermSchema);
     */
    createTermSchema(options = {}) {
      const {
        allowBlankNodes = true,
        allowLiterals = true,
        allowNamedNodes = true
      } = options;
      
      const termTypes = [];
      if (allowNamedNodes) termTypes.push("NamedNode");
      if (allowBlankNodes) termTypes.push("BlankNode");
      if (allowLiterals) termTypes.push("Literal");
      
      return z.object({
        termType: z.enum(termTypes),
        value: z.string(),
        language: z.string().optional(),
        datatype: z.object({ value: z.string() }).optional()
      });
    },

    /**
     * Create a Zod schema for RDF quads
     * @param {Object} [options] - Schema options
     * @returns {z.ZodSchema} Zod schema for RDF quads
     * 
     * @example
     * const QuadSchema = zod.createQuadSchema();
     * const validation = await zod.validateResult(quad, QuadSchema);
     */
    createQuadSchema(options = {}) {
      const termSchema = this.createTermSchema(options);
      
      return z.object({
        subject: termSchema,
        predicate: termSchema,
        object: termSchema,
        graph: termSchema.optional()
      });
    },

    /**
     * Create a Zod schema for SPARQL SELECT results
     * @param {Array<string>} variables - SPARQL variables
     * @param {Object} [options] - Schema options
     * @returns {z.ZodSchema} Zod schema for SELECT results
     * 
     * @example
     * const SelectSchema = zod.createSelectSchema(['id', 'name', 'age']);
     * const validation = await zod.validateResults(results, SelectSchema);
     */
    createSelectSchema(variables, options = {}) {
      const { strict = true } = options;
      
      const shape = {};
      for (const variable of variables) {
        shape[variable] = z.any(); // Can be refined based on needs
      }
      
      const baseSchema = z.object(shape);
      return strict ? baseSchema : baseSchema.partial();
    },

    /**
     * Transform SPARQL results to match a Zod schema
     * @param {Array<Object>} results - SPARQL SELECT results
     * @param {z.ZodSchema} schema - Zod schema with transformations
     * @param {Object} [options] - Transformation options
     * @returns {Array<Object>} Transformed results
     * 
     * @example
     * const TransformSchema = z.object({
     *   id: z.string().url(),
     *   name: z.string(),
     *   age: z.string().transform(val => parseInt(val, 10))
     * });
     * 
     * const transformed = await zod.transformResults(results, TransformSchema);
     */
    async transformResults(results, schema, options = {}) {
      const { strict: strictOverride } = options;
      const useStrict = strictOverride === undefined ? strict : strictOverride;
      
      const transformed = [];
      const errors = [];
      
      for (const [i, result] of results.entries()) {
        try {
          const transformedResult = useStrict 
            ? schema.parse(result)
            : schema.safeParse(result);
          
          if (useStrict) {
            transformed.push(transformedResult);
          } else if (transformedResult.success) {
            transformed.push(transformedResult.data);
          } else {
            errors.push({ index: i, error: transformedResult.error, data: result });
          }
        } catch (error) {
          errors.push({ index: i, error, data: result });
        }
      }
      
      if (errors.length > 0 && onError) {
        onError(errors);
      }
      
      return { transformed, errors };
    },

    /**
     * Create a validation pipeline
     * @param {Array<Object>} steps - Array of validation steps
     * @returns {Object} Pipeline interface
     * 
     * @example
     * const pipeline = zod.createPipeline([
     *   { name: "basic", schema: BasicSchema },
     *   { name: "advanced", schema: AdvancedSchema },
     *   { name: "transform", schema: TransformSchema }
     * ]);
     * 
     * const result = await pipeline.execute(results);
     */
    createPipeline(steps) {
      return {
        steps,
        
        /**
         * Execute the validation pipeline
         * @param {Array<Object>} results - Results to validate
         * @returns {Promise<Object>} Pipeline result
         */
        async execute(results) {
          let currentResults = results;
          const stepResults = [];
          
          for (const step of this.steps) {
            const startTime = performance.now();
            const validation = await this.validateResults(currentResults, step.schema);
            const duration = performance.now() - startTime;
            
            stepResults.push({
              name: step.name,
              inputCount: currentResults.length,
              validCount: validation.validated.length,
              errorCount: validation.errors.length,
              duration
            });
            
            currentResults = validation.validated;
          }
          
          return {
            results: currentResults,
            steps: stepResults
          };
        }
      };
    },

    /**
     * Get validation statistics
     * @param {Object} validationResult - Result from validateResults
     * @returns {Object} Statistics object
     * 
     * @example
     * const validation = await zod.validateResults(results, schema);
     * const stats = zod.getStats(validation);
     * console.log(`Valid: ${stats.validCount}, Invalid: ${stats.invalidCount}`);
     */
    getStats(validationResult) {
      const { validated, errors } = validationResult;
      
      return {
        totalCount: validated.length + errors.length,
        validCount: validated.length,
        invalidCount: errors.length,
        successRate: (validated.length / (validated.length + errors.length)) * 100,
        errors: errors.map(e => ({
          index: e.index,
          message: e.error.message || e.error.toString()
        }))
      };
    }
  };
}
