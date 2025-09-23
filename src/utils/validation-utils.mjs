/**
 * @fileoverview Validation utilities - RDF validation helpers
 * 
 * These utilities cover the 80/20 dark matter of RDF validation
 * that every project ends up reimplementing.
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { z } from "zod";
import { asNamedNode } from "./term-utils.mjs";

/**
 * Zod schema for validating RDF IRIs
 */
export const IRISchema = z.string().url();

/**
 * Zod schema for validating RDF literals
 */
export const LiteralSchema = z.object({
  termType: z.literal("Literal"),
  value: z.string(),
  language: z.string().optional(),
  datatype: z.object({ value: z.string() }).optional(),
});

/**
 * Zod schema for validating RDF named nodes
 */
export const NamedNodeSchema = z.object({
  termType: z.literal("NamedNode"),
  value: z.string().url(),
});

/**
 * Zod schema for validating RDF blank nodes
 */
export const BlankNodeSchema = z.object({
  termType: z.literal("BlankNode"),
  value: z.string(),
});

/**
 * Zod schema for validating RDF terms
 */
export const TermSchema = z.union([LiteralSchema, NamedNodeSchema, BlankNodeSchema]);

/**
 * Zod schema for validating RDF quads
 */
export const QuadSchema = z.object({
  subject: TermSchema,
  predicate: NamedNodeSchema,
  object: TermSchema,
  graph: TermSchema.optional(),
});

/**
 * Validate an RDF IRI
 * @param {string} iri - IRI to validate
 * @returns {boolean} True if valid IRI
 * 
 * @example
 * const isValid = validateIRI("http://example.org/foo");
 */
export function validateIRI(iri) {
  try {
    IRISchema.parse(iri);
    return true;
  } catch {
    return false;
  }
}

/**
 * Validate an RDF literal
 * @param {Object} literal - Literal to validate
 * @returns {boolean} True if valid literal
 * 
 * @example
 * const isValid = validateLiteral({ termType: "Literal", value: "hello" });
 */
export function validateLiteral(literal) {
  try {
    LiteralSchema.parse(literal);
    return true;
  } catch {
    return false;
  }
}

/**
 * Validate an RDF named node
 * @param {Object} namedNode - Named node to validate
 * @returns {boolean} True if valid named node
 * 
 * @example
 * const isValid = validateNamedNode({ termType: "NamedNode", value: "http://example.org/foo" });
 */
export function validateNamedNode(namedNode) {
  try {
    NamedNodeSchema.parse(namedNode);
    return true;
  } catch {
    return false;
  }
}

/**
 * Validate an RDF blank node
 * @param {Object} blankNode - Blank node to validate
 * @returns {boolean} True if valid blank node
 * 
 * @example
 * const isValid = validateBlankNode({ termType: "BlankNode", value: "_:b1" });
 */
export function validateBlankNode(blankNode) {
  try {
    BlankNodeSchema.parse(blankNode);
    return true;
  } catch {
    return false;
  }
}

/**
 * Validate an RDF term
 * @param {Object} term - Term to validate
 * @returns {boolean} True if valid term
 * 
 * @example
 * const isValid = validateTerm({ termType: "NamedNode", value: "http://example.org/foo" });
 */
export function validateTerm(term) {
  try {
    TermSchema.parse(term);
    return true;
  } catch {
    return false;
  }
}

/**
 * Validate an RDF quad
 * @param {Object} quad - Quad to validate
 * @returns {boolean} True if valid quad
 * 
 * @example
 * const isValid = validateQuad({
 *   subject: { termType: "NamedNode", value: "http://example.org/s" },
 *   predicate: { termType: "NamedNode", value: "http://example.org/p" },
 *   object: { termType: "Literal", value: "hello" }
 * });
 */
export function validateQuad(quad) {
  try {
    QuadSchema.parse(quad);
    return true;
  } catch {
    return false;
  }
}

/**
 * Validate a Turtle string
 * @param {string} turtle - Turtle string to validate
 * @param {Object} [options] - Validation options
 * @param {string} [options.baseIRI] - Base IRI for parsing
 * @returns {Promise<boolean>} True if valid Turtle
 * 
 * @example
 * const isValid = await validateTurtle("@prefix ex: <http://example.org/> . ex:foo a ex:Bar .");
 */
export async function validateTurtle(turtle, options = {}) {
  try {
    const { RdfEngine } = await import("../engines/RdfEngine.mjs");
    const engine = new RdfEngine();
    engine.parseTurtle(turtle, options);
    return true;
  } catch {
    return false;
  }
}

/**
 * Validate an N-Quads string
 * @param {string} nquads - N-Quads string to validate
 * @returns {Promise<boolean>} True if valid N-Quads
 * 
 * @example
 * const isValid = await validateNQuads("<http://example.org/s> <http://example.org/p> <http://example.org/o> .");
 */
export async function validateNQuads(nquads) {
  try {
    const { RdfEngine } = await import("../engines/RdfEngine.mjs");
    const engine = new RdfEngine();
    engine.parseNQuads(nquads);
    return true;
  } catch {
    return false;
  }
}

/**
 * Validate a JSON-LD document
 * @param {Object} jsonld - JSON-LD document to validate
 * @returns {Promise<boolean>} True if valid JSON-LD
 * 
 * @example
 * const isValid = await validateJSONLD({ "@context": {}, "@id": "http://example.org/foo" });
 */
export async function validateJSONLD(jsonld) {
  try {
    const { RdfEngine } = await import("../engines/RdfEngine.mjs");
    const engine = new RdfEngine();
    await engine.fromJSONLD(jsonld);
    return true;
  } catch {
    return false;
  }
}

/**
 * Validate a SPARQL query string
 * @param {string} sparql - SPARQL query to validate
 * @returns {Promise<boolean>} True if valid SPARQL
 * 
 * @example
 * const isValid = await validateSPARQL("SELECT ?s WHERE { ?s ?p ?o }");
 */
export async function validateSPARQL(sparql) {
  try {
    const { RdfEngine } = await import("../engines/RdfEngine.mjs");
    const engine = new RdfEngine();
    const store = engine.createStore();
    await engine.query(store, sparql);
    return true;
  } catch {
    return false;
  }
}

/**
 * Validate a SHACL shapes document
 * @param {string} shapes - SHACL shapes as Turtle string
 * @returns {Promise<boolean>} True if valid SHACL
 * 
 * @example
 * const isValid = await validateSHACL("@prefix sh: <http://www.w3.org/ns/shacl#> . ex:Shape a sh:NodeShape .");
 */
export async function validateSHACL(shapes) {
  try {
    const { RdfEngine } = await import("../engines/RdfEngine.mjs");
    const engine = new RdfEngine();
    const shapesStore = engine.parseTurtle(shapes);
    const dataStore = engine.createStore();
    await engine.validateShacl(dataStore, shapesStore);
    return true;
  } catch {
    return false;
  }
}

/**
 * Validate a store for common issues
 * @param {Store} store - Store to validate
 * @returns {Object} Validation result with issues
 * 
 * @example
 * const result = validateStore(store);
 * if (result.valid) {
 *   console.log("Store is valid");
 * } else {
 *   console.log("Issues found:", result.issues);
 * }
 */
export function validateStore(store) {
  const issues = [];
  
  // Check for empty store
  if (store.size === 0) {
    issues.push({ type: "warning", message: "Store is empty" });
  }
  
  // Check for invalid terms
  for (const quad of store) {
    if (!validateTerm(quad.subject)) {
      issues.push({ type: "error", message: `Invalid subject: ${quad.subject}`, quad });
    }
    if (!validateTerm(quad.predicate)) {
      issues.push({ type: "error", message: `Invalid predicate: ${quad.predicate}`, quad });
    }
    if (!validateTerm(quad.object)) {
      issues.push({ type: "error", message: `Invalid object: ${quad.object}`, quad });
    }
    if (quad.graph && !validateTerm(quad.graph)) {
      issues.push({ type: "error", message: `Invalid graph: ${quad.graph}`, quad });
    }
  }
  
  // Check for duplicate quads
  const quadStrings = new Set();
  for (const quad of store) {
    const quadStr = `${quad.subject.value} ${quad.predicate.value} ${quad.object.value} ${quad.graph?.value || ''}`;
    if (quadStrings.has(quadStr)) {
      issues.push({ type: "warning", message: `Duplicate quad: ${quadStr}`, quad });
    }
    quadStrings.add(quadStr);
  }
  
  return {
    valid: issues.length === 0,
    issues,
    issueCount: issues.length,
    errorCount: issues.filter(i => i.type === "error").length,
    warningCount: issues.filter(i => i.type === "warning").length
  };
}

/**
 * Validate a store against basic RDF constraints
 * @param {Store} store - Store to validate
 * @returns {Object} Validation result
 * 
 * @example
 * const result = validateRDFConstraints(store);
 * if (!result.valid) {
 *   console.log("RDF constraints violated:", result.violations);
 * }
 */
export function validateRDFConstraints(store) {
  const violations = [];
  
  for (const quad of store) {
    // Check that predicate is always a named node
    if (quad.predicate.termType !== "NamedNode") {
      violations.push({
        type: "error",
        message: "Predicate must be a named node",
        quad
      });
    }
    
    // Check that subject is not a literal
    if (quad.subject.termType === "Literal") {
      violations.push({
        type: "error",
        message: "Subject cannot be a literal",
        quad
      });
    }
    
    // Check that object is not a predicate
    if (quad.object.termType === "NamedNode" && quad.object.value === quad.predicate.value) {
      violations.push({
        type: "warning",
        message: "Object and predicate are the same",
        quad
      });
    }
  }
  
  return {
    valid: violations.length === 0,
    violations,
    violationCount: violations.length,
    errorCount: violations.filter(v => v.type === "error").length,
    warningCount: violations.filter(v => v.type === "warning").length
  };
}

/**
 * Validate a store for common patterns
 * @param {Store} store - Store to validate
 * @returns {Object} Validation result
 * 
 * @example
 * const result = validateCommonPatterns(store);
 * console.log(`Found ${result.patterns.length} common patterns`);
 */
export function validateCommonPatterns(store) {
  const patterns = [];
  
  // Check for rdf:type usage
  const typeQuads = store.getQuads(null, asNamedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"), null, null);
  if (typeQuads.length > 0) {
    patterns.push({
      type: "info",
      name: "rdf:type usage",
      count: typeQuads.length,
      message: `Found ${typeQuads.length} rdf:type statements`
    });
  }
  
  // Check for rdfs:label usage
  const labelQuads = store.getQuads(null, asNamedNode("http://www.w3.org/2000/01/rdf-schema#label"), null, null);
  if (labelQuads.length > 0) {
    patterns.push({
      type: "info",
      name: "rdfs:label usage",
      count: labelQuads.length,
      message: `Found ${labelQuads.length} rdfs:label statements`
    });
  }
  
  // Check for blank node usage
  const blankNodes = new Set();
  for (const quad of store) {
    if (quad.subject.termType === "BlankNode") blankNodes.add(quad.subject.value);
    if (quad.object.termType === "BlankNode") blankNodes.add(quad.object.value);
  }
  if (blankNodes.size > 0) {
    patterns.push({
      type: "info",
      name: "blank node usage",
      count: blankNodes.size,
      message: `Found ${blankNodes.size} unique blank nodes`
    });
  }
  
  return {
    patterns,
    patternCount: patterns.length
  };
}

/**
 * Create a validation pipeline
 * @param {Array<Function>} validators - Array of validator functions
 * @returns {Object} Pipeline interface
 * 
 * @example
 * const pipeline = createValidationPipeline([
 *   validateStore,
 *   validateRDFConstraints,
 *   validateCommonPatterns
 * ]);
 * 
 * const result = await pipeline.execute(store);
 */
export function createValidationPipeline(validators) {
  return {
    validators,
    
    /**
     * Execute the validation pipeline
     * @param {Store} store - Store to validate
     * @returns {Promise<Object>} Pipeline result
     */
    async execute(store) {
      const results = [];
      const allIssues = [];
      
      for (const validator of this.validators) {
        try {
          const result = await validator(store);
          results.push(result);
          
          if (result.issues) {
            allIssues.push(...result.issues);
          }
          if (result.violations) {
            allIssues.push(...result.violations);
          }
        } catch (error) {
          results.push({
            valid: false,
            error: error.message
          });
        }
      }
      
      return {
        results,
        allIssues,
        valid: allIssues.length === 0,
        issueCount: allIssues.length,
        errorCount: allIssues.filter(i => i.type === "error").length,
        warningCount: allIssues.filter(i => i.type === "warning").length
      };
    }
  };
}