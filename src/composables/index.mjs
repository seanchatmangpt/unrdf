/**
 * @fileoverview unrdf composables - opinionated RDF composable functions
 * 
 * This module exports all the composable functions that make up the unrdf API.
 * Each composable enforces a single, opinionated path through the RDF universe.
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

// Core composables
export { useGraph } from "./use-graph.mjs";
export { useTurtle } from "./use-turtle.mjs";
export { useTerms } from "./use-terms.mjs";
export { usePrefixes } from "./use-prefixes.mjs";
export { useLists } from "./use-lists.mjs";

// I/O composables
export { useNQuads } from "./use-n-quads.mjs";

// Validation and reasoning
export { useValidator } from "./use-validator.mjs";
export { useReasoner } from "./use-reasoner.mjs";
export { useCanon } from "./use-canon.mjs";

// Type safety and validation
export { useZod } from "./use-zod.mjs";
export { useTypes } from "./use-types.mjs";

// Advanced RDF operations
export { useRDFExt } from "./use-rdfext.mjs";

// Knowledge Hooks
export { useKnowledgeHooks, defineHook, evaluateHook } from "./use-knowledge-hooks.mjs";

// Change and provenance
export { useDelta } from "./use-delta.mjs";
