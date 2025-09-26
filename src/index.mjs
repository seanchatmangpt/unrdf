/**
 * @fileoverview unrdf - Opinionated composable framework for RDF knowledge operations
 * 
 * unrdf is the opinionated RDF framework for JavaScript. It makes the RDF universe 
 * accessible through a single composable surface — powered by N3.js, Comunica, 
 * SHACL, and Zod.
 * 
 * No TypeScript. Ever. TypeScript is an illusion of safety that collapses at runtime. 
 * unrdf guarantees correctness at the only level that matters: execution.
 * 
 * JSDoc is the source of truth. Documentation, type hints, and developer experience 
 * are delivered directly via JSDoc, keeping the codebase minimal and expressive.
 * 
 * Zod is the contract. Runtime validation ensures that what you think your data is, 
 * and what it actually is, are always in sync.
 * 
 * Composables everywhere. Every aspect of RDF — graphs, queries, validation, 
 * reasoning, serialization — is accessible through consistent composable functions.
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

// Store context
export * from "./context/index.mjs";

// Core composables
export { useGraph } from "./composables/use-graph.mjs";
export { useTurtle } from "./composables/use-turtle.mjs";
export { useTerms } from "./composables/use-terms.mjs";

// Validation and reasoning
export { useReasoner } from "./composables/use-reasoner.mjs";
export { useCanon } from "./composables/use-canon.mjs";

// Type safety and validation
export { useZod } from "./composables/use-zod.mjs";

// Change and provenance
export { useDelta } from "./composables/use-delta.mjs";

// Engines
export { RdfEngine } from "./engines/rdf-engine.mjs";

// Utilities
export * from "./utils/index.mjs";
