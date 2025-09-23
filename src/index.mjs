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
export { useStore } from "./composables/useStore.mjs";
export { useGraph } from "./composables/useGraph.mjs";
export { useTurtle } from "./composables/useTurtle.mjs";
export { useTerms } from "./composables/useTerms.mjs";
export { usePrefixes } from "./composables/usePrefixes.mjs";
export { useIRIs } from "./composables/useIRIs.mjs";
export { useLists } from "./composables/useLists.mjs";

// I/O composables
export { useTurtleFS } from "./composables/useTurtleFS.mjs";
export { useNQuads } from "./composables/useNQuads.mjs";
export { useJsonLd } from "./composables/useJsonLd.mjs";

// Query and traversal
export { usePointer } from "./composables/usePointer.mjs";

// Validation and reasoning
export { useValidator } from "./composables/useValidator.mjs";
export { useReasoner } from "./composables/useReasoner.mjs";
export { useCanon } from "./composables/useCanon.mjs";

// Type safety and validation
export { useZod } from "./composables/useZod.mjs";

// Change and provenance
export { useDelta } from "./composables/useDelta.mjs";

// Performance and caching
export { useMetrics } from "./composables/useMetrics.mjs";
export { useCache } from "./composables/useCache.mjs";

// Engines
export { RdfEngine } from "./engines/rdf-engine.mjs";

// Utilities
export * from "./utils/index.mjs";
