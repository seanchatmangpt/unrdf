/**
 * @file Diataxis Kit - Main Entry Point
 * @module @unrdf/diataxis-kit
 * @description
 * Evidence-driven documentation classification and scaffold generation
 * using the Diataxis framework (tutorials, how-tos, reference, explanation).
 */

// Classification
export { classifyPackage } from './classify.mjs';

// Evidence collection
export { collectEvidence, hashEvidence } from './evidence.mjs';

// Hashing utilities
export { hashObject, hashString, hashFile } from './hash.mjs';

// Package inventory discovery
export { discoverPackages, validatePackageEntry } from './inventory.mjs';

// Scaffold generation
export { generateScaffold } from './scaffold.mjs';

// Stable JSON stringification
export { stableStringify, stableEqual } from './stable-json.mjs';

// Diataxis schema and validation
export {
  createDiataxisEntry,
  validateDiataxisEntry,
  ensureMinimumDiataxis,
} from './diataxis-schema.mjs';

// Reference extraction
export { extractReference } from './reference-extractor.mjs';
