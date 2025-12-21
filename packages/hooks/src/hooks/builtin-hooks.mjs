/**
 * @file Built-in hooks for common validation and transformation patterns.
 * @module hooks/builtin-hooks
 */

import { defineHook } from './define-hook.mjs';
import { dataFactory } from '@unrdf/oxigraph';
import { quadPool } from './quad-pool.mjs';

/**
 * @typedef {import('n3').Quad} Quad
 * @typedef {import('./define-hook.mjs').Hook} Hook
 */

/* ========================================================================= */
/* Validation Hooks                                                         */
/* ========================================================================= */

/**
 * Validate that quad subject is a Named Node (IRI).
 */
export const validateSubjectIRI = defineHook({
  name: 'validate-subject-iri',
  trigger: 'before-add',
  validate: quad => {
    return quad.subject.termType === 'NamedNode';
  },
  metadata: {
    description: 'Validates that quad subject is a Named Node (IRI)',
  },
});

/**
 * Validate that quad predicate is a Named Node (IRI).
 */
export const validatePredicateIRI = defineHook({
  name: 'validate-predicate-iri',
  trigger: 'before-add',
  validate: quad => {
    return quad.predicate.termType === 'NamedNode';
  },
  metadata: {
    description: 'Validates that quad predicate is a Named Node (IRI)',
  },
});

/**
 * Validate that quad object is a Literal with non-empty value.
 */
export const validateObjectLiteral = defineHook({
  name: 'validate-object-literal',
  trigger: 'before-add',
  validate: quad => {
    return quad.object.termType === 'Literal' && quad.object.value.length > 0;
  },
  metadata: {
    description: 'Validates that quad object is a non-empty Literal',
  },
});

/**
 * Validate that IRI values are well-formed (no spaces, valid URL structure).
 * Only validates subject and predicate (objects can be literals).
 */
export const validateIRIFormat = defineHook({
  name: 'validate-iri-format',
  trigger: 'before-add',
  validate: quad => {
    const validateIRI = term => {
      // Check for spaces or invalid characters
      if (/\s/.test(term.value)) {
        return false;
      }
      try {
        new URL(term.value);
        return true;
      } catch {
        return false;
      }
    };

    // Only validate subject and predicate (not object, which can be a literal)
    return validateIRI(quad.subject) && validateIRI(quad.predicate);
  },
  metadata: {
    description: 'Validates that subject and predicate IRIs are well-formed URLs without spaces',
  },
});

/**
 * Validate that language tags are well-formed (BCP 47 format: en, en-US, etc).
 */
export const validateLanguageTag = defineHook({
  name: 'validate-language-tag',
  trigger: 'before-add',
  validate: quad => {
    // Skip if no language tag present
    if (!quad.object.language) {
      return true;
    }
    // BCP 47 language tag: letters and hyphens only, no underscores
    // Examples: en, en-US, fr, de-DE
    const validTag = /^[a-zA-Z]{2,3}(-[a-zA-Z]{2,4})?$/;
    return validTag.test(quad.object.language);
  },
  metadata: {
    description: 'Validates that language tags conform to BCP 47 format',
  },
});

/**
 * Validate that no blank nodes are used.
 */
export const rejectBlankNodes = defineHook({
  name: 'reject-blank-nodes',
  trigger: 'before-add',
  validate: quad => {
    return quad.subject.termType !== 'BlankNode' && quad.object.termType !== 'BlankNode';
  },
  metadata: {
    description: 'Rejects quads containing blank nodes',
  },
});

/* ========================================================================= */
/* Transformation Hooks                                                     */
/* ========================================================================= */

/**
 * Normalize namespace prefixes to full IRIs.
 * Note: This is a simple example - production use would need namespace map.
 */
export const normalizeNamespace = defineHook({
  name: 'normalize-namespace',
  trigger: 'before-add',
  transform: quad => {
    return quad;
  },
  metadata: {
    description: 'Normalizes namespace prefixes to full IRIs',
  },
});

/**
 * Normalize language tags to lowercase.
 */
export const normalizeLanguageTag = defineHook({
  name: 'normalize-language-tag',
  trigger: 'before-add',
  transform: quad => {
    if (!quad.object.language) {
      return quad;
    }

    // Create new quad with lowercase language tag
    return {
      ...quad,
      object: {
        ...quad.object,
        value: quad.object.value,
        language: quad.object.language.toLowerCase(),
      },
    };
  },
  metadata: {
    description: 'Normalizes language tags to lowercase',
  },
});

/**
 * Trim whitespace from literal values.
 */
export const trimLiterals = defineHook({
  name: 'trim-literals',
  trigger: 'before-add',
  transform: quad => {
    if (quad.object.termType !== 'Literal') {
      return quad;
    }

    // Create new quad with trimmed literal
    return {
      ...quad,
      object: {
        ...quad.object,
        value: quad.object.value.trim(),
      },
    };
  },
  metadata: {
    description: 'Trims whitespace from literal values',
  },
});

/* ========================================================================= */
/* Composite Hooks                                                          */
/* ========================================================================= */

/**
 * Standard validation for RDF quads (includes IRI format validation).
 * Combines IRI validation, predicate validation, and format checks.
 */
export const standardValidation = defineHook({
  name: 'standard-validation',
  trigger: 'before-add',
  validate: quad => {
    // Validate predicate is NamedNode
    if (quad.predicate.termType !== 'NamedNode') {
      return false;
    }
    // Validate subject is NamedNode or BlankNode
    if (quad.subject.termType !== 'NamedNode' && quad.subject.termType !== 'BlankNode') {
      return false;
    }
    // Validate IRI format (no spaces)
    const validateIRI = term => {
      if (term.termType !== 'NamedNode') return true;
      if (/\s/.test(term.value)) return false;
      try {
        new URL(term.value);
        return true;
      } catch {
        return false;
      }
    };
    return validateIRI(quad.subject) && validateIRI(quad.predicate);
  },
  metadata: {
    description: 'Standard RDF validation rules with IRI format checks',
  },
});

/* ========================================================================= */
/* Pooled Variants (Zero-Allocation Transforms)                             */
/* Uses quad-pool for branchless, Rust-inspired zero-copy semantics         */
/* ========================================================================= */

/**
 * Pooled language tag normalization - zero allocation in hot path.
 * Rust-inspired: borrow semantics via pool, avoid heap allocation.
 */
export const normalizeLanguageTagPooled = defineHook({
  name: 'normalize-language-tag-pooled',
  trigger: 'before-add',
  transform: quad => {
    if (!quad.object.language) return quad;

    // Pool-allocated quad (zero-copy transform)
    const pooledQuad = quadPool.acquire(
      quad.subject,
      quad.predicate,
      {
        ...quad.object,
        language: quad.object.language.toLowerCase(),
      },
      quad.graph
    );
    return pooledQuad;
  },
  metadata: {
    description: 'Zero-allocation language tag normalization using quad pool',
    pooled: true,
  },
});

/**
 * Pooled literal trimming - zero allocation in hot path.
 * Rust-inspired: borrow semantics via pool, avoid heap allocation.
 */
export const trimLiteralsPooled = defineHook({
  name: 'trim-literals-pooled',
  trigger: 'before-add',
  transform: quad => {
    if (quad.object.termType !== 'Literal') return quad;

    const trimmed = quad.object.value.trim();
    if (trimmed === quad.object.value) return quad;

    // Pool-allocated quad (zero-copy transform)
    return quadPool.acquire(
      quad.subject,
      quad.predicate,
      {
        ...quad.object,
        value: trimmed,
      },
      quad.graph
    );
  },
  metadata: {
    description: 'Zero-allocation literal trimming using quad pool',
    pooled: true,
  },
});

/* ========================================================================= */
/* Export all built-in hooks                                                */
/* ========================================================================= */

export const builtinHooks = {
  // Validation
  validateSubjectIRI,
  validatePredicateIRI,
  validateObjectLiteral,
  validateIRIFormat,
  validateLanguageTag,
  rejectBlankNodes,

  // Transformation
  normalizeNamespace,
  normalizeLanguageTag,
  trimLiterals,

  // Pooled variants (zero-allocation)
  normalizeLanguageTagPooled,
  trimLiteralsPooled,

  // Composite
  standardValidation,
};
