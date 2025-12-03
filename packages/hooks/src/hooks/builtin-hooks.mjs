/**
 * @file Built-in hooks for common validation and transformation patterns.
 * @module hooks/builtin-hooks
 */

import { defineHook } from './define-hook.mjs';

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
 * Validate that quad object is a Literal.
 */
export const validateObjectLiteral = defineHook({
  name: 'validate-object-literal',
  trigger: 'before-add',
  validate: quad => {
    return quad.object.termType === 'Literal';
  },
  metadata: {
    description: 'Validates that quad object is a Literal',
  },
});

/**
 * Validate that IRI values are well-formed.
 */
export const validateIRIFormat = defineHook({
  name: 'validate-iri-format',
  trigger: 'before-add',
  validate: quad => {
    const validateIRI = term => {
      if (term.termType !== 'NamedNode') {
        return true;
      }
      try {
        new URL(term.value);
        return true;
      } catch {
        return false;
      }
    };

    return validateIRI(quad.subject) && validateIRI(quad.predicate) && validateIRI(quad.object);
  },
  metadata: {
    description: 'Validates that IRI values are well-formed URLs',
  },
});

/**
 * Validate that literals have language tags if required.
 */
export const validateLanguageTag = defineHook({
  name: 'validate-language-tag',
  trigger: 'before-add',
  validate: quad => {
    if (quad.object.termType !== 'Literal') {
      return true;
    }
    return quad.object.language !== undefined && quad.object.language !== '';
  },
  metadata: {
    description: 'Validates that literal objects have language tags',
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
    if (quad.object.termType !== 'Literal' || !quad.object.language) {
      return quad;
    }

    const DataFactory = quad.subject.constructor.prototype.constructor;
    return DataFactory.quad(
      quad.subject,
      quad.predicate,
      DataFactory.literal(quad.object.value, quad.object.language.toLowerCase()),
      quad.graph
    );
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

    const DataFactory = quad.subject.constructor.prototype.constructor;
    return DataFactory.quad(
      quad.subject,
      quad.predicate,
      DataFactory.literal(quad.object.value.trim(), quad.object.language || quad.object.datatype),
      quad.graph
    );
  },
  metadata: {
    description: 'Trims whitespace from literal values',
  },
});

/* ========================================================================= */
/* Composite Hooks                                                          */
/* ========================================================================= */

/**
 * Standard validation for RDF quads.
 * Combines IRI validation and predicate validation.
 */
export const standardValidation = defineHook({
  name: 'standard-validation',
  trigger: 'before-add',
  validate: quad => {
    return (
      quad.predicate.termType === 'NamedNode' &&
      (quad.subject.termType === 'NamedNode' || quad.subject.termType === 'BlankNode')
    );
  },
  metadata: {
    description: 'Standard RDF validation rules',
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

  // Composite
  standardValidation,
};
