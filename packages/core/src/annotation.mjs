/**
 * @file Annotation Helpers for RDF-star
 * @module @unrdf/core/annotation
 * @description Utilities for creating and managing RDF-star annotations
 */

import {
  validateProvenance,
  validateTemporal,
  validateConfidence,
  validateAnnotation,
} from './rdf-star.schema.mjs';

/**
 * Annotation Builder - Fluent API for building annotations
 *
 * @example
 * const builder = new AnnotationBuilder()
 *   .source('http://example.org/dataset')
 *   .creator('Alice')
 *   .confidence(0.95)
 *   .validFrom('2026-01-01T00:00:00Z')
 *   .build();
 */
export class AnnotationBuilder {
  /**
   * Create a new annotation builder
   */
  constructor() {
    this.annotation = {
      provenance: {},
      temporal: {},
      confidence: null,
      multiSource: null,
      custom: {},
    };
  }

  /**
   * Set source URL
   * @param {string} source - Source URL
   * @returns {AnnotationBuilder} This instance for chaining
   */
  source(source) {
    if (!this.annotation.provenance) {
      this.annotation.provenance = {};
    }
    this.annotation.provenance.source = source;
    return this;
  }

  /**
   * Set creator
   * @param {string} creator - Creator identifier
   * @returns {AnnotationBuilder} This instance for chaining
   */
  creator(creator) {
    if (!this.annotation.provenance) {
      this.annotation.provenance = {};
    }
    this.annotation.provenance.creator = creator;
    return this;
  }

  /**
   * Set creation timestamp
   * @param {string} created - ISO 8601 datetime
   * @returns {AnnotationBuilder} This instance for chaining
   */
  created(created) {
    if (!this.annotation.provenance) {
      this.annotation.provenance = {};
    }
    this.annotation.provenance.created = created;
    return this;
  }

  /**
   * Set modification timestamp
   * @param {string} modified - ISO 8601 datetime
   * @returns {AnnotationBuilder} This instance for chaining
   */
  modified(modified) {
    if (!this.annotation.provenance) {
      this.annotation.provenance = {};
    }
    this.annotation.provenance.modified = modified;
    return this;
  }

  /**
   * Set valid from timestamp
   * @param {string} validFrom - ISO 8601 datetime
   * @returns {AnnotationBuilder} This instance for chaining
   */
  validFrom(validFrom) {
    if (!this.annotation.temporal) {
      this.annotation.temporal = {};
    }
    this.annotation.temporal.validFrom = validFrom;
    return this;
  }

  /**
   * Set valid to timestamp
   * @param {string} validTo - ISO 8601 datetime
   * @returns {AnnotationBuilder} This instance for chaining
   */
  validTo(validTo) {
    if (!this.annotation.temporal) {
      this.annotation.temporal = {};
    }
    this.annotation.temporal.validTo = validTo;
    return this;
  }

  /**
   * Set confidence score
   * @param {number} confidence - Confidence score (0-1)
   * @param {string} [method] - Method used to determine confidence
   * @returns {AnnotationBuilder} This instance for chaining
   */
  confidence(confidence, method) {
    this.annotation.confidence = { confidence, method };
    return this;
  }

  /**
   * Set multiple sources
   * @param {Array<string>} sources - Array of source URLs
   * @param {number} [agreement] - Agreement score (0-1)
   * @returns {AnnotationBuilder} This instance for chaining
   */
  sources(sources, agreement) {
    this.annotation.multiSource = { sources, agreement };
    return this;
  }

  /**
   * Add custom annotation
   * @param {string} key - Custom key
   * @param {any} value - Custom value
   * @returns {AnnotationBuilder} This instance for chaining
   */
  custom(key, value) {
    if (!this.annotation.custom) {
      this.annotation.custom = {};
    }
    this.annotation.custom[key] = value;
    return this;
  }

  /**
   * Build and validate the annotation
   * @returns {Object} Validated annotation object
   * @throws {z.ZodError} If validation fails
   */
  build() {
    const cleaned = {};

    if (this.annotation.provenance && Object.keys(this.annotation.provenance).length > 0) {
      cleaned.provenance = this.annotation.provenance;
    } else {
      cleaned.provenance = null;
    }

    if (this.annotation.temporal && Object.keys(this.annotation.temporal).length > 0) {
      cleaned.temporal = this.annotation.temporal;
    } else {
      cleaned.temporal = null;
    }

    if (this.annotation.confidence) {
      cleaned.confidence = this.annotation.confidence;
    } else {
      cleaned.confidence = null;
    }

    if (this.annotation.multiSource) {
      cleaned.multiSource = this.annotation.multiSource;
    } else {
      cleaned.multiSource = null;
    }

    if (this.annotation.custom && Object.keys(this.annotation.custom).length > 0) {
      cleaned.custom = this.annotation.custom;
    } else {
      cleaned.custom = null;
    }

    return validateAnnotation(cleaned);
  }
}

/**
 * Create provenance annotation
 * @param {Object} options - Provenance options
 * @param {string} [options.source] - Source URL
 * @param {string} [options.creator] - Creator identifier
 * @param {string} [options.created] - Creation timestamp
 * @param {string} [options.modified] - Modification timestamp
 * @returns {Object} Validated provenance annotation
 */
export function createProvenance(options) {
  return validateProvenance(options);
}

/**
 * Create temporal annotation
 * @param {Object} options - Temporal options
 * @param {string} [options.validFrom] - Valid from timestamp
 * @param {string} [options.validTo] - Valid to timestamp
 * @returns {Object} Validated temporal annotation
 */
export function createTemporal(options) {
  return validateTemporal(options);
}

/**
 * Create confidence annotation
 * @param {number} confidence - Confidence score (0-1)
 * @param {string} [method] - Method used
 * @returns {Object} Validated confidence annotation
 */
export function createConfidence(confidence, method) {
  return validateConfidence({ confidence, method });
}

/**
 * Create multi-source annotation
 * @param {Array<string>} sources - Array of source URLs
 * @param {Object} [options] - Additional options
 * @param {number} [options.agreement] - Agreement score
 * @param {boolean} [options.conflicting] - Whether sources conflict
 * @returns {Object} Multi-source annotation
 */
export function createMultiSource(sources, options = {}) {
  return {
    sources,
    agreement: options.agreement,
    conflicting: options.conflicting,
  };
}

/**
 * Merge multiple annotations
 * @param {...Object} annotations - Annotations to merge
 * @returns {Object} Merged annotation
 */
export function mergeAnnotations(...annotations) {
  const merged = {
    provenance: {},
    temporal: {},
    confidence: null,
    multiSource: null,
    custom: {},
  };

  for (const ann of annotations) {
    if (ann.provenance) {
      Object.assign(merged.provenance, ann.provenance);
    }
    if (ann.temporal) {
      Object.assign(merged.temporal, ann.temporal);
    }
    if (ann.confidence) {
      merged.confidence = ann.confidence;
    }
    if (ann.multiSource) {
      merged.multiSource = ann.multiSource;
    }
    if (ann.custom) {
      Object.assign(merged.custom, ann.custom);
    }
  }

  return validateAnnotation(merged);
}

/**
 * Extract annotations from quads
 * @param {Array<Object>} quads - Array of quads
 * @param {Object} quotedTriple - The quoted triple to extract annotations for
 * @returns {Object} Extracted annotations
 */
export function extractAnnotations(quads, quotedTriple) {
  const annotations = {
    provenance: {},
    temporal: {},
    confidence: null,
    multiSource: null,
    custom: {},
  };

  for (const quad of quads) {
    if (quad.subject === quotedTriple || quad.subject.value === quotedTriple.value) {
      const predValue = quad.predicate.value;
      const objValue = quad.object.value;

      if (predValue.includes('source')) {
        annotations.provenance.source = objValue;
      } else if (predValue.includes('creator')) {
        annotations.provenance.creator = objValue;
      } else if (predValue.includes('created')) {
        annotations.provenance.created = objValue;
      } else if (predValue.includes('modified')) {
        annotations.provenance.modified = objValue;
      } else if (predValue.includes('validFrom')) {
        annotations.temporal.validFrom = objValue;
      } else if (predValue.includes('validTo')) {
        annotations.temporal.validTo = objValue;
      } else if (predValue.includes('confidence')) {
        annotations.confidence = { confidence: parseFloat(objValue) };
      }
    }
  }

  return annotations;
}

/**
 * Create annotation builder
 * @returns {AnnotationBuilder} New annotation builder
 */
export function createAnnotationBuilder() {
  return new AnnotationBuilder();
}

export default {
  AnnotationBuilder,
  createAnnotationBuilder,
  createProvenance,
  createTemporal,
  createConfidence,
  createMultiSource,
  mergeAnnotations,
  extractAnnotations,
};
