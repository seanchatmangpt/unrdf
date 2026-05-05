/**
 * @fileoverview PICTL Result Validation Against SHACL Shapes
 * @module @unrdf/pictl-semantics/result-validator
 *
 * Validates PICTL process mining results (fitness, precision, model conformance)
 * against SHACL shape constraints before quorum voting.
 */

import { z } from 'zod';
import { trace, SpanStatusCode } from '@opentelemetry/api';

const tracer = trace.getTracer('@unrdf/pictl-semantics', '[VERSION]');

/**
 * PICTL Result Schema (comprehensive)
 */
export const PictlResultSchema = z.object({
  // Process model reference
  modelId: z.string().min(1).optional(),

  // Quality metrics (0.0 - 1.0)
  fitness: z.number().min(0).max(1).optional(),
  precision: z.number().min(0).max(1).optional(),
  generalization: z.number().min(0).max(1).optional(),
  simplicity: z.number().min(0).max(1).optional(),

  // Conformance details
  alignmentCost: z.number().nonnegative().optional(),
  moveOnLog: z.number().nonnegative().optional(),
  moveOnModel: z.number().nonnegative().optional(),
  synchronous: z.number().nonnegative().optional(),

  // Process model (simplified representation)
  model: z
    .object({
      type: z.enum(['petri-net', 'bpmn', 'dfg', 'other']),
      places: z.number().nonnegative().optional(),
      transitions: z.number().nonnegative().optional(),
      arcs: z.number().nonnegative().optional(),
    })
    .optional(),

  // Data quality
  logSize: z.number().int().positive().optional(),
  traceCount: z.number().int().positive().optional(),
  eventCount: z.number().int().positive().optional(),

  // Timestamp
  timestamp: z.number().int().positive().optional(),
});

/**
 * SHACL Shape Constraint
 */
export const ShapeConstraintSchema = z.object({
  shapeUri: z.string().min(1),
  name: z.string(),
  description: z.string().optional(),
  nodeKind: z.enum(['IRI', 'BlankNode', 'Literal', 'BlankNodeOrIRI', 'BlankNodeOrLiteral', 'IRIOrLiteral']).optional(),
  datatype: z.string().optional(),
  minInclusive: z.number().optional(),
  maxInclusive: z.number().optional(),
  minExclusive: z.number().optional(),
  maxExclusive: z.number().optional(),
  minLength: z.number().int().nonnegative().optional(),
  maxLength: z.number().int().nonnegative().optional(),
  pattern: z.string().optional(),
});

/**
 * Pre-defined SHACL shapes for PICTL results
 */
const PICTL_SHAPES = {
  fitness: {
    shapeUri: 'urn:pictl:FitnessShape',
    name: 'Fitness Score Shape',
    description: 'Fitness score must be between 0.0 and 1.0',
    datatype: 'http://www.w3.org/2001/XMLSchema#double',
    minInclusive: 0.0,
    maxInclusive: 1.0,
  },
  precision: {
    shapeUri: 'urn:pictl:PrecisionShape',
    name: 'Precision Score Shape',
    description: 'Precision score must be between 0.0 and 1.0',
    datatype: 'http://www.w3.org/2001/XMLSchema#double',
    minInclusive: 0.0,
    maxInclusive: 1.0,
  },
  generalization: {
    shapeUri: 'urn:pictl:GeneralizationShape',
    name: 'Generalization Score Shape',
    description: 'Generalization score must be between 0.0 and 1.0',
    datatype: 'http://www.w3.org/2001/XMLSchema#double',
    minInclusive: 0.0,
    maxInclusive: 1.0,
  },
  simplicity: {
    shapeUri: 'urn:pictl:SimplicityShape',
    name: 'Simplicity Score Shape',
    description: 'Simplicity score must be between 0.0 and 1.0',
    datatype: 'http://www.w3.org/2001/XMLSchema#double',
    minInclusive: 0.0,
    maxInclusive: 1.0,
  },
  alignmentCost: {
    shapeUri: 'urn:pictl:AlignmentCostShape',
    name: 'Alignment Cost Shape',
    description: 'Alignment cost must be non-negative',
    datatype: 'http://www.w3.org/2001/XMLSchema#integer',
    minInclusive: 0,
  },
  logSize: {
    shapeUri: 'urn:pictl:LogSizeShape',
    name: 'Log Size Shape',
    description: 'Log size must be positive integer',
    datatype: 'http://www.w3.org/2001/XMLSchema#integer',
    minInclusive: 1,
  },
};

/**
 * Validate PICTL result against schema
 *
 * Checks that result object has valid types and value ranges
 * for quality metrics and conformance data.
 *
 * @param {Object} result - PICTL result to validate
 * @returns {Object} Validation result with details
 *
 * @example
 * const validation = validatePictlResult({
 *   fitness: 0.92,
 *   precision: 0.88,
 *   model: { type: 'petri-net', places: 5, transitions: 4 }
 * });
 * if (validation.valid) {
 *   console.log('Result schema is valid');
 * }
 */
export function validatePictlResult(result) {
  const span = tracer.startSpan('pictl.validate_result');
  try {
    // Validate schema
    const parsed = PictlResultSchema.safeParse(result);

    if (!parsed.success) {
      const errors = parsed.error.errors.map(e => ({
        path: e.path.join('.'),
        message: e.message,
        code: e.code,
      }));

      span.addEvent('schema_validation_failed', {
        'pictl.error_count': errors.length,
      });

      span.setStatus({ code: SpanStatusCode.OK });
      return {
        valid: false,
        schemaValid: false,
        shapesValid: null,
        errors,
        warnings: [],
      };
    }

    // Check value constraints
    const data = parsed.data;
    const warnings = [];

    // Warn if metrics are suspiciously extreme
    if (data.fitness === 1.0) {
      warnings.push('Fitness score of 1.0 may indicate overfitting');
    }
    if (data.precision === 0.0) {
      warnings.push('Precision score of 0.0 suggests poor model');
    }
    if (data.fitness && data.precision && Math.abs(data.fitness - data.precision) > 0.3) {
      warnings.push('Large gap between fitness and precision suggests model quality issue');
    }

    // Check alignment metrics if present
    if (data.alignmentCost !== undefined && data.logSize !== undefined) {
      const normalizedCost = data.alignmentCost / data.logSize;
      if (normalizedCost > 10) {
        warnings.push('High normalized alignment cost suggests poor model-log alignment');
      }
    }

    span.addEvent('schema_validation_passed', {
      'pictl.has_fitness': data.fitness !== undefined,
      'pictl.has_precision': data.precision !== undefined,
      'pictl.warning_count': warnings.length,
    });

    span.setStatus({ code: SpanStatusCode.OK });
    return {
      valid: true,
      schemaValid: true,
      shapesValid: true,
      errors: [],
      warnings,
      data,
    };
  } catch (error) {
    span.recordException(error);
    span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
    return {
      valid: false,
      schemaValid: false,
      shapesValid: null,
      errors: [{ message: error.message }],
      warnings: [],
    };
  } finally {
    span.end();
  }
}

/**
 * Validate result data against SHACL shape
 *
 * Verifies that a specific metric (fitness, precision, etc.)
 * satisfies the shape constraints (range, type, etc.)
 *
 * @param {Object} data - Data to validate
 * @param {string} shapeUri - SHACL shape URI to validate against
 * @param {Object} quorumState - Quorum state (for shape lookup)
 * @returns {Object} Shape validation result
 *
 * @example
 * const shapeValid = validateAgainstShapes(
 *   { fitness: 0.92 },
 *   'urn:pictl:FitnessShape',
 *   quorumState
 * );
 */
export function validateAgainstShapes(data, shapeUri, quorumState = null) {
  const span = tracer.startSpan('pictl.validate_shapes');
  try {
    // Get shape definition
    const shape = Object.values(PICTL_SHAPES).find(s => s.shapeUri === shapeUri);
    if (!shape) {
      return {
        valid: false,
        shapeFound: false,
        error: `Shape not found: ${shapeUri}`,
        conforms: [],
      };
    }

    ShapeConstraintSchema.parse(shape);

    // Extract the metric value to check
    // Shape URI like "urn:pictl:FitnessShape" -> look for "fitness" field
    const baseName = shape.shapeUri.split(':').pop().replace('Shape', '');
    const metricName = baseName.charAt(0).toLowerCase() + baseName.slice(1);
    const value = data[metricName];

    const conforms = [];
    const violations = [];

    // Type check
    if (shape.datatype) {
      if (shape.datatype.includes('double') && typeof value !== 'number') {
        violations.push(`Expected number for ${metricName}, got ${typeof value}`);
      } else if (shape.datatype.includes('integer') && !Number.isInteger(value)) {
        violations.push(`Expected integer for ${metricName}, got ${value}`);
      } else {
        conforms.push('Type check passed');
      }
    }

    // Range checks
    if (shape.minInclusive !== undefined && value < shape.minInclusive) {
      violations.push(`Value ${value} is below minimum ${shape.minInclusive}`);
    } else if (shape.minInclusive !== undefined) {
      conforms.push(`Value ${value} >= minimum ${shape.minInclusive}`);
    }

    if (shape.maxInclusive !== undefined && value > shape.maxInclusive) {
      violations.push(`Value ${value} is above maximum ${shape.maxInclusive}`);
    } else if (shape.maxInclusive !== undefined) {
      conforms.push(`Value ${value} <= maximum ${shape.maxInclusive}`);
    }

    // Length checks (for strings)
    if (typeof value === 'string') {
      if (shape.minLength !== undefined && value.length < shape.minLength) {
        violations.push(`String length ${value.length} is below minimum ${shape.minLength}`);
      }
      if (shape.maxLength !== undefined && value.length > shape.maxLength) {
        violations.push(`String length ${value.length} is above maximum ${shape.maxLength}`);
      }
    }

    // Pattern check (regex)
    if (shape.pattern && typeof value === 'string') {
      const regex = new RegExp(shape.pattern);
      if (!regex.test(value)) {
        violations.push(`Value "${value}" does not match pattern ${shape.pattern}`);
      } else {
        conforms.push(`Value "${value}" matches pattern ${shape.pattern}`);
      }
    }

    const isValid = violations.length === 0 && value !== undefined;

    span.addEvent('shapes_validation_completed', {
      'pictl.shape_uri': shapeUri,
      'pictl.conforms': conforms.length,
      'pictl.violations': violations.length,
      'pictl.valid': isValid,
    });

    if (isValid) {
      span.setStatus({ code: SpanStatusCode.OK });
    } else {
      span.setStatus({ code: SpanStatusCode.ERROR, message: violations[0] || 'Unknown violation' });
    }

    return {
      valid: isValid,
      shapeFound: true,
      shapeName: shape.name,
      conforms,
      violations,
      value,
    };
  } catch (error) {
    span.recordException(error);
    span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
    return {
      valid: false,
      shapeFound: true,
      error: error.message,
      conforms: [],
    };
  } finally {
    span.end();
  }
}

/**
 * Batch validate multiple results
 *
 * Validates multiple PICTL results and collects conformance status.
 *
 * @param {Array<Object>} results - Results to validate
 * @returns {Object} Batch validation result
 */
export function validateBatchResults(results) {
  const span = tracer.startSpan('pictl.validate_batch');
  try {
    const validations = results.map(result => ({
      result: result,
      validation: validatePictlResult(result),
    }));

    const validCount = validations.filter(v => v.validation.valid).length;
    const invalidCount = validations.length - validCount;

    span.addEvent('batch_validation_completed', {
      'pictl.total_results': results.length,
      'pictl.valid_count': validCount,
      'pictl.invalid_count': invalidCount,
    });

    span.setStatus({ code: SpanStatusCode.OK });
    return {
      totalResults: results.length,
      validCount,
      invalidCount,
      validationRate: results.length > 0 ? validCount / results.length : 0,
      validations,
    };
  } catch (error) {
    span.recordException(error);
    span.setStatus({ code: SpanStatusCode.ERROR, message: error.message });
    throw error;
  } finally {
    span.end();
  }
}

export { PICTL_SHAPES };
