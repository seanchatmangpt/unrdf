/**
 * RDF Validation Framework
 *
 * Validates RDF triples and graphs against constraints using SHACL-like shapes.
 * Provides built-in validators for IRIs, literals, and property constraints.
 *
 * **Poka-Yoke Design**: Validation prevents invalid data from entering the store.
 * All validation errors are collected with specific messages.
 *
 * @module rdf-validator
 */

import { trace } from '@opentelemetry/api';

/**
 * Get tracer lazily to ensure provider is registered first
 * @returns {import('@opentelemetry/api').Tracer}
 */
function getTracer() {
  return trace.getTracer('rdf-validator');
}

/**
 * Common RDF namespace prefixes
 * @type {Object<string, string>}
 */
export const NAMESPACES = {
  xsd: 'http://www.w3.org/2001/XMLSchema#',
  rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
  foaf: 'http://xmlns.com/foaf/0.1/',
  schema: 'http://schema.org/',
  owl: 'http://www.w3.org/2002/07/owl#',
  dc: 'http://purl.org/dc/elements/1.1/',
  dcterms: 'http://purl.org/dc/terms/',
};

/**
 * IRI validation regex (RFC 3987 simplified)
 * @type {RegExp}
 */
const IRI_REGEX = /^[a-zA-Z][a-zA-Z0-9+.-]*:[^\s<>"{}|\\^`]*$/;

/**
 * Blank node regex
 * @type {RegExp}
 */
const BLANK_NODE_REGEX = /^_:[a-zA-Z0-9_.-]+$/;

/**
 * XSD datatype validators
 * @type {Object<string, (value: string) => boolean>}
 */
const DATATYPE_VALIDATORS = {
  'xsd:string': (value) => typeof value === 'string',
  'xsd:integer': (value) => /^-?\d+$/.test(value),
  'xsd:decimal': (value) => /^-?\d+(\.\d+)?$/.test(value),
  'xsd:double': (value) => /^-?\d+(\.\d+)?([eE][+-]?\d+)?$/.test(value),
  'xsd:float': (value) => /^-?\d+(\.\d+)?([eE][+-]?\d+)?$/.test(value),
  'xsd:boolean': (value) => value === 'true' || value === 'false' || value === '1' || value === '0',
  'xsd:date': (value) => /^\d{4}-\d{2}-\d{2}$/.test(value),
  'xsd:dateTime': (value) => /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(\.\d+)?(Z|[+-]\d{2}:\d{2})?$/.test(value),
  'xsd:time': (value) => /^\d{2}:\d{2}:\d{2}(\.\d+)?(Z|[+-]\d{2}:\d{2})?$/.test(value),
  'xsd:nonNegativeInteger': (value) => /^\d+$/.test(value) && parseInt(value, 10) >= 0,
  'xsd:positiveInteger': (value) => /^\d+$/.test(value) && parseInt(value, 10) > 0,
  'xsd:anyURI': (value) => IRI_REGEX.test(value) || /^[^\s<>"{}|\\^`]*$/.test(value),
};

/**
 * Validation error structure
 * @typedef {Object} ValidationError
 * @property {string} type - Error type (e.g., 'IRI_INVALID', 'DATATYPE_MISMATCH')
 * @property {string} message - Human-readable error message
 * @property {Object} [context] - Additional context (subject, property, value)
 * @property {'error' | 'warning'} severity - Error severity
 */

/**
 * Shape rule definition
 * @typedef {Object} ShapeRule
 * @property {string} property - Property IRI or prefixed name
 * @property {boolean} [required=false] - Whether property is required
 * @property {string} [datatype] - Expected datatype (prefixed or full IRI)
 * @property {number} [minValue] - Minimum numeric value (for numeric datatypes)
 * @property {number} [maxValue] - Maximum numeric value (for numeric datatypes)
 * @property {number} [minLength] - Minimum string length
 * @property {number} [maxLength] - Maximum string length
 * @property {RegExp} [pattern] - Regex pattern for string values
 * @property {number} [minCount=0] - Minimum occurrences
 * @property {number} [maxCount] - Maximum occurrences (undefined = unlimited)
 * @property {string[]} [in] - Allowed values (closed list)
 * @property {string} [nodeKind] - Expected node kind ('IRI', 'Literal', 'BlankNode')
 */

/**
 * Shape definition
 * @typedef {Object} ShapeDefinition
 * @property {string} shape - Shape name/IRI
 * @property {string} [targetClass] - Target class for this shape
 * @property {ShapeRule[]} rules - Validation rules
 */

/**
 * Triple structure
 * @typedef {Object} Triple
 * @property {string} subject - Subject IRI or blank node
 * @property {string} predicate - Predicate IRI
 * @property {string} value - Object value (IRI, literal, or blank node)
 * @property {string} [datatype] - Literal datatype
 * @property {string} [language] - Literal language tag
 */

/**
 * Validation result
 * @typedef {Object} ValidationResult
 * @property {boolean} valid - Whether validation passed
 * @property {ValidationError[]} errors - List of validation errors
 * @property {ValidationError[]} warnings - List of validation warnings
 * @property {number} triplesValidated - Number of triples validated
 */

/**
 * RDF Validator class
 *
 * **Poka-Yoke Design**: Validates RDF data before store insertion.
 * Prevents invalid data from corrupting the knowledge graph.
 */
export class RDFValidator {
  /**
   * Create RDF Validator
   *
   * @param {Object} [oxigraphBridge] - Optional OxigraphBridge for graph queries
   */
  constructor(oxigraphBridge = null) {
    /** @type {Object|null} */
    this.bridge = oxigraphBridge;

    /** @type {Map<string, ShapeDefinition>} */
    this.shapes = new Map();

    /** @type {ValidationError[]} */
    this.errors = [];

    /** @type {ValidationError[]} */
    this.warnings = [];

    /** @type {Object<string, string>} */
    this.prefixes = { ...NAMESPACES };

    // Register built-in shapes
    this._registerBuiltInShapes();
  }

  /**
   * Register built-in shapes for common RDF types
   * @private
   */
  _registerBuiltInShapes() {
    // FOAF Person shape
    this.registerShape('foaf:Person', [
      { property: 'foaf:name', required: true, datatype: 'xsd:string' },
      { property: 'foaf:mbox', required: false, nodeKind: 'IRI' },
      { property: 'foaf:age', required: false, datatype: 'xsd:integer', minValue: 0, maxValue: 150 },
      { property: 'foaf:knows', required: false, nodeKind: 'IRI', maxCount: undefined },
      { property: 'foaf:homepage', required: false, nodeKind: 'IRI' },
    ]);

    // Schema.org Person shape
    this.registerShape('schema:Person', [
      { property: 'schema:name', required: true, datatype: 'xsd:string' },
      { property: 'schema:email', required: false, datatype: 'xsd:string' },
      { property: 'schema:birthDate', required: false, datatype: 'xsd:date' },
      { property: 'schema:url', required: false, nodeKind: 'IRI' },
    ]);

    // Schema.org Thing shape (base shape)
    this.registerShape('schema:Thing', [
      { property: 'schema:name', required: false, datatype: 'xsd:string' },
      { property: 'schema:description', required: false, datatype: 'xsd:string' },
      { property: 'schema:url', required: false, nodeKind: 'IRI' },
    ]);

    // Schema.org Organization shape
    this.registerShape('schema:Organization', [
      { property: 'schema:name', required: true, datatype: 'xsd:string' },
      { property: 'schema:url', required: false, nodeKind: 'IRI' },
      { property: 'schema:email', required: false, datatype: 'xsd:string' },
      { property: 'schema:founder', required: false, nodeKind: 'IRI' },
    ]);

    // RDFS Class shape
    this.registerShape('rdfs:Class', [
      { property: 'rdfs:label', required: false, datatype: 'xsd:string' },
      { property: 'rdfs:comment', required: false, datatype: 'xsd:string' },
      { property: 'rdfs:subClassOf', required: false, nodeKind: 'IRI' },
    ]);

    // OWL Class shape
    this.registerShape('owl:Class', [
      { property: 'rdfs:label', required: false, datatype: 'xsd:string' },
      { property: 'rdfs:comment', required: false, datatype: 'xsd:string' },
      { property: 'owl:equivalentClass', required: false, nodeKind: 'IRI' },
    ]);
  }

  /**
   * Expand prefixed name to full IRI
   *
   * @param {string} prefixedName - Prefixed name (e.g., 'foaf:name')
   * @returns {string} Full IRI or original if not prefixed
   */
  expandPrefix(prefixedName) {
    if (!prefixedName || typeof prefixedName !== 'string') {
      return prefixedName;
    }
    const colonIndex = prefixedName.indexOf(':');
    if (colonIndex === -1) {
      return prefixedName;
    }
    // Check if it's already a full IRI (http://, https://, etc.)
    if (prefixedName.includes('://')) {
      return prefixedName;
    }
    const prefix = prefixedName.substring(0, colonIndex);
    const localName = prefixedName.substring(colonIndex + 1);
    const namespace = this.prefixes[prefix];
    return namespace ? namespace + localName : prefixedName;
  }

  /**
   * Validate IRI format
   *
   * **Poka-Yoke**: Ensures IRI conforms to RFC 3987 format
   *
   * @param {string} value - Value to validate as IRI
   * @returns {{valid: boolean, error?: ValidationError}}
   */
  validateIRI(value) {
    if (!value || typeof value !== 'string') {
      return {
        valid: false,
        error: {
          type: 'IRI_INVALID',
          message: 'IRI must be a non-empty string',
          context: { value },
          severity: 'error',
        },
      };
    }

    // Expand prefix if present
    const expanded = this.expandPrefix(value);

    if (!IRI_REGEX.test(expanded)) {
      return {
        valid: false,
        error: {
          type: 'IRI_INVALID',
          message: `Invalid IRI format: "${value}" (expanded: "${expanded}")`,
          context: { value, expanded },
          severity: 'error',
        },
      };
    }

    return { valid: true };
  }

  /**
   * Validate literal value against datatype
   *
   * **Poka-Yoke**: Ensures literal conforms to XSD datatype
   *
   * @param {string} value - Literal value
   * @param {string} datatype - Datatype IRI or prefixed name
   * @returns {{valid: boolean, error?: ValidationError}}
   */
  validateLiteral(value, datatype) {
    if (value === null || value === undefined) {
      return {
        valid: false,
        error: {
          type: 'LITERAL_INVALID',
          message: 'Literal value cannot be null or undefined',
          context: { value, datatype },
          severity: 'error',
        },
      };
    }

    // Convert to string for validation
    const stringValue = String(value);

    // If no datatype specified, treat as xsd:string
    if (!datatype) {
      return { valid: true };
    }

    // Normalize datatype (support both prefixed and full IRI)
    let normalizedDatatype = datatype;
    if (datatype.startsWith(NAMESPACES.xsd)) {
      normalizedDatatype = 'xsd:' + datatype.substring(NAMESPACES.xsd.length);
    }

    const validator = DATATYPE_VALIDATORS[normalizedDatatype];
    if (!validator) {
      // Unknown datatype - warn but don't fail
      return {
        valid: true,
        error: {
          type: 'DATATYPE_UNKNOWN',
          message: `Unknown datatype: "${datatype}" - validation skipped`,
          context: { value: stringValue, datatype },
          severity: 'warning',
        },
      };
    }

    if (!validator(stringValue)) {
      return {
        valid: false,
        error: {
          type: 'DATATYPE_MISMATCH',
          message: `Value "${stringValue}" does not match datatype "${datatype}"`,
          context: { value: stringValue, datatype },
          severity: 'error',
        },
      };
    }

    return { valid: true };
  }

  /**
   * Validate that required property is present for a subject
   *
   * @param {string} subject - Subject IRI
   * @param {string} property - Property IRI
   * @param {Triple[]} triples - Triples to search
   * @returns {{valid: boolean, error?: ValidationError}}
   */
  validateProperty(subject, property, triples = []) {
    const expandedSubject = this.expandPrefix(subject);
    const expandedProperty = this.expandPrefix(property);

    const hasProperty = triples.some(
      (t) =>
        this.expandPrefix(t.subject) === expandedSubject &&
        this.expandPrefix(t.predicate) === expandedProperty
    );

    if (!hasProperty) {
      return {
        valid: false,
        error: {
          type: 'PROPERTY_MISSING',
          message: `Required property "${property}" missing for subject "${subject}"`,
          context: { subject, property },
          severity: 'error',
        },
      };
    }

    return { valid: true };
  }

  /**
   * Register a validation shape
   *
   * @param {string} shapeName - Shape name/IRI
   * @param {ShapeRule[]} rules - Validation rules
   * @param {string} [targetClass] - Optional target class
   */
  registerShape(shapeName, rules, targetClass = undefined) {
    if (!shapeName || typeof shapeName !== 'string') {
      throw new Error('Shape name must be a non-empty string');
    }
    if (!Array.isArray(rules)) {
      throw new Error('Rules must be an array');
    }

    /** @type {ShapeDefinition} */
    const shape = {
      shape: shapeName,
      targetClass: targetClass || shapeName,
      rules: rules.map((rule) => ({
        property: rule.property,
        required: rule.required ?? false,
        datatype: rule.datatype,
        minValue: rule.minValue,
        maxValue: rule.maxValue,
        minLength: rule.minLength,
        maxLength: rule.maxLength,
        pattern: rule.pattern,
        minCount: rule.minCount ?? 0,
        maxCount: rule.maxCount,
        in: rule.in,
        nodeKind: rule.nodeKind,
      })),
    };

    this.shapes.set(shapeName, shape);
  }

  /**
   * Validate a single triple
   *
   * @param {Triple} triple - Triple to validate
   * @returns {{valid: boolean, errors: ValidationError[]}}
   */
  validateTriple(triple) {
    const errors = [];

    if (!triple || typeof triple !== 'object') {
      errors.push({
        type: 'TRIPLE_INVALID',
        message: 'Triple must be a non-null object',
        context: { triple },
        severity: 'error',
      });
      return { valid: false, errors };
    }

    // Validate subject (IRI or blank node)
    if (!triple.subject) {
      errors.push({
        type: 'SUBJECT_MISSING',
        message: 'Triple must have a subject',
        context: { triple },
        severity: 'error',
      });
    } else if (!BLANK_NODE_REGEX.test(triple.subject)) {
      const iriResult = this.validateIRI(triple.subject);
      if (!iriResult.valid && iriResult.error) {
        errors.push({
          ...iriResult.error,
          message: `Invalid subject: ${iriResult.error.message}`,
        });
      }
    }

    // Validate predicate (must be IRI)
    if (!triple.predicate) {
      errors.push({
        type: 'PREDICATE_MISSING',
        message: 'Triple must have a predicate',
        context: { triple },
        severity: 'error',
      });
    } else {
      const iriResult = this.validateIRI(triple.predicate);
      if (!iriResult.valid && iriResult.error) {
        errors.push({
          ...iriResult.error,
          message: `Invalid predicate: ${iriResult.error.message}`,
        });
      }
    }

    // Validate object/value
    if (triple.value === undefined || triple.value === null) {
      errors.push({
        type: 'OBJECT_MISSING',
        message: 'Triple must have an object value',
        context: { triple },
        severity: 'error',
      });
    } else if (triple.datatype || triple.language) {
      // It's a literal - validate against datatype if provided
      if (triple.datatype) {
        const literalResult = this.validateLiteral(triple.value, triple.datatype);
        if (!literalResult.valid && literalResult.error) {
          errors.push(literalResult.error);
        }
      }
    } else {
      // Could be IRI or blank node
      const valueStr = String(triple.value);
      if (!BLANK_NODE_REGEX.test(valueStr) && !IRI_REGEX.test(this.expandPrefix(valueStr))) {
        // Assume it's a plain literal (xsd:string)
        // This is valid - no error needed
      }
    }

    return { valid: errors.length === 0, errors };
  }

  /**
   * Validate triples against a registered shape
   *
   * @param {string} shapeName - Shape name to validate against
   * @param {string} subject - Subject IRI to validate
   * @param {Triple[]} triples - Triples describing the subject
   * @returns {{valid: boolean, errors: ValidationError[]}}
   */
  validateAgainstShape(shapeName, subject, triples) {
    const errors = [];
    const shape = this.shapes.get(shapeName);

    if (!shape) {
      errors.push({
        type: 'SHAPE_NOT_FOUND',
        message: `Shape "${shapeName}" not registered`,
        context: { shapeName },
        severity: 'error',
      });
      return { valid: false, errors };
    }

    const expandedSubject = this.expandPrefix(subject);
    const subjectTriples = triples.filter(
      (t) => this.expandPrefix(t.subject) === expandedSubject
    );

    for (const rule of shape.rules) {
      const expandedProperty = this.expandPrefix(rule.property);
      const propertyTriples = subjectTriples.filter(
        (t) => this.expandPrefix(t.predicate) === expandedProperty
      );

      // Check required
      if (rule.required && propertyTriples.length === 0) {
        errors.push({
          type: 'PROPERTY_MISSING',
          message: `Required property "${rule.property}" missing for "${subject}"`,
          context: { subject, property: rule.property, shape: shapeName },
          severity: 'error',
        });
        continue;
      }

      // Check min/max count
      if (rule.minCount !== undefined && propertyTriples.length < rule.minCount) {
        errors.push({
          type: 'MIN_COUNT_VIOLATION',
          message: `Property "${rule.property}" has ${propertyTriples.length} values, minimum is ${rule.minCount}`,
          context: { subject, property: rule.property, count: propertyTriples.length, minCount: rule.minCount },
          severity: 'error',
        });
      }

      if (rule.maxCount !== undefined && propertyTriples.length > rule.maxCount) {
        errors.push({
          type: 'MAX_COUNT_VIOLATION',
          message: `Property "${rule.property}" has ${propertyTriples.length} values, maximum is ${rule.maxCount}`,
          context: { subject, property: rule.property, count: propertyTriples.length, maxCount: rule.maxCount },
          severity: 'error',
        });
      }

      // Validate each value
      for (const triple of propertyTriples) {
        // Check nodeKind
        if (rule.nodeKind) {
          const valueStr = String(triple.value);
          const isIRI = IRI_REGEX.test(this.expandPrefix(valueStr));
          const isBlankNode = BLANK_NODE_REGEX.test(valueStr);
          const isLiteral = triple.datatype !== undefined || triple.language !== undefined || (!isIRI && !isBlankNode);

          if (rule.nodeKind === 'IRI' && !isIRI) {
            errors.push({
              type: 'NODE_KIND_VIOLATION',
              message: `Property "${rule.property}" expects IRI, got "${valueStr}"`,
              context: { subject, property: rule.property, value: valueStr, expectedKind: 'IRI' },
              severity: 'error',
            });
          } else if (rule.nodeKind === 'Literal' && !isLiteral) {
            errors.push({
              type: 'NODE_KIND_VIOLATION',
              message: `Property "${rule.property}" expects Literal, got IRI/BlankNode`,
              context: { subject, property: rule.property, value: valueStr, expectedKind: 'Literal' },
              severity: 'error',
            });
          } else if (rule.nodeKind === 'BlankNode' && !isBlankNode) {
            errors.push({
              type: 'NODE_KIND_VIOLATION',
              message: `Property "${rule.property}" expects BlankNode, got "${valueStr}"`,
              context: { subject, property: rule.property, value: valueStr, expectedKind: 'BlankNode' },
              severity: 'error',
            });
          }
        }

        // Check datatype
        if (rule.datatype) {
          const literalResult = this.validateLiteral(triple.value, rule.datatype);
          if (!literalResult.valid && literalResult.error) {
            errors.push({
              ...literalResult.error,
              context: {
                ...literalResult.error.context,
                subject,
                property: rule.property,
                shape: shapeName,
              },
            });
          }
        }

        // Check numeric range
        if (rule.minValue !== undefined || rule.maxValue !== undefined) {
          const numValue = parseFloat(String(triple.value));
          if (!isNaN(numValue)) {
            if (rule.minValue !== undefined && numValue < rule.minValue) {
              errors.push({
                type: 'MIN_VALUE_VIOLATION',
                message: `Value ${numValue} is less than minimum ${rule.minValue} for "${rule.property}"`,
                context: { subject, property: rule.property, value: numValue, minValue: rule.minValue },
                severity: 'error',
              });
            }
            if (rule.maxValue !== undefined && numValue > rule.maxValue) {
              errors.push({
                type: 'MAX_VALUE_VIOLATION',
                message: `Value ${numValue} exceeds maximum ${rule.maxValue} for "${rule.property}"`,
                context: { subject, property: rule.property, value: numValue, maxValue: rule.maxValue },
                severity: 'error',
              });
            }
          }
        }

        // Check string constraints
        const stringValue = String(triple.value);
        if (rule.minLength !== undefined && stringValue.length < rule.minLength) {
          errors.push({
            type: 'MIN_LENGTH_VIOLATION',
            message: `Value length ${stringValue.length} is less than minimum ${rule.minLength}`,
            context: { subject, property: rule.property, length: stringValue.length, minLength: rule.minLength },
            severity: 'error',
          });
        }
        if (rule.maxLength !== undefined && stringValue.length > rule.maxLength) {
          errors.push({
            type: 'MAX_LENGTH_VIOLATION',
            message: `Value length ${stringValue.length} exceeds maximum ${rule.maxLength}`,
            context: { subject, property: rule.property, length: stringValue.length, maxLength: rule.maxLength },
            severity: 'error',
          });
        }

        // Check pattern
        if (rule.pattern && !rule.pattern.test(stringValue)) {
          errors.push({
            type: 'PATTERN_VIOLATION',
            message: `Value "${stringValue}" does not match pattern ${rule.pattern}`,
            context: { subject, property: rule.property, value: stringValue, pattern: rule.pattern.toString() },
            severity: 'error',
          });
        }

        // Check in (allowed values)
        if (rule.in && !rule.in.includes(stringValue)) {
          errors.push({
            type: 'IN_VIOLATION',
            message: `Value "${stringValue}" is not in allowed values: [${rule.in.join(', ')}]`,
            context: { subject, property: rule.property, value: stringValue, allowedValues: rule.in },
            severity: 'error',
          });
        }
      }
    }

    return { valid: errors.length === 0, errors };
  }

  /**
   * Validate all triples in a graph
   *
   * @param {Triple[]} triples - Triples to validate
   * @param {Object} [options] - Validation options
   * @param {string[]} [options.shapes] - Specific shapes to validate against
   * @param {boolean} [options.validateStructure=true] - Validate triple structure
   * @returns {Promise<ValidationResult>}
   */
  async validateGraph(triples, options = {}) {
    const { shapes: shapeNames, validateStructure = true } = options;

    return getTracer().startActiveSpan(
      'rdf.validate_graph',
      {
        attributes: {
          'validation.triple_count': triples.length,
          'validation.shape_count': shapeNames?.length ?? 0,
        },
      },
      async (span) => {
        this.errors = [];
        this.warnings = [];
        let triplesValidated = 0;

        try {
          // Validate triple structure
          if (validateStructure) {
            for (const triple of triples) {
              const result = this.validateTriple(triple);
              triplesValidated++;
              for (const error of result.errors) {
                if (error.severity === 'warning') {
                  this.warnings.push(error);
                } else {
                  this.errors.push(error);
                }
              }
            }
          }

          // Validate against shapes if specified
          if (shapeNames && shapeNames.length > 0) {
            // Group triples by subject
            const subjectTriples = new Map();
            for (const triple of triples) {
              const subject = this.expandPrefix(triple.subject);
              if (!subjectTriples.has(subject)) {
                subjectTriples.set(subject, []);
              }
              subjectTriples.get(subject).push(triple);
            }

            // Find subjects with rdf:type matching shape target classes
            for (const shapeName of shapeNames) {
              const shape = this.shapes.get(shapeName);
              if (!shape) {
                this.warnings.push({
                  type: 'SHAPE_NOT_FOUND',
                  message: `Shape "${shapeName}" not registered - skipping`,
                  context: { shapeName },
                  severity: 'warning',
                });
                continue;
              }

              const targetClass = this.expandPrefix(shape.targetClass);
              const rdfType = this.expandPrefix('rdf:type');

              for (const [subject, subjectTriplesArr] of subjectTriples) {
                // Check if subject has rdf:type = targetClass
                const hasTargetType = subjectTriplesArr.some(
                  (t) =>
                    this.expandPrefix(t.predicate) === rdfType &&
                    this.expandPrefix(String(t.value)) === targetClass
                );

                if (hasTargetType) {
                  const result = this.validateAgainstShape(shapeName, subject, triples);
                  for (const error of result.errors) {
                    if (error.severity === 'warning') {
                      this.warnings.push(error);
                    } else {
                      this.errors.push(error);
                    }
                  }
                }
              }
            }
          }

          const valid = this.errors.length === 0;
          span.setAttribute('validation.valid', valid);
          span.setAttribute('validation.error_count', this.errors.length);
          span.setAttribute('validation.warning_count', this.warnings.length);

          if (!valid) {
            span.setStatus({ code: 2, message: `${this.errors.length} validation errors` });
          } else {
            span.setStatus({ code: 1 }); // OK
          }

          return {
            valid,
            errors: [...this.errors],
            warnings: [...this.warnings],
            triplesValidated,
          };
        } catch (error) {
          span.setStatus({ code: 2, message: error.message });
          throw error;
        } finally {
          span.end();
        }
      }
    );
  }

  /**
   * Get accumulated validation errors
   *
   * @returns {ValidationError[]}
   */
  getValidationErrors() {
    return [...this.errors];
  }

  /**
   * Get accumulated validation warnings
   *
   * @returns {ValidationError[]}
   */
  getValidationWarnings() {
    return [...this.warnings];
  }

  /**
   * Clear accumulated errors and warnings
   */
  clearErrors() {
    this.errors = [];
    this.warnings = [];
  }

  /**
   * Get registered shape names
   *
   * @returns {string[]}
   */
  getRegisteredShapes() {
    return Array.from(this.shapes.keys());
  }

  /**
   * Get shape definition
   *
   * @param {string} shapeName - Shape name
   * @returns {ShapeDefinition|undefined}
   */
  getShape(shapeName) {
    return this.shapes.get(shapeName);
  }

  /**
   * Add namespace prefix
   *
   * @param {string} prefix - Prefix (without colon)
   * @param {string} namespace - Full namespace IRI
   */
  addPrefix(prefix, namespace) {
    if (!prefix || typeof prefix !== 'string') {
      throw new Error('Prefix must be a non-empty string');
    }
    if (!namespace || typeof namespace !== 'string') {
      throw new Error('Namespace must be a non-empty string');
    }
    this.prefixes[prefix] = namespace;
  }
}

/**
 * Create pre-insertion validator function
 *
 * **Poka-Yoke**: Returns a function that validates triples before insertion.
 * Use this to guard store.addTriples() calls.
 *
 * @param {RDFValidator} validator - Validator instance
 * @param {Object} [options] - Options
 * @param {boolean} [options.throwOnError=true] - Throw on validation error
 * @param {string[]} [options.shapes] - Shapes to validate against
 * @returns {(triples: Triple[]) => Promise<{valid: boolean, errors: ValidationError[], triples: Triple[]}>}
 */
export function createPreInsertionValidator(validator, options = {}) {
  const { throwOnError = true, shapes } = options;

  return async (triples) => {
    const result = await validator.validateGraph(triples, {
      shapes,
      validateStructure: true,
    });

    if (!result.valid && throwOnError) {
      const errorMessages = result.errors
        .map((e) => e.message)
        .slice(0, 5)
        .join('; ');
      throw new Error(`RDF validation failed: ${errorMessages}${result.errors.length > 5 ? ` (and ${result.errors.length - 5} more)` : ''}`);
    }

    return {
      valid: result.valid,
      errors: result.errors,
      triples,
    };
  };
}
