/**
 * @file RDF Schema Builder - Fluent API for SHACL Shapes
 * @module @unrdf/core/validation/rdf-schema-builder
 * @description Fluent API for building SHACL shapes with method chaining
 */

import { z } from 'zod';
import { dataFactory } from '@unrdf/oxigraph';

const { namedNode, literal, blankNode, quad } = dataFactory;

/**
 * @typedef {import('@unrdf/oxigraph').Quad} Quad
 * @typedef {import('@unrdf/oxigraph').NamedNode} NamedNode
 */

/**
 * SHACL namespace
 */
const SH = 'http://www.w3.org/ns/shacl#';
const RDF = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';
const XSD = 'http://www.w3.org/2001/XMLSchema#';

/**
 * Property constraint builder
 */
class PropertyBuilder {
  /**
   * @param {string} propertyPath - Property path IRI
   * @param {ShapeBuilder} shapeBuilder - Parent shape builder
   */
  constructor(propertyPath, shapeBuilder) {
    this.propertyPath = propertyPath;
    this.shapeBuilder = shapeBuilder;
    this.constraints = {};
    this.propertyNode = blankNode();
  }

  /**
   * Set minimum cardinality
   * @param {number} count - Minimum count
   * @returns {PropertyBuilder} This builder for chaining
   *
   * @example
   * property('foaf:name').minCount(1).datatype('xsd:string')
   */
  minCount(count) {
    z.number().int().nonnegative().parse(count);
    this.constraints.minCount = count;
    return this;
  }

  /**
   * Set maximum cardinality
   * @param {number} count - Maximum count
   * @returns {PropertyBuilder} This builder for chaining
   *
   * @example
   * property('foaf:name').maxCount(1)
   */
  maxCount(count) {
    z.number().int().nonnegative().parse(count);
    this.constraints.maxCount = count;
    return this;
  }

  /**
   * Set datatype constraint
   * @param {string} datatypeIRI - Datatype IRI
   * @returns {PropertyBuilder} This builder for chaining
   *
   * @example
   * property('foaf:age').datatype('xsd:integer')
   */
  datatype(datatypeIRI) {
    z.string().min(1).parse(datatypeIRI);
    this.constraints.datatype = this._expandIRI(datatypeIRI);
    return this;
  }

  /**
   * Set node kind constraint
   * @param {string} nodeKind - Node kind (IRI, BlankNode, Literal)
   * @returns {PropertyBuilder} This builder for chaining
   *
   * @example
   * property('foaf:knows').nodeKind('IRI')
   */
  nodeKind(nodeKind) {
    const validKinds = ['IRI', 'BlankNode', 'Literal', 'BlankNodeOrIRI', 'BlankNodeOrLiteral', 'IRIOrLiteral'];
    if (!validKinds.includes(nodeKind)) {
      throw new Error(`Invalid node kind: ${nodeKind}`);
    }
    this.constraints.nodeKind = `${SH}${nodeKind}`;
    return this;
  }

  /**
   * Set pattern constraint
   * @param {string} pattern - Regular expression pattern
   * @param {string} [flags] - Regex flags
   * @returns {PropertyBuilder} This builder for chaining
   *
   * @example
   * property('foaf:email').pattern('^[a-z]+@[a-z]+\\.[a-z]+$')
   */
  pattern(pattern, flags) {
    z.string().min(1).parse(pattern);
    this.constraints.pattern = pattern;
    if (flags) {
      this.constraints.flags = flags;
    }
    return this;
  }

  /**
   * Set minimum inclusive value
   * @param {number} value - Minimum value (inclusive)
   * @returns {PropertyBuilder} This builder for chaining
   *
   * @example
   * property('foaf:age').minInclusive(0)
   */
  minInclusive(value) {
    z.number().parse(value);
    this.constraints.minInclusive = value;
    return this;
  }

  /**
   * Set maximum inclusive value
   * @param {number} value - Maximum value (inclusive)
   * @returns {PropertyBuilder} This builder for chaining
   *
   * @example
   * property('foaf:age').maxInclusive(120)
   */
  maxInclusive(value) {
    z.number().parse(value);
    this.constraints.maxInclusive = value;
    return this;
  }

  /**
   * Set minimum exclusive value
   * @param {number} value - Minimum value (exclusive)
   * @returns {PropertyBuilder} This builder for chaining
   */
  minExclusive(value) {
    z.number().parse(value);
    this.constraints.minExclusive = value;
    return this;
  }

  /**
   * Set maximum exclusive value
   * @param {number} value - Maximum value (exclusive)
   * @returns {PropertyBuilder} This builder for chaining
   */
  maxExclusive(value) {
    z.number().parse(value);
    this.constraints.maxExclusive = value;
    return this;
  }

  /**
   * Set unique language constraint
   * @returns {PropertyBuilder} This builder for chaining
   *
   * @example
   * property('rdfs:label').uniqueLang()
   */
  uniqueLang() {
    this.constraints.uniqueLang = true;
    return this;
  }

  /**
   * Set minimum length constraint
   * @param {number} length - Minimum length
   * @returns {PropertyBuilder} This builder for chaining
   */
  minLength(length) {
    z.number().int().nonnegative().parse(length);
    this.constraints.minLength = length;
    return this;
  }

  /**
   * Set maximum length constraint
   * @param {number} length - Maximum length
   * @returns {PropertyBuilder} This builder for chaining
   */
  maxLength(length) {
    z.number().int().nonnegative().parse(length);
    this.constraints.maxLength = length;
    return this;
  }

  /**
   * Set class constraint
   * @param {string} classIRI - Class IRI
   * @returns {PropertyBuilder} This builder for chaining
   */
  class(classIRI) {
    z.string().min(1).parse(classIRI);
    this.constraints.class = this._expandIRI(classIRI);
    return this;
  }

  /**
   * Add another property to the parent shape
   * @param {string} propertyPath - Property path IRI
   * @returns {PropertyBuilder} New property builder
   */
  property(propertyPath) {
    return this.shapeBuilder.property(propertyPath);
  }

  /**
   * Build the complete shape
   * @returns {Quad[]} Array of RDF quads
   */
  build() {
    return this.shapeBuilder.build();
  }

  /**
   * Expand prefixed IRI
   * @param {string} iri - IRI to expand
   * @returns {string} Expanded IRI
   * @private
   */
  _expandIRI(iri) {
    if (iri.startsWith('xsd:')) {
      return XSD + iri.slice(4);
    }
    return iri;
  }

  /**
   * Build property shape quads
   * @returns {Quad[]} Property shape quads
   * @private
   */
  buildQuads() {
    const quads = [];
    const pathNode = namedNode(this.propertyPath);

    // sh:path
    quads.push(quad(
      this.propertyNode,
      namedNode(`${SH}path`),
      pathNode
    ));

    // Add constraints
    for (const [key, value] of Object.entries(this.constraints)) {
      const predicate = namedNode(`${SH}${key}`);
      let object;

      if (typeof value === 'number') {
        object = literal(value.toString(), namedNode(`${XSD}integer`));
      } else if (typeof value === 'boolean') {
        object = literal(value.toString(), namedNode(`${XSD}boolean`));
      } else if (key === 'pattern' || key === 'flags') {
        // Pattern and flags are literals, not IRIs
        object = literal(value.toString());
      } else {
        object = namedNode(value);
      }

      quads.push(quad(this.propertyNode, predicate, object));
    }

    return quads;
  }
}

/**
 * Shape builder with fluent API
 */
class ShapeBuilder {
  /**
   * @param {string} [shapeIRI] - Shape IRI (auto-generated if not provided)
   */
  constructor(shapeIRI) {
    this.shapeNode = shapeIRI ? namedNode(shapeIRI) : blankNode();
    this.properties = [];
    this.targetClasses = [];
    this.logicalConstraints = [];
  }

  /**
   * Set target class for this shape
   * @param {string} classIRI - Target class IRI
   * @returns {ShapeBuilder} This builder for chaining
   *
   * @example
   * shacl().targetClass('ex:Person')
   */
  targetClass(classIRI) {
    z.string().min(1).parse(classIRI);
    this.targetClasses.push(classIRI);
    return this;
  }

  /**
   * Add a property constraint
   * @param {string} propertyPath - Property path IRI
   * @returns {PropertyBuilder} Property builder for chaining
   *
   * @example
   * shacl()
   *   .targetClass('ex:Person')
   *   .property('foaf:name')
   *     .minCount(1)
   *     .datatype('xsd:string')
   */
  property(propertyPath) {
    z.string().min(1).parse(propertyPath);
    const propBuilder = new PropertyBuilder(propertyPath, this);
    this.properties.push(propBuilder);
    return propBuilder;
  }

  /**
   * Add AND logical constraint
   * @param {ShapeBuilder[]} shapes - Shapes to combine with AND
   * @returns {ShapeBuilder} This builder for chaining
   *
   * @example
   * shacl().and([shape1, shape2])
   */
  and(shapes) {
    this.logicalConstraints.push({ type: 'and', shapes });
    return this;
  }

  /**
   * Add OR logical constraint
   * @param {ShapeBuilder[]} shapes - Shapes to combine with OR
   * @returns {ShapeBuilder} This builder for chaining
   *
   * @example
   * shacl().or([shape1, shape2])
   */
  or(shapes) {
    this.logicalConstraints.push({ type: 'or', shapes });
    return this;
  }

  /**
   * Add NOT logical constraint
   * @param {ShapeBuilder} shape - Shape to negate
   * @returns {ShapeBuilder} This builder for chaining
   *
   * @example
   * shacl().not(shape)
   */
  not(shape) {
    this.logicalConstraints.push({ type: 'not', shape });
    return this;
  }

  /**
   * Build the complete SHACL shape as RDF quads
   * @returns {Quad[]} Array of RDF quads representing the shape
   *
   * @example
   * const quads = shacl()
   *   .targetClass('ex:Person')
   *   .property('foaf:name').minCount(1).datatype('xsd:string')
   *   .build();
   */
  build() {
    const quads = [];

    // sh:NodeShape type
    quads.push(quad(
      this.shapeNode,
      namedNode(`${RDF}type`),
      namedNode(`${SH}NodeShape`)
    ));

    // Target classes
    for (const targetClass of this.targetClasses) {
      quads.push(quad(
        this.shapeNode,
        namedNode(`${SH}targetClass`),
        namedNode(targetClass)
      ));
    }

    // Properties
    for (const property of this.properties) {
      const propertyQuads = property.buildQuads();
      quads.push(...propertyQuads);

      // Link property to shape
      quads.push(quad(
        this.shapeNode,
        namedNode(`${SH}property`),
        property.propertyNode
      ));
    }

    // Logical constraints
    for (const constraint of this.logicalConstraints) {
      const listNode = blankNode();
      quads.push(quad(
        this.shapeNode,
        namedNode(`${SH}${constraint.type}`),
        listNode
      ));

      // Build RDF list (simplified)
      if (constraint.shapes) {
        for (const shape of constraint.shapes) {
          quads.push(...shape.build());
        }
      } else if (constraint.shape) {
        quads.push(...constraint.shape.build());
      }
    }

    return quads;
  }

  /**
   * Build as Turtle string
   * @returns {string} Turtle serialization of the shape
   */
  toTurtle() {
    const quads = this.build();
    const lines = ['@prefix sh: <http://www.w3.org/ns/shacl#> .'];
    lines.push('@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .');
    lines.push('');

    for (const q of quads) {
      lines.push(`${q.subject.value} ${q.predicate.value} ${q.object.value} .`);
    }

    return lines.join('\n');
  }
}

/**
 * Create a new SHACL shape builder
 * @param {string} [shapeIRI] - Optional shape IRI
 * @returns {ShapeBuilder} New shape builder instance
 *
 * @example
 * const shape = shacl('http://example.org/shapes/PersonShape')
 *   .targetClass('ex:Person')
 *   .property('foaf:name')
 *     .minCount(1)
 *     .datatype('xsd:string')
 *   .property('foaf:age')
 *     .datatype('xsd:integer')
 *     .minInclusive(0)
 *   .build();
 */
export function shacl(shapeIRI) {
  return new ShapeBuilder(shapeIRI);
}

/**
 * Create common shape patterns
 */
export const CommonShapes = {
  /**
   * Create an IRI shape pattern
   * @param {string} propertyPath - Property path
   * @param {Object} [options] - Additional options
   * @returns {ShapeBuilder} Shape builder
   */
  iri(propertyPath, options = {}) {
    const builder = new ShapeBuilder();
    builder.property(propertyPath).nodeKind('IRI');
    if (options.minCount !== undefined) {
      builder.property(propertyPath).minCount(options.minCount);
    }
    if (options.maxCount !== undefined) {
      builder.property(propertyPath).maxCount(options.maxCount);
    }
    return builder;
  },

  /**
   * Create a literal shape pattern
   * @param {string} propertyPath - Property path
   * @param {string} datatype - Datatype IRI
   * @param {Object} [options] - Additional options
   * @returns {ShapeBuilder} Shape builder
   */
  literal(propertyPath, datatype, options = {}) {
    const builder = new ShapeBuilder();
    builder.property(propertyPath).datatype(datatype).nodeKind('Literal');
    if (options.minCount !== undefined) {
      builder.property(propertyPath).minCount(options.minCount);
    }
    if (options.maxCount !== undefined) {
      builder.property(propertyPath).maxCount(options.maxCount);
    }
    if (options.pattern) {
      builder.property(propertyPath).pattern(options.pattern);
    }
    return builder;
  },

  /**
   * Create a string shape pattern
   * @param {string} propertyPath - Property path
   * @param {Object} [options] - Additional options
   * @returns {ShapeBuilder} Shape builder
   */
  string(propertyPath, options = {}) {
    return this.literal(propertyPath, 'xsd:string', options);
  },

  /**
   * Create an integer shape pattern
   * @param {string} propertyPath - Property path
   * @param {Object} [options] - Additional options
   * @returns {ShapeBuilder} Shape builder
   */
  integer(propertyPath, options = {}) {
    const builder = new ShapeBuilder();
    builder.property(propertyPath).datatype('xsd:integer').nodeKind('Literal');
    if (options.minCount !== undefined) {
      builder.property(propertyPath).minCount(options.minCount);
    }
    if (options.maxCount !== undefined) {
      builder.property(propertyPath).maxCount(options.maxCount);
    }
    if (options.minInclusive !== undefined) {
      builder.property(propertyPath).minInclusive(options.minInclusive);
    }
    if (options.maxInclusive !== undefined) {
      builder.property(propertyPath).maxInclusive(options.maxInclusive);
    }
    return builder;
  },
};

export { ShapeBuilder, PropertyBuilder };
