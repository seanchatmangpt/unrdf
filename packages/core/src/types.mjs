/**
 * @file RDF term creation utilities
 * @module @unrdf/core/types
 */

import { DataFactory } from 'n3';

const { namedNode, literal, blankNode, variable, defaultGraph, quad } = DataFactory;

/**
 * Create RDF terms from simple values
 * @param {Object} data - Data to convert to terms
 * @returns {Object} Object with created terms
 *
 * @example
 * const terms = createTerms({
 *   subject: 'http://example.org/alice',
 *   name: 'Alice',
 *   age: 30
 * });
 *
 * // Returns:
 * // {
 * //   subject: NamedNode('http://example.org/alice'),
 * //   name: Literal('Alice'),
 * //   age: Literal('30', XSD.integer)
 * // }
 */
export function createTerms(data) {
  const terms = {};

  for (const [key, value] of Object.entries(data)) {
    if (value === null || value === undefined) {
      continue;
    }

    // Detect term type based on value
    if (typeof value === 'string') {
      // Check if it's a URL
      if (value.startsWith('http://') || value.startsWith('https://') || value.startsWith('urn:')) {
        terms[key] = namedNode(value);
      } else if (value.startsWith('_:')) {
        // Blank node
        terms[key] = blankNode(value.slice(2));
      } else if (value.startsWith('?')) {
        // Variable
        terms[key] = variable(value.slice(1));
      } else {
        // Plain literal
        terms[key] = literal(value);
      }
    } else if (typeof value === 'number') {
      // Numeric literal
      if (Number.isInteger(value)) {
        terms[key] = literal(
          value.toString(),
          namedNode('http://www.w3.org/2001/XMLSchema#integer')
        );
      } else {
        terms[key] = literal(
          value.toString(),
          namedNode('http://www.w3.org/2001/XMLSchema#double')
        );
      }
    } else if (typeof value === 'boolean') {
      // Boolean literal
      terms[key] = literal(value.toString(), namedNode('http://www.w3.org/2001/XMLSchema#boolean'));
    } else if (value instanceof Date) {
      // DateTime literal
      terms[key] = literal(
        value.toISOString(),
        namedNode('http://www.w3.org/2001/XMLSchema#dateTime')
      );
    } else {
      // Default to string literal
      terms[key] = literal(String(value));
    }
  }

  return terms;
}

/**
 * Create a named node
 * @param {string} iri - IRI string
 * @returns {NamedNode} Named node
 *
 * @example
 * const alice = createNamedNode('http://example.org/alice');
 */
export function createNamedNode(iri) {
  return namedNode(iri);
}

/**
 * Create a literal
 * @param {string} value - Literal value
 * @param {NamedNode|string} [datatype] - Datatype IRI or named node
 * @param {string} [language] - Language tag
 * @returns {Literal} Literal term
 *
 * @example
 * const name = createLiteral('Alice');
 * const age = createLiteral('30', 'http://www.w3.org/2001/XMLSchema#integer');
 * const label = createLiteral('Nom', null, 'fr');
 */
export function createLiteral(value, datatype = null, language = null) {
  if (language) {
    return literal(value, language);
  }
  if (datatype) {
    const dt = typeof datatype === 'string' ? namedNode(datatype) : datatype;
    return literal(value, dt);
  }
  return literal(value);
}

/**
 * Create a blank node
 * @param {string} [id] - Blank node identifier (auto-generated if not provided)
 * @returns {BlankNode} Blank node
 *
 * @example
 * const blank1 = createBlankNode();
 * const blank2 = createBlankNode('person1');
 */
export function createBlankNode(id) {
  return blankNode(id);
}

/**
 * Create a variable
 * @param {string} name - Variable name
 * @returns {Variable} Variable term
 *
 * @example
 * const nameVar = createVariable('name');
 */
export function createVariable(name) {
  return variable(name);
}

/**
 * Create a quad
 * @param {Term} subject - Subject term
 * @param {Term} predicate - Predicate term
 * @param {Term} object - Object term
 * @param {Term} [graph] - Graph term (default graph if not provided)
 * @returns {Quad} Quad
 *
 * @example
 * const q = createQuad(
 *   namedNode('http://example.org/alice'),
 *   namedNode('http://xmlns.com/foaf/0.1/name'),
 *   literal('Alice')
 * );
 */
export function createQuad(subject, predicate, object, graph) {
  return quad(subject, predicate, object, graph || defaultGraph());
}

// Re-export DataFactory functions
export { namedNode, literal, blankNode, variable, defaultGraph, quad };
