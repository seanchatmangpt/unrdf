import { DataFactory } from 'n3';
import { asNamedNode, asLiteral, _isNamedNode, _isLiteral, _isBlankNode } from './term-utils.mjs';

const { quad } = DataFactory;

/**
 * Convert quad → plain JSON
 * @param {import('n3').Quad} q - RDF quad to convert
 * @returns {Object} Plain JSON representation of the quad
 */
export const quadToJSON = q => ({
  subject: q.subject.value,
  predicate: q.predicate.value,
  object: q.object.value,
  graph: q.graph.termType === 'DefaultGraph' ? null : q.graph.value,
  // Include term types for reconstruction
  subjectType: q.subject.termType,
  objectType: q.object.termType,
  graphType: q.graph.termType,
});

/**
 * Convert JSON → quad
 * @param {Object} obj - JSON object with quad data
 * @param {string} obj.subject - Subject IRI
 * @param {string} obj.predicate - Predicate IRI
 * @param {string} obj.object - Object value
 * @param {string|null} [obj.graph] - Graph IRI (optional)
 * @returns {import('n3').Quad} RDF quad
 */
export const jsonToQuad = obj => {
  const subject = asNamedNode(obj.subject);
  const predicate = asNamedNode(obj.predicate);

  // Smart object handling - check if it looks like an IRI
  const object =
    obj.object.startsWith('http') || obj.object.startsWith('urn:')
      ? asNamedNode(obj.object)
      : asLiteral(obj.object);

  const graph = obj.graph ? asNamedNode(obj.graph) : undefined;

  return quad(subject, predicate, object, graph);
};

/**
 * Convert array of quads to JSON
 * @param {import('n3').Quad[]} quads - Array of RDF quads
 * @returns {Object[]} Array of JSON objects
 */
export const quadsToJSON = quads => quads.map(quadToJSON);

/**
 * Convert array of JSON objects to quads
 * @param {Object[]} jsonArray - Array of JSON quad objects
 * @returns {import('n3').Quad[]} Array of RDF quads
 */
export const jsonToQuads = jsonArray => jsonArray.map(jsonToQuad);

/**
 * Extract all unique subjects from quads
 * @param {import('n3').Quad[]} quads - Array of RDF quads
 * @returns {string[]} Array of unique subject IRIs
 */
export const extractSubjects = quads => [...new Set(quads.map(q => q.subject.value))];

/**
 * Extract all unique predicates from quads
 * @param {import('n3').Quad[]} quads - Array of RDF quads
 * @returns {string[]} Array of unique predicate IRIs
 */
export const extractPredicates = quads => [...new Set(quads.map(q => q.predicate.value))];

/**
 * Extract all unique objects from quads
 * @param {import('n3').Quad[]} quads - Array of RDF quads
 * @returns {string[]} Array of unique object values
 */
export const extractObjects = quads => [...new Set(quads.map(q => q.object.value))];

/**
 * Filter quads by subject
 * @param {import('n3').Quad[]} quads - Array of RDF quads
 * @param {string} subjectIRI - Subject IRI to filter by
 * @returns {import('n3').Quad[]} Filtered quads
 */
export const filterBySubject = (quads, subjectIRI) =>
  quads.filter(q => q.subject.value === subjectIRI);

/**
 * Filter quads by predicate
 * @param {import('n3').Quad[]} quads - Array of RDF quads
 * @param {string} predicateIRI - Predicate IRI to filter by
 * @returns {import('n3').Quad[]} Filtered quads
 */
export const filterByPredicate = (quads, predicateIRI) =>
  quads.filter(q => q.predicate.value === predicateIRI);

/**
 * Filter quads by object
 * @param {import('n3').Quad[]} quads - Array of RDF quads
 * @param {string} objectValue - Object value to filter by
 * @returns {import('n3').Quad[]} Filtered quads
 */
export const filterByObject = (quads, objectValue) =>
  quads.filter(q => q.object.value === objectValue);

/**
 * Group quads by subject
 * @param {import('n3').Quad[]} quads - Array of RDF quads
 * @returns {Map<string, import('n3').Quad[]>} Map of subject IRIs to their quads
 */
export const groupBySubject = quads => {
  const groups = new Map();
  for (const q of quads) {
    const subject = q.subject.value;
    if (!groups.has(subject)) {
      groups.set(subject, []);
    }
    groups.get(subject).push(q);
  }
  return groups;
};

/**
 * Group quads by predicate
 * @param {import('n3').Quad[]} quads - Array of RDF quads
 * @returns {Map<string, import('n3').Quad[]>} Map of predicate IRIs to their quads
 */
export const groupByPredicate = quads => {
  const groups = new Map();
  for (const q of quads) {
    const predicate = q.predicate.value;
    if (!groups.has(predicate)) {
      groups.set(predicate, []);
    }
    groups.get(predicate).push(q);
  }
  return groups;
};
