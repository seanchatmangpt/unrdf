import { asNamedNode, asLiteral, getIRI } from "./term-utils.mjs";

/**
 * Get all objects for subject+predicate
 * @param {import('n3').Store} store - RDF store to query
 * @param {string|import('n3').NamedNode} subject - Subject IRI or NamedNode
 * @param {string|import('n3').NamedNode} predicate - Predicate IRI or NamedNode
 * @returns {import('n3').Term[]} Array of object terms
 */
export const getObjects = (store, subject, predicate) =>
  store.getObjects(subject, asNamedNode(predicate), null);

/**
 * Get all subjects for predicate+object
 * @param {import('n3').Store} store - RDF store to query
 * @param {string|import('n3').NamedNode} predicate - Predicate IRI or NamedNode
 * @param {string|import('n3').Term} object - Object value or term
 * @returns {import('n3').Term[]} Array of subject terms
 */
export const getSubjects = (store, predicate, object) =>
  store.getSubjects(asNamedNode(predicate), object, null);

/**
 * Get all predicates for subject+object
 * @param {import('n3').Store} store - RDF store to query
 * @param {string|import('n3').NamedNode} subject - Subject IRI or NamedNode
 * @param {string|import('n3').Term} object - Object value or term
 * @returns {import('n3').Term[]} Array of predicate terms
 */
export const getPredicates = (store, subject, object) =>
  store.getPredicates(subject, null, object);

/**
 * Does subject have rdf:type of typeIRI
 * @param {import('n3').Store} store - RDF store to query
 * @param {string|import('n3').NamedNode} subject - Subject IRI or NamedNode
 * @param {string|import('n3').NamedNode} typeIRI - Type IRI or NamedNode
 * @returns {boolean} True if subject has the specified type
 */
export const isA = (store, subject, typeIRI) =>
  store.countQuads(
    subject, 
    asNamedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"), 
    asNamedNode(typeIRI), 
    null
  ) > 0;

/**
 * Get all types for a subject
 * @param {import('n3').Store} store - RDF store to query
 * @param {string|import('n3').NamedNode} subject - Subject IRI or NamedNode
 * @returns {string[]} Array of type IRIs
 */
export const getTypes = (store, subject) =>
  getObjects(store, subject, "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
    .map(term => getIRI(term));

/**
 * Predicate pluck (all values across subjects)
 * @param {import('n3').Store} store - RDF store to query
 * @param {string|import('n3').NamedNode} predicateIRI - Predicate IRI or NamedNode
 * @returns {import('n3').Quad[]} Array of quads with the specified predicate
 */
export const pluck = (store, predicateIRI) =>
  store.getQuads(null, asNamedNode(predicateIRI), null, null);

/**
 * Index subject → objects by predicate
 * @param {import('n3').Store} store - RDF store to query
 * @param {string|import('n3').NamedNode} predicateIRI - Predicate IRI or NamedNode
 * @returns {Map<string, string[]>} Map of subject IRIs to their object values
 */
export const indexByPredicate = (store, predicateIRI) => {
  const out = new Map();
  for (const q of pluck(store, predicateIRI)) {
    const s = q.subject.value;
    if (!out.has(s)) out.set(s, []);
    out.get(s).push(q.object.value);
  }
  return out;
};

/**
 * Get all properties for a subject
 * @param {import('n3').Store} store - RDF store to query
 * @param {string|import('n3').NamedNode} subject - Subject IRI or NamedNode
 * @returns {Map<string, string[]>} Map of predicate IRIs to their object values
 */
export const getProperties = (store, subject) => {
  const properties = new Map();
  const quads = store.getQuads(subject, null, null, null);
  
  for (const q of quads) {
    const predicate = q.predicate.value;
    if (!properties.has(predicate)) {
      properties.set(predicate, []);
    }
    properties.get(predicate).push(q.object.value);
  }
  
  return properties;
};

/**
 * Check if a subject exists in the store
 * @param {import('n3').Store} store - RDF store to query
 * @param {string|import('n3').NamedNode} subject - Subject IRI or NamedNode
 * @returns {boolean} True if subject exists
 */
export const hasSubject = (store, subject) =>
  store.countQuads(subject, null, null, null) > 0;

/**
 * Get all subjects in the store
 * @param {import('n3').Store} store - RDF store to query
 * @returns {string[]} Array of unique subject IRIs
 */
export const getAllSubjects = (store) => {
  const subjects = new Set();
  for (const q of store) {
    subjects.add(q.subject.value);
  }
  return [...subjects];
};

/**
 * Get all predicates in the store
 * @param {import('n3').Store} store - RDF store to query
 * @returns {string[]} Array of unique predicate IRIs
 */
export const getAllPredicates = (store) => {
  const predicates = new Set();
  for (const q of store) {
    predicates.add(q.predicate.value);
  }
  return [...predicates];
};

/**
 * Get all objects in the store
 * @param {import('n3').Store} store - RDF store to query
 * @returns {string[]} Array of unique object values
 */
export const getAllObjects = (store) => {
  const objects = new Set();
  for (const q of store) {
    objects.add(q.object.value);
  }
  return [...objects];
};

/**
 * Find subjects that have a specific property value
 * @param {import('n3').Store} store - RDF store to query
 * @param {string|import('n3').NamedNode} predicate - Predicate IRI or NamedNode
 * @param {string} value - Object value to search for
 * @returns {string[]} Array of subject IRIs
 */
export const findByProperty = (store, predicate, value) =>
  getSubjects(store, predicate, asLiteral(value))
    .map(term => getIRI(term));

/**
 * Get the first object value for a subject+predicate
 * @param {import('n3').Store} store - RDF store to query
 * @param {string|import('n3').NamedNode} subject - Subject IRI or NamedNode
 * @param {string|import('n3').NamedNode} predicate - Predicate IRI or NamedNode
 * @returns {string|null} First object value or null
 */
export const getFirstObject = (store, subject, predicate) => {
  const objects = getObjects(store, subject, predicate);
  return objects.length > 0 ? objects[0].value : null;
};

/**
 * Count quads for a subject
 * @param {import('n3').Store} store - RDF store to query
 * @param {string|import('n3').NamedNode} subject - Subject IRI or NamedNode
 * @returns {number} Number of quads for the subject
 */
export const countQuadsForSubject = (store, subject) =>
  store.countQuads(subject, null, null, null);

/**
 * Get all quads for a subject
 * @param {import('n3').Store} store - RDF store to query
 * @param {string|import('n3').NamedNode} subject - Subject IRI or NamedNode
 * @returns {import('n3').Quad[]} Array of quads for the subject
 */
export const getQuadsForSubject = (store, subject) =>
  store.getQuads(subject, null, null, null);