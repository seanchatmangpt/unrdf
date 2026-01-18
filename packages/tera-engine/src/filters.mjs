/**
 * @file Custom RDF Filters for Tera Engine
 * @module @unrdf/tera-engine/filters
 * @description RDF-specific template filters for knowledge graph operations
 */

/**
 * Standard filters (Tera-compatible)
 */

/**
 * Converts string to uppercase
 * @param {string} value - Input value
 * @returns {string} Uppercase string
 */
export function upper(value) {
  return String(value).toUpperCase();
}

/**
 * Converts string to lowercase
 * @param {string} value - Input value
 * @returns {string} Lowercase string
 */
export function lower(value) {
  return String(value).toLowerCase();
}

/**
 * Capitalizes first letter
 * @param {string} value - Input value
 * @returns {string} Capitalized string
 */
export function capitalize(value) {
  const str = String(value);
  return str.charAt(0).toUpperCase() + str.slice(1).toLowerCase();
}

/**
 * Truncates string to length
 * @param {string} value - Input value
 * @param {number} length - Max length
 * @returns {string} Truncated string
 */
export function truncate(value, length = 255) {
  const str = String(value);
  return str.length > length ? str.slice(0, length) + '...' : str;
}

/**
 * Joins array with separator
 * @param {Array} value - Input array
 * @param {string} separator - Join separator
 * @returns {string} Joined string
 */
export function join(value, separator = ', ') {
  if (!Array.isArray(value)) return String(value);
  return value.join(separator);
}

/**
 * Returns length of array or string
 * @param {Array|string} value - Input value
 * @returns {number} Length
 */
export function length(value) {
  if (Array.isArray(value) || typeof value === 'string') {
    return value.length;
  }
  if (typeof value === 'object' && value !== null) {
    return Object.keys(value).length;
  }
  return 0;
}

/**
 * Returns first element of array
 * @param {Array} value - Input array
 * @returns {any} First element
 */
export function first(value) {
  if (!Array.isArray(value)) return value;
  return value[0];
}

/**
 * Returns last element of array
 * @param {Array} value - Input array
 * @returns {any} Last element
 */
export function last(value) {
  if (!Array.isArray(value)) return value;
  return value[value.length - 1];
}

/**
 * Reverses array or string
 * @param {Array|string} value - Input value
 * @returns {Array|string} Reversed value
 */
export function reverse(value) {
  if (Array.isArray(value)) return [...value].reverse();
  if (typeof value === 'string') return value.split('').reverse().join('');
  return value;
}

/**
 * Replaces substring
 * @param {string} value - Input string
 * @param {string} from - String to replace
 * @param {string} to - Replacement string
 * @returns {string} Result
 */
export function replace(value, from, to = '') {
  // Escape regex special characters for literal string replacement
  const escaped = from.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
  return String(value).replace(new RegExp(escaped, 'g'), to);
}

/**
 * RDF-specific filters
 */

/**
 * Extracts local name from URI
 * @param {string} uri - RDF URI
 * @returns {string} Local name
 * @example
 * localname('http://example.org/Person') → 'Person'
 */
export function localname(uri) {
  const str = String(uri);
  const hashIndex = str.lastIndexOf('#');
  const slashIndex = str.lastIndexOf('/');
  const splitIndex = Math.max(hashIndex, slashIndex);
  return splitIndex >= 0 ? str.slice(splitIndex + 1) : str;
}

/**
 * Extracts namespace from URI
 * @param {string} uri - RDF URI
 * @returns {string} Namespace
 * @example
 * namespace('http://example.org/Person') → 'http://example.org/'
 */
export function namespace(uri) {
  const str = String(uri);
  const hashIndex = str.lastIndexOf('#');
  const slashIndex = str.lastIndexOf('/');
  const splitIndex = Math.max(hashIndex, slashIndex);
  return splitIndex >= 0 ? str.slice(0, splitIndex + 1) : '';
}

/**
 * Converts URI to prefixed name
 * @param {string} uri - RDF URI
 * @param {Record<string, string>} prefixes - Prefix map
 * @returns {string} Prefixed name
 * @example
 * prefixedName('http://www.w3.org/1999/02/22-rdf-syntax-ns#type', {rdf: '...'}) → 'rdf:type'
 */
export function prefixedName(uri, prefixes = {}) {
  const str = String(uri);
  for (const [prefix, ns] of Object.entries(prefixes)) {
    if (str.startsWith(ns)) {
      return `${prefix}:${str.slice(ns.length)}`;
    }
  }
  return str;
}

/**
 * Formats RDF term as N-Triples
 * @param {any} term - RDF term
 * @returns {string} N-Triples representation
 */
export function ntriples(term) {
  if (!term || typeof term !== 'object') return String(term);

  if (term.termType === 'NamedNode') {
    return `<${term.value}>`;
  }
  if (term.termType === 'Literal') {
    let result = `"${term.value.replace(/"/g, '\\"')}"`;
    if (term.language) {
      result += `@${term.language}`;
      // Don't add datatype if language tag is present
      return result;
    }
    if (term.datatype && term.datatype.value !== 'http://www.w3.org/2001/XMLSchema#string') {
      result += `^^<${term.datatype.value}>`;
    }
    return result;
  }
  if (term.termType === 'BlankNode') {
    return `_:${term.value}`;
  }

  return String(term);
}

/**
 * Formats RDF term as Turtle
 * @param {any} term - RDF term
 * @param {Record<string, string>} prefixes - Prefix map
 * @returns {string} Turtle representation
 */
export function turtle(term, prefixes = {}) {
  if (!term || typeof term !== 'object') return String(term);

  if (term.termType === 'NamedNode') {
    return prefixedName(term.value, prefixes);
  }

  return ntriples(term);
}

/**
 * Extracts subjects from triples
 * @param {Array} triples - Array of RDF triples
 * @returns {Array} Unique subjects
 */
export function subjects(triples) {
  if (!Array.isArray(triples)) return [];
  const subjects = new Set();
  for (const triple of triples) {
    if (triple && triple.subject) {
      subjects.add(triple.subject.value);
    }
  }
  return Array.from(subjects);
}

/**
 * Extracts predicates from triples
 * @param {Array} triples - Array of RDF triples
 * @returns {Array} Unique predicates
 */
export function predicates(triples) {
  if (!Array.isArray(triples)) return [];
  const preds = new Set();
  for (const triple of triples) {
    if (triple && triple.predicate) {
      preds.add(triple.predicate.value);
    }
  }
  return Array.from(preds);
}

/**
 * Extracts objects from triples
 * @param {Array} triples - Array of RDF triples
 * @returns {Array} Unique objects
 */
export function objects(triples) {
  if (!Array.isArray(triples)) return [];
  const objs = new Set();
  for (const triple of triples) {
    if (triple && triple.object) {
      objs.add(triple.object.value);
    }
  }
  return Array.from(objs);
}

/**
 * Filters triples by predicate
 * @param {Array} triples - Array of RDF triples
 * @param {string} predicateUri - Predicate URI to filter
 * @returns {Array} Filtered triples
 */
export function filterByPredicate(triples, predicateUri) {
  if (!Array.isArray(triples)) return [];
  return triples.filter(t => t && t.predicate && t.predicate.value === predicateUri);
}

/**
 * Gets all standard filters
 * @returns {Record<string, Function>} Filter registry
 */
export function getStandardFilters() {
  return {
    upper,
    lower,
    capitalize,
    truncate,
    join,
    length,
    first,
    last,
    reverse,
    replace,
  };
}

/**
 * Gets all RDF filters
 * @returns {Record<string, Function>} RDF filter registry
 */
export function getRdfFilters() {
  return {
    localname,
    namespace,
    prefixedName,
    ntriples,
    turtle,
    subjects,
    predicates,
    objects,
    filterByPredicate,
  };
}

/**
 * Gets all filters (standard + RDF)
 * @returns {Record<string, Function>} Complete filter registry
 */
export function getAllFilters() {
  return {
    ...getStandardFilters(),
    ...getRdfFilters(),
  };
}
