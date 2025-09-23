import { DataFactory } from "n3";
import { createHash, randomBytes } from "node:crypto";

const { blankNode, namedNode } = DataFactory;

/**
 * Deterministic blank node generator
 * @param {string} [prefix="bn"] - Prefix for blank node IDs
 * @returns {Function} Function that generates blank nodes
 */
export const makeBNodeGenerator = (prefix = "bn") => {
  let i = 0;
  return () => blankNode(`${prefix}${i++}`);
};

/**
 * Create a skolemized IRI for a blank node
 * @param {string} id - Blank node identifier
 * @param {string} [baseIRI="http://example.org/.well-known/genid/"] - Base IRI for skolemization
 * @returns {string} Skolemized IRI
 */
export const skolemize = (id, baseIRI = "http://example.org/.well-known/genid/") =>
  `${baseIRI}${id}`;

/**
 * Generate a random blank node ID
 * @param {number} [length=16] - Length of random ID
 * @returns {string} Random blank node ID
 */
export const generateRandomBNodeId = (length = 16) => {
  const bytes = randomBytes(Math.ceil(length / 2));
  return bytes.toString("hex").slice(0, Math.max(0, length));
};

/**
 * Generate a deterministic blank node ID from content
 * @param {string} content - Content to hash
 * @param {number} [length=16] - Length of hash to use
 * @returns {string} Deterministic blank node ID
 */
export const generateDeterministicBNodeId = (content, length = 16) => {
  const hash = createHash("sha256");
  hash.update(content);
  return hash.digest("hex").slice(0, Math.max(0, length));
};

/**
 * Create a blank node with random ID
 * @param {number} [length=16] - Length of random ID
 * @returns {import('n3').BlankNode} Blank node with random ID
 */
export const createRandomBlankNode = (length = 16) => {
  return blankNode(generateRandomBNodeId(length));
};

/**
 * Create a blank node with deterministic ID
 * @param {string} content - Content to hash for ID
 * @param {number} [length=16] - Length of hash to use
 * @returns {import('n3').BlankNode} Blank node with deterministic ID
 */
export const createDeterministicBlankNode = (content, length = 16) => {
  return blankNode(generateDeterministicBNodeId(content, length));
};

/**
 * Generate a UUID v4
 * @returns {string} UUID v4 string
 */
export const generateUUID = () => {
  return "xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx".replace(/[xy]/g, (c) => {
    const r = (Math.random() * 16) | 0;
    const v = c === "x" ? r : (r & 0x3) | 0x8;
    return v.toString(16);
  });
};

/**
 * Generate a generic ID with optional prefix
 * @param {string} [prefix="id"] - Prefix for the ID
 * @returns {string} Generated ID
 */
export const generateId = (prefix = "id") => {
  const uuid = generateUUID();
  return `${prefix}-${uuid}`;
};

/**
 * Generate a hash-based ID from input
 * @param {string} input - Input string to hash
 * @returns {string} Hash-based ID
 */
export const generateHashId = (input) => {
  const hash = createHash("sha256");
  hash.update(input);
  return hash.digest("hex");
};

/**
 * Generate a short UUID (base36)
 * @returns {string} Short UUID string
 */
export const generateShortUUID = () => {
  return Math.random().toString(36).slice(2) + Date.now().toString(36);
};

/**
 * Create a named node with UUID
 * @param {string} [baseIRI] - Base IRI for the named node
 * @returns {import('n3').NamedNode} Named node with UUID
 */
export const createUUIDNamedNode = (baseIRI = "http://example.org/id/") => {
  return namedNode(`${baseIRI}${generateUUID()}`);
};

/**
 * Create a named node with short UUID
 * @param {string} [baseIRI] - Base IRI for the named node
 * @returns {import('n3').NamedNode} Named node with short UUID
 */
export const createShortUUIDNamedNode = (baseIRI = "http://example.org/id/") => {
  return namedNode(`${baseIRI}${generateShortUUID()}`);
};

/**
 * Generate a timestamp-based ID
 * @param {string} [prefix="ts"] - Prefix for the ID
 * @returns {string} Timestamp-based ID
 */
export const generateTimestampId = (prefix = "ts") => {
  return `${prefix}${Date.now()}`;
};

/**
 * Generate a counter-based ID
 * @param {string} [prefix="id"] - Prefix for the ID
 * @returns {Function} Function that generates sequential IDs
 */
export const makeCounterIdGenerator = (prefix = "id") => {
  let counter = 0;
  return () => `${prefix}${counter++}`;
};

/**
 * Create a hash-based IRI
 * @param {string} content - Content to hash
 * @param {string} [baseIRI="http://example.org/hash/"] - Base IRI
 * @param {string} [algorithm="sha256"] - Hash algorithm
 * @returns {string} Hash-based IRI
 */
export const createHashIRI = (content, baseIRI = "http://example.org/hash/", algorithm = "sha256") => {
  const hash = createHash(algorithm);
  hash.update(content);
  const hashValue = hash.digest("hex");
  return `${baseIRI}${algorithm}/${hashValue}`;
};

/**
 * Create a named node with hash-based IRI
 * @param {string} content - Content to hash
 * @param {string} [baseIRI="http://example.org/hash/"] - Base IRI
 * @param {string} [algorithm="sha256"] - Hash algorithm
 * @returns {import('n3').NamedNode} Named node with hash-based IRI
 */
export const createHashNamedNode = (content, baseIRI = "http://example.org/hash/", algorithm = "sha256") => {
  return namedNode(createHashIRI(content, baseIRI, algorithm));
};

/**
 * Generate a namespace-aware ID
 * @param {string} namespace - Namespace IRI
 * @param {string} localName - Local name
 * @returns {string} Full IRI
 */
export const createNamespaceId = (namespace, localName) => {
  const cleanNamespace = namespace.endsWith("/") || namespace.endsWith("#") ? namespace : `${namespace}#`;
  return `${cleanNamespace}${localName}`;
};

/**
 * Create a named node with namespace
 * @param {string} namespace - Namespace IRI
 * @param {string} localName - Local name
 * @returns {import('n3').NamedNode} Named node with namespace
 */
export const createNamespaceNamedNode = (namespace, localName) => {
  return namedNode(createNamespaceId(namespace, localName));
};

/**
 * Extract local name from IRI
 * @param {string} iri - IRI to extract from
 * @returns {string} Local name
 */
export const extractLocalName = (iri) => {
  const hashIndex = iri.lastIndexOf("#");
  const slashIndex = iri.lastIndexOf("/");
  const index = Math.max(hashIndex, slashIndex);
  return index >= 0 ? iri.slice(Math.max(0, index + 1)) : iri;
};

/**
 * Extract namespace from IRI
 * @param {string} iri - IRI to extract from
 * @returns {string} Namespace
 */
export const extractNamespace = (iri) => {
  const hashIndex = iri.lastIndexOf("#");
  const slashIndex = iri.lastIndexOf("/");
  const index = Math.max(hashIndex, slashIndex);
  return index >= 0 ? iri.slice(0, Math.max(0, index + 1)) : "";
};

/**
 * Check if an IRI is a blank node identifier
 * @param {string} iri - IRI to check
 * @returns {boolean} True if it's a blank node identifier
 */
export const isBlankNodeIRI = (iri) => {
  return iri.startsWith("_:") || iri.includes(".well-known/genid/");
};

/**
 * Convert blank node IRI to blank node ID
 * @param {string} iri - Blank node IRI
 * @returns {string} Blank node ID
 */
export const iriToBlankNodeId = (iri) => {
  if (iri.startsWith("_:")) {
    return iri.slice(2);
  }
  if (iri.includes(".well-known/genid/")) {
    return iri.split(".well-known/genid/")[1];
  }
  return iri;
};

/**
 * Convert blank node ID to blank node IRI
 * @param {string} id - Blank node ID
 * @param {string} [baseIRI="http://example.org/.well-known/genid/"] - Base IRI
 * @returns {string} Blank node IRI
 */
export const blankNodeIdToIRI = (id, baseIRI = "http://example.org/.well-known/genid/") => {
  return `${baseIRI}${id}`;
};

/**
 * Generate a stable ID from multiple values
 * @param {...any} values - Values to combine for ID generation
 * @returns {string} Stable ID
 */
export const generateStableId = (...values) => {
  const content = values.map(String).join("|");
  return generateDeterministicBNodeId(content);
};

/**
 * Create a stable named node from multiple values
 * @param {string} baseIRI - Base IRI
 * @param {...any} values - Values to combine for ID generation
 * @returns {import('n3').NamedNode} Stable named node
 */
export const createStableNamedNode = (baseIRI, ...values) => {
  const stableId = generateStableId(...values);
  return namedNode(`${baseIRI}${stableId}`);
};
