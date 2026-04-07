/**
 * @file Standard RDF vocabulary constants and common test entities.
 *
 * Eliminates repeated `namedNode('http://www.w3.org/...')` calls across 50+ test files.
 *
 * @module @unrdf/test-utils/vocab
 */

import { dataFactory } from '@unrdf/oxigraph';
const { namedNode, literal, quad, defaultGraph } = dataFactory;

// ============================================================================
// Standard vocabulary IRIs
// ============================================================================

export const RDF_NS = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';
export const RDFS_NS = 'http://www.w3.org/2000/01/rdf-schema#';
export const OWL_NS = 'http://www.w3.org/2002/07/owl#';
export const XSD_NS = 'http://www.w3.org/2001/XMLSchema#';
export const FOAF_NS = 'http://xmlns.com/foaf/0.1/';
export const SCHEMA_NS = 'http://schema.org/';
export const DCTERMS_NS = 'http://purl.org/dc/terms/';

/** Standard vocabulary IRI strings */
export const VOCAB = {
  rdf: {
    type: `${RDF_NS}type`,
    Property: `${RDF_NS}Property`,
    List: `${RDF_NS}List`,
    nil: `${RDF_NS}nil`,
    first: `${RDF_NS}first`,
    rest: `${RDF_NS}rest`,
  },
  rdfs: {
    label: `${RDFS_NS}label`,
    comment: `${RDFS_NS}comment`,
    domain: `${RDFS_NS}domain`,
    range: `${RDFS_NS}range`,
    subClassOf: `${RDFS_NS}subClassOf`,
    subPropertyOf: `${RDFS_NS}subPropertyOf`,
    Class: `${RDFS_NS}Class`,
  },
  owl: {
    Class: `${OWL_NS}Class`,
    ObjectProperty: `${OWL_NS}ObjectProperty`,
    DatatypeProperty: `${OWL_NS}DatatypeProperty`,
    sameAs: `${OWL_NS}sameAs`,
    equivalentClass: `${OWL_NS}equivalentClass`,
  },
  xsd: {
    string: `${XSD_NS}string`,
    integer: `${XSD_NS}integer`,
    decimal: `${XSD_NS}decimal`,
    boolean: `${XSD_NS}boolean`,
    dateTime: `${XSD_NS}dateTime`,
    date: `${XSD_NS}date`,
  },
  foaf: {
    Person: `${FOAF_NS}Person`,
    name: `${FOAF_NS}name`,
    knows: `${FOAF_NS}knows`,
    mbox: `${FOAF_NS}mbox`,
    age: `${FOAF_NS}age`,
  },
  schema: {
    Person: `${SCHEMA_NS}Person`,
    name: `${SCHEMA_NS}name`,
    email: `${SCHEMA_NS}email`,
    knows: `${SCHEMA_NS}knows`,
    birthDate: `${SCHEMA_NS}birthDate`,
  },
  dcterms: {
    title: `${DCTERMS_NS}title`,
    description: `${DCTERMS_NS}description`,
    created: `${DCTERMS_NS}created`,
    creator: `${DCTERMS_NS}creator`,
  },
};

// ============================================================================
// Pre-built named node terms
// ============================================================================

/** Common RDF predicates as pre-built named nodes */
export const terms = {
  rdfType: namedNode(VOCAB.rdf.type),
  rdfsLabel: namedNode(VOCAB.rdfs.label),
  rdfsComment: namedNode(VOCAB.rdfs.comment),
  rdfsSubClassOf: namedNode(VOCAB.rdfs.subClassOf),
  owlClass: namedNode(VOCAB.owl.Class),
  foafPerson: namedNode(VOCAB.foaf.Person),
  foafName: namedNode(VOCAB.foaf.name),
  foafKnows: namedNode(VOCAB.foaf.knows),
  schemaPerson: namedNode(VOCAB.schema.Person),
  schemaName: namedNode(VOCAB.schema.name),
};

// ============================================================================
// Common test entities
// ============================================================================

const EX = 'http://example.org/';

/** Common test entity named nodes */
export const entities = {
  alice: namedNode(`${EX}alice`),
  bob: namedNode(`${EX}bob`),
  charlie: namedNode(`${EX}charlie`),
  person: namedNode(`${EX}Person`),
  knows: namedNode(`${EX}knows`),
  name: namedNode(`${EX}name`),
  age: namedNode(`${EX}age`),
  aliceName: literal('Alice'),
  bobName: literal('Bob'),
};

/**
 * Build a 7-triple social graph for Alice, Bob, Charlie.
 * Useful for tests that need a non-trivial populated store.
 *
 * @returns {object[]} Array of RDF quads
 *
 * @example
 * const store = createStoreWith(socialGraphQuads());
 * // store has 7 quads: alice/bob/charlie types, names, knows relationships
 */
export function socialGraphQuads() {
  const dg = defaultGraph();
  return [
    quad(entities.alice, terms.rdfType, entities.person, dg),
    quad(entities.bob, terms.rdfType, entities.person, dg),
    quad(entities.charlie, terms.rdfType, entities.person, dg),
    quad(entities.alice, entities.name, entities.aliceName, dg),
    quad(entities.bob, entities.name, entities.bobName, dg),
    quad(entities.alice, entities.knows, entities.bob, dg),
    quad(entities.bob, entities.knows, entities.charlie, dg),
  ];
}
