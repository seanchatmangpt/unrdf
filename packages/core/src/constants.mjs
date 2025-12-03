/**
 * @file Common RDF namespace constants
 * @module @unrdf/core/constants
 */

import namespace from '@rdfjs/namespace';

/**
 * RDF namespace
 * @constant
 * @type {Object}
 */
export const RDF = namespace('http://www.w3.org/1999/02/22-rdf-syntax-ns#', {
  type: true,
  Property: true,
  Statement: true,
  subject: true,
  predicate: true,
  object: true,
  first: true,
  rest: true,
  nil: true,
  List: true,
});

/**
 * RDFS namespace
 * @constant
 * @type {Object}
 */
export const RDFS = namespace('http://www.w3.org/2000/01/rdf-schema#', {
  Resource: true,
  Class: true,
  subClassOf: true,
  subPropertyOf: true,
  domain: true,
  range: true,
  label: true,
  comment: true,
  Literal: true,
  Datatype: true,
  Container: true,
  member: true,
  isDefinedBy: true,
  seeAlso: true,
});

/**
 * OWL namespace
 * @constant
 * @type {Object}
 */
export const OWL = namespace('http://www.w3.org/2002/07/owl#', {
  Class: true,
  Thing: true,
  Nothing: true,
  ObjectProperty: true,
  DatatypeProperty: true,
  AnnotationProperty: true,
  Ontology: true,
  imports: true,
  versionInfo: true,
  equivalentClass: true,
  equivalentProperty: true,
  sameAs: true,
  differentFrom: true,
  inverseOf: true,
  TransitiveProperty: true,
  SymmetricProperty: true,
  FunctionalProperty: true,
  InverseFunctionalProperty: true,
});

/**
 * XSD namespace
 * @constant
 * @type {Object}
 */
export const XSD = namespace('http://www.w3.org/2001/XMLSchema#', {
  string: true,
  boolean: true,
  decimal: true,
  integer: true,
  double: true,
  float: true,
  date: true,
  time: true,
  dateTime: true,
  dateTimeStamp: true,
  gYear: true,
  gMonth: true,
  gDay: true,
  gYearMonth: true,
  gMonthDay: true,
  duration: true,
  yearMonthDuration: true,
  dayTimeDuration: true,
  byte: true,
  short: true,
  int: true,
  long: true,
  unsignedByte: true,
  unsignedShort: true,
  unsignedInt: true,
  unsignedLong: true,
  positiveInteger: true,
  nonNegativeInteger: true,
  negativeInteger: true,
  nonPositiveInteger: true,
  hexBinary: true,
  base64Binary: true,
  anyURI: true,
  language: true,
  normalizedString: true,
  token: true,
  NMTOKEN: true,
  Name: true,
  NCName: true,
});

/**
 * FOAF namespace
 * @constant
 * @type {Object}
 */
export const FOAF = namespace('http://xmlns.com/foaf/0.1/', {
  Agent: true,
  Person: true,
  Organization: true,
  Group: true,
  Document: true,
  Image: true,
  name: true,
  title: true,
  firstName: true,
  lastName: true,
  givenName: true,
  familyName: true,
  nick: true,
  mbox: true,
  homepage: true,
  weblog: true,
  openid: true,
  img: true,
  depiction: true,
  depicts: true,
  knows: true,
  member: true,
  age: true,
  birthday: true,
  gender: true,
  account: true,
  OnlineAccount: true,
  accountName: true,
  accountServiceHomepage: true,
});

/**
 * DCTERMS namespace
 * @constant
 * @type {Object}
 */
export const DCTERMS = namespace('http://purl.org/dc/terms/', {
  title: true,
  creator: true,
  subject: true,
  description: true,
  publisher: true,
  contributor: true,
  date: true,
  type: true,
  format: true,
  identifier: true,
  source: true,
  language: true,
  relation: true,
  coverage: true,
  rights: true,
  created: true,
  modified: true,
  issued: true,
  available: true,
  valid: true,
  license: true,
});

/**
 * SKOS namespace
 * @constant
 * @type {Object}
 */
export const SKOS = namespace('http://www.w3.org/2004/02/skos/core#', {
  Concept: true,
  ConceptScheme: true,
  Collection: true,
  prefLabel: true,
  altLabel: true,
  hiddenLabel: true,
  notation: true,
  note: true,
  changeNote: true,
  definition: true,
  editorialNote: true,
  example: true,
  historyNote: true,
  scopeNote: true,
  broader: true,
  narrower: true,
  related: true,
  broaderTransitive: true,
  narrowerTransitive: true,
  semanticRelation: true,
  inScheme: true,
  hasTopConcept: true,
  topConceptOf: true,
  member: true,
});

/**
 * Common prefixes for SPARQL queries
 * @constant
 * @type {Object}
 */
export const COMMON_PREFIXES = {
  rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
  owl: 'http://www.w3.org/2002/07/owl#',
  xsd: 'http://www.w3.org/2001/XMLSchema#',
  foaf: 'http://xmlns.com/foaf/0.1/',
  dcterms: 'http://purl.org/dc/terms/',
  skos: 'http://www.w3.org/2004/02/skos/core#',
};
