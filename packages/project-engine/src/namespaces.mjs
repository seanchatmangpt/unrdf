/**
 * @file RDF vocabulary used by the project engine
 * @module project-engine/namespaces
 */

export const RDF_TYPE = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type';
export const RDFS_LABEL = 'http://www.w3.org/2000/01/rdf-schema#label';
export const XSD = 'http://www.w3.org/2001/XMLSchema#';
export const NFO = 'http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#';

/** Filesystem ontology */
export const FS = {
  ProjectRoot: 'http://example.org/unrdf/filesystem#ProjectRoot',
  SourceFolder: 'http://example.org/unrdf/filesystem#SourceFolder',
  BuildFolder: 'http://example.org/unrdf/filesystem#BuildFolder',
  ConfigFolder: 'http://example.org/unrdf/filesystem#ConfigFolder',
  relativePath: 'http://example.org/unrdf/filesystem#relativePath',
  depth: 'http://example.org/unrdf/filesystem#depth',
  byteSize: 'http://example.org/unrdf/filesystem#byteSize',
  lastModified: 'http://example.org/unrdf/filesystem#lastModified',
  isHidden: 'http://example.org/unrdf/filesystem#isHidden',
  extension: 'http://example.org/unrdf/filesystem#extension',
  containedIn: 'http://example.org/unrdf/filesystem#containedIn',
};

/** Project ontology */
export const PROJECT = {
  Project: 'http://example.org/unrdf/project#Project',
  Feature: 'http://example.org/unrdf/project#Feature',
  Module: 'http://example.org/unrdf/project#Module',
  hasFeature: 'http://example.org/unrdf/project#hasFeature',
  belongsToFeature: 'http://example.org/unrdf/project#belongsToFeature',
  hasRole: 'http://example.org/unrdf/project#hasRole',
  roleString: 'http://example.org/unrdf/project#roleString',
};

/** Domain ontology */
export const DOMAIN = {
  Entity: 'http://example.org/unrdf/domain#Entity',
  hasField: 'http://example.org/unrdf/domain#hasField',
  fieldType: 'http://example.org/unrdf/domain#fieldType',
};
