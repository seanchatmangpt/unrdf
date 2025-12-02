/**
 * @fileoverview Transform utilities - RDF data transformations and conversions
 *
 * These utilities provide comprehensive data transformation capabilities
 * for RDF data, including format conversions, data reshaping, and
 * structural transformations.
 *
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { DataFactory, Store } from 'n3';
import {
  asNamedNode,
  asLiteral,
  asBlankNode as _asBlankNode,
  getIRI as _getIRI,
} from './term-utils.mjs';
import { quadToJSON as _quadToJSON, jsonToQuad as _jsonToQuad } from './quad-utils.mjs';

const { _namedNode, _literal, _blankNode, quad, _defaultGraph } = DataFactory;

/**
 * Transform a store to a different structure
 * @param {import('n3').Store} sourceStore - Source store
 * @param {Function} transformer - Transformation function
 * @returns {import('n3').Store} Transformed store
 */
export const transformStore = (sourceStore, transformer) => {
  const targetStore = new Store();

  for (const sourceQuad of sourceStore) {
    const transformedQuads = transformer(sourceQuad);
    if (Array.isArray(transformedQuads)) {
      for (const transformedQuad of transformedQuads) {
        targetStore.add(transformedQuad);
      }
    } else if (transformedQuads) {
      targetStore.add(transformedQuads);
    }
  }

  return targetStore;
};

/**
 * Convert store to JSON-LD format
 * @param {import('n3').Store} store - RDF store
 * @param {Object} [options] - Conversion options
 * @param {string} [options.baseIRI] - Base IRI for the document
 * @param {Object} [options.context] - JSON-LD context
 * @returns {Object} JSON-LD document
 */
export const storeToJSONLD = (store, options = {}) => {
  const { baseIRI, context = {} } = options;
  const subjects = new Map();

  // Group quads by subject
  for (const q of store) {
    const subjectIRI = q.subject.value;

    if (!subjects.has(subjectIRI)) {
      subjects.set(subjectIRI, {
        '@id': subjectIRI,
        '@type': [],
      });
    }

    const subject = subjects.get(subjectIRI);
    const predicate = q.predicate.value;
    const object = q.object.termType === 'Literal' ? q.object.value : q.object.value;

    // Handle rdf:type
    if (predicate === 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type') {
      subject['@type'].push(object);
    } else {
      if (!subject[predicate]) {
        subject[predicate] = [];
      }
      subject[predicate].push(object);
    }
  }

  // Convert to array format
  const graph = [...subjects.values()];

  // Build JSON-LD document
  const jsonld = {
    '@context': context,
    '@graph': graph,
  };

  if (baseIRI) {
    jsonld['@base'] = baseIRI;
  }

  return jsonld;
};

/**
 * Convert JSON-LD to store
 * @param {Object} jsonld - JSON-LD document
 * @returns {import('n3').Store} RDF store
 */
export const jsonLDToStore = jsonld => {
  const store = new Store();
  const graph = jsonld['@graph'] || (Array.isArray(jsonld) ? jsonld : [jsonld]);

  for (const node of graph) {
    const subject = asNamedNode(node['@id']);

    // Add type statements
    if (node['@type']) {
      const types = Array.isArray(node['@type']) ? node['@type'] : [node['@type']];
      for (const type of types) {
        store.add(
          quad(
            subject,
            asNamedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
            asNamedNode(type)
          )
        );
      }
    }

    // Add property statements
    for (const [predicate, objects] of Object.entries(node)) {
      if (predicate.startsWith('@')) continue; // Skip JSON-LD keywords

      const predicateNode = asNamedNode(predicate);
      const objectList = Array.isArray(objects) ? objects : [objects];

      for (const object of objectList) {
        const objectNode =
          typeof object === 'string' && (object.startsWith('http') || object.startsWith('urn:'))
            ? asNamedNode(object)
            : asLiteral(object);

        store.add(quad(subject, predicateNode, objectNode));
      }
    }
  }

  return store;
};

/**
 * Convert store to RDF/XML
 * @param {import('n3').Store} store - RDF store
 * @param {Object} [options] - Conversion options
 * @returns {string} RDF/XML string
 */
export const storeToRDFXML = (store, _options = {}) => {
  // This is a simplified RDF/XML conversion
  // In a real implementation, you'd use a proper RDF/XML serializer
  let xml = '<?xml version="1.0" encoding="UTF-8"?>\n';
  xml += '<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">\n';

  const subjects = new Map();

  // Group by subject
  for (const q of store) {
    const subjectIRI = q.subject.value;
    if (!subjects.has(subjectIRI)) {
      subjects.set(subjectIRI, []);
    }
    subjects.get(subjectIRI).push(q);
  }

  // Generate XML for each subject
  for (const [subjectIRI, quads] of subjects) {
    xml += `  <rdf:Description rdf:about="${subjectIRI}">\n`;

    for (const q of quads) {
      const predicate = q.predicate.value;
      const object = q.object;

      xml +=
        object.termType === 'Literal'
          ? `    <${predicate}>${object.value}</${predicate}>\n`
          : `    <${predicate} rdf:resource="${object.value}"/>\n`;
    }

    xml += '  </rdf:Description>\n';
  }

  xml += '</rdf:RDF>';
  return xml;
};

/**
 * Convert store to N-Triples
 * @param {import('n3').Store} store - RDF store
 * @returns {string} N-Triples string
 */
export const storeToNTriples = store => {
  const lines = [];

  for (const q of store) {
    const subject = formatNTriplesTerm(q.subject);
    const predicate = formatNTriplesTerm(q.predicate);
    const object = formatNTriplesTerm(q.object);
    const graph = q.graph.termType === 'DefaultGraph' ? '' : formatNTriplesTerm(q.graph);

    const line = `${subject} ${predicate} ${object}${graph ? ` ${graph}` : ''} .`;
    lines.push(line);
  }

  return lines.join('\n');
};

/**
 * Format a term for N-Triples
 * @param {import('n3').Term} term - RDF term
 * @returns {string} Formatted term
 */
const formatNTriplesTerm = term => {
  switch (term.termType) {
    case 'NamedNode': {
      return `<${term.value}>`;
    }
    case 'Literal': {
      const value = term.value.replace(/\\/g, '\\\\').replace(/"/g, String.raw`\"`);
      return `"${value}"`;
    }
    case 'BlankNode': {
      return `_:${term.value}`;
    }
    // No default
  }
  return term.value;
};

/**
 * Convert store to CSV
 * @param {import('n3').Store} store - RDF store
 * @param {Object} [options] - Conversion options
 * @param {string[]} [options.columns] - Column names
 * @returns {string} CSV string
 */
export const storeToCSV = (store, options = {}) => {
  const { columns = ['subject', 'predicate', 'object', 'graph'] } = options;
  const rows = [];

  // Add header
  rows.push(columns.join(','));

  // Add data rows
  for (const q of store) {
    const row = [
      q.subject.value,
      q.predicate.value,
      q.object.value,
      q.graph.termType === 'DefaultGraph' ? '' : q.graph.value,
    ];
    rows.push(row.join(','));
  }

  return rows.join('\n');
};

/**
 * Convert CSV to store
 * @param {string} csv - CSV string
 * @param {Object} [options] - Conversion options
 * @returns {import('n3').Store} RDF store
 */
export const csvToStore = (csv, _options = {}) => {
  const store = new Store();
  const lines = csv.split('\n').filter(line => line.trim());

  if (lines.length < 2) {
    return store; // Need at least header and one data row
  }

  const header = lines[0].split(',');
  const subjectIndex = header.indexOf('subject');
  const predicateIndex = header.indexOf('predicate');
  const objectIndex = header.indexOf('object');
  const graphIndex = header.indexOf('graph');

  for (let i = 1; i < lines.length; i++) {
    const values = lines[i].split(',');

    if (values.length >= 3) {
      const subject = asNamedNode(values[subjectIndex]);
      const predicate = asNamedNode(values[predicateIndex]);
      const object =
        values[objectIndex].startsWith('http') || values[objectIndex].startsWith('urn:')
          ? asNamedNode(values[objectIndex])
          : asLiteral(values[objectIndex]);
      const graph =
        graphIndex !== -1 && values[graphIndex] ? asNamedNode(values[graphIndex]) : undefined;

      store.add(quad(subject, predicate, object, graph));
    }
  }

  return store;
};

/**
 * Flatten nested RDF structures
 * @param {import('n3').Store} store - RDF store
 * @param {Object} [options] - Flattening options
 * @returns {import('n3').Store} Flattened store
 */
export const flattenStore = (store, _options = {}) => {
  const flattenedStore = new Store();
  const visited = new Set();

  const flatten = (subject, depth = 0) => {
    if (depth > 10 || visited.has(subject.value)) return; // Prevent infinite recursion
    visited.add(subject.value);

    const quads = store.getQuads(subject, null, null, null);
    for (const q of quads) {
      flattenedStore.add(q);

      // Recursively flatten object if it's a named node
      if (q.object.termType === 'NamedNode') {
        flatten(q.object, depth + 1);
      }
    }
  };

  // Start flattening from all subjects
  const subjects = new Set();
  for (const q of store) {
    subjects.add(q.subject);
  }

  for (const subject of subjects) {
    flatten(subject);
  }

  return flattenedStore;
};

/**
 * Denormalize RDF data (convert to denormalized format)
 * @param {import('n3').Store} store - RDF store
 * @param {Object} [options] - Denormalization options
 * @returns {Object[]} Array of denormalized objects
 */
export const denormalizeStore = (store, _options = {}) => {
  const subjects = new Map();

  // Group by subject
  for (const q of store) {
    const subjectIRI = q.subject.value;

    if (!subjects.has(subjectIRI)) {
      subjects.set(subjectIRI, {
        '@id': subjectIRI,
        '@type': [],
        properties: {},
      });
    }

    const subject = subjects.get(subjectIRI);
    const predicate = q.predicate.value;
    const object = q.object.value;

    // Handle rdf:type
    if (predicate === 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type') {
      subject['@type'].push(object);
    } else {
      if (!subject.properties[predicate]) {
        subject.properties[predicate] = [];
      }
      subject.properties[predicate].push(object);
    }
  }

  // Convert to denormalized format
  const denormalized = [];
  for (const subject of subjects.values()) {
    const obj = {
      '@id': subject['@id'],
      '@type': subject['@type'],
    };

    // Flatten properties
    for (const [predicate, values] of Object.entries(subject.properties)) {
      obj[predicate] = values.length === 1 ? values[0] : values;
    }

    denormalized.push(obj);
  }

  return denormalized;
};

/**
 * Normalize RDF data (convert from denormalized format)
 * @param {Object[]} data - Array of denormalized objects
 * @returns {import('n3').Store} RDF store
 */
export const normalizeData = data => {
  const store = new Store();

  for (const obj of data) {
    const subject = asNamedNode(obj['@id']);

    // Add type statements
    if (obj['@type']) {
      const types = Array.isArray(obj['@type']) ? obj['@type'] : [obj['@type']];
      for (const type of types) {
        store.add(
          quad(
            subject,
            asNamedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
            asNamedNode(type)
          )
        );
      }
    }

    // Add property statements
    for (const [predicate, value] of Object.entries(obj)) {
      if (predicate.startsWith('@')) continue; // Skip JSON-LD keywords

      const predicateNode = asNamedNode(predicate);
      const values = Array.isArray(value) ? value : [value];

      for (const val of values) {
        const objectNode =
          typeof val === 'string' && (val.startsWith('http') || val.startsWith('urn:'))
            ? asNamedNode(val)
            : asLiteral(val);

        store.add(quad(subject, predicateNode, objectNode));
      }
    }
  }

  return store;
};

/**
 * Transform RDF data using a mapping
 * @param {import('n3').Store} sourceStore - Source store
 * @param {Object} mapping - Transformation mapping
 * @returns {import('n3').Store} Transformed store
 */
export const transformWithMapping = (sourceStore, mapping) => {
  const targetStore = new Store();

  for (const q of sourceStore) {
    const sourcePredicate = q.predicate.value;

    if (mapping[sourcePredicate]) {
      const targetPredicate = mapping[sourcePredicate];
      const transformedQuad = quad(q.subject, asNamedNode(targetPredicate), q.object, q.graph);
      targetStore.add(transformedQuad);
    } else {
      // Keep original quad if no mapping found
      targetStore.add(q);
    }
  }

  return targetStore;
};

/**
 * Convert between different RDF serialization formats
 * @param {string} input - Input data
 * @param {string} inputFormat - Input format (turtle, ntriples, rdfxml, jsonld)
 * @param {string} outputFormat - Output format (turtle, ntriples, rdfxml, jsonld)
 * @param {Object} [options] - Conversion options
 * @returns {Promise<string>} Converted data
 */
export const convertFormat = async (input, inputFormat, outputFormat, options = {}) => {
  // This would typically use an RDF engine for parsing and serialization
  // For now, we'll provide a basic implementation

  if (inputFormat === outputFormat) {
    return input;
  }

  // Parse input to store
  let store;
  if (inputFormat === 'jsonld') {
    const jsonld = JSON.parse(input);
    store = jsonLDToStore(jsonld);
  } else {
    // For other formats, you'd use an RDF parser
    throw new Error(`Input format ${inputFormat} not yet supported`);
  }

  // Serialize store to output format
  switch (outputFormat) {
    case 'jsonld': {
      return JSON.stringify(storeToJSONLD(store, options), null, 2);
    }
    case 'ntriples': {
      return storeToNTriples(store);
    }
    case 'rdfxml': {
      return storeToRDFXML(store, options);
    }
    case 'csv': {
      return storeToCSV(store, options);
    }
    default: {
      throw new Error(`Output format ${outputFormat} not yet supported`);
    }
  }
};
