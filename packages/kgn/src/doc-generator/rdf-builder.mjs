/**
 * @fileoverview RDF Graph Builder
 * Converts parsed JSDoc data to RDF triples
 */

import crypto from 'crypto';

/**
 * RDF Vocabulary Prefixes
 */
export const PREFIXES = {
  rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
  xsd: 'http://www.w3.org/2001/XMLSchema#',
  code: 'http://unrdf.org/vocab/code#',
  fs: 'http://unrdf.org/vocab/fs#',
  doc: 'http://unrdf.org/vocab/doc#',
  ex: 'http://example.org/',
};

/**
 * Generate a deterministic URI for a resource
 * @param {string} path - Resource path
 * @returns {string} URI
 */
function generateURI(path) {
  // Use package path as namespace
  const namespace = 'http://unrdf.org/packages/';
  // Create deterministic ID from path
  const id = path.replace(/[^a-zA-Z0-9]/g, '-').replace(/-+/g, '-');
  return namespace + id;
}

/**
 * Generate a blank node ID
 * @param {string} name - Name for blank node
 * @returns {string} Blank node ID
 */
function blankNode(name) {
  const hash = crypto.createHash('md5').update(name).digest('hex').substring(0, 8);
  return `_:${name}_${hash}`;
}

/**
 * Escape string for Turtle literal
 * @param {string} str - String to escape
 * @returns {string} Escaped string
 */
function escapeLiteral(str) {
  if (!str) return '""';
  return `"${str
    .replace(/\\/g, '\\\\')
    .replace(/"/g, '\\"')
    .replace(/\n/g, '\\n')
    .replace(/\r/g, '\\r')
    .replace(/\t/g, '\\t')}"`;
}

/**
 * Build RDF graph from parsed file data
 * @param {Object} parsedFile - Output from parser.parseFile()
 * @returns {string} Turtle RDF representation
 */
export function buildRDFGraph(parsedFile) {
  const triples = [];

  // File resource
  const fileURI = generateURI(parsedFile.relativePath);

  triples.push(`# File: ${parsedFile.relativePath}`);
  triples.push(`${fileURI}`);
  triples.push(`  a fs:File , code:Module ;`);
  triples.push(`  fs:path ${escapeLiteral(parsedFile.relativePath)} ;`);
  triples.push(`  fs:absolutePath ${escapeLiteral(parsedFile.file)} ;`);
  triples.push(`  code:commentCount ${parsedFile.comments} .`);
  triples.push('');

  // Exports
  parsedFile.exports.forEach((exp, idx) => {
    const exportURI = `${fileURI}#${exp.name}`;

    triples.push(`# Export: ${exp.name}`);
    triples.push(`${exportURI}`);

    if (exp.type === 'function') {
      triples.push(`  a code:Function ;`);
      triples.push(`  code:name ${escapeLiteral(exp.name)} ;`);

      if (exp.description) {
        triples.push(`  doc:description ${escapeLiteral(exp.description)} ;`);
      }

      if (exp.async) {
        triples.push(`  code:async true ;`);
      }

      // Parameters
      exp.params?.forEach((param, paramIdx) => {
        const paramNode = blankNode(`${exp.name}_param_${param.name}`);
        triples.push(`  code:param ${paramNode} ;`);

        const paramTriples = [];
        paramTriples.push(`${paramNode}`);
        paramTriples.push(`  a code:Parameter ;`);
        paramTriples.push(`  code:name ${escapeLiteral(param.name)} ;`);
        paramTriples.push(`  code:type ${escapeLiteral(param.type)} ;`);

        if (param.optional) {
          paramTriples.push(`  code:optional true ;`);
        }

        if (param.description) {
          paramTriples.push(`  doc:description ${escapeLiteral(param.description)} ;`);
        }

        if (param.default !== undefined) {
          paramTriples.push(`  code:default ${escapeLiteral(String(param.default))} ;`);
        }

        paramTriples.push(`  code:position ${paramIdx} .`);
        triples.push(...paramTriples);
        triples.push('');
      });

      // Returns
      if (exp.returns) {
        triples.push(`  code:returns ${escapeLiteral(exp.returns.type)} ;`);
        if (exp.returns.description) {
          triples.push(`  code:returnsDescription ${escapeLiteral(exp.returns.description)} ;`);
        }
      }

      // Examples
      exp.examples?.forEach((example, exIdx) => {
        triples.push(`  doc:example ${escapeLiteral(example)} ;`);
      });

      // Remove trailing semicolon from last triple
      const lastLine = triples[triples.length - 1];
      triples[triples.length - 1] = lastLine.replace(/\s;$/, ' .');

    } else if (exp.type === 'class') {
      triples.push(`  a code:Class ;`);
      triples.push(`  code:name ${escapeLiteral(exp.name)} ;`);

      if (exp.description) {
        triples.push(`  doc:description ${escapeLiteral(exp.description)} ;`);
      }

      if (exp.extends) {
        triples.push(`  code:extends ${escapeLiteral(exp.extends)} ;`);
      }

      // Methods
      exp.methods?.forEach(method => {
        const methodNode = blankNode(`${exp.name}_${method.name}`);
        triples.push(`  code:method ${methodNode} ;`);

        const methodTriples = [];
        methodTriples.push(`${methodNode}`);
        methodTriples.push(`  a code:Method ;`);
        methodTriples.push(`  code:name ${escapeLiteral(method.name)} ;`);
        methodTriples.push(`  code:kind ${escapeLiteral(method.kind)} ;`);

        if (method.static) {
          methodTriples.push(`  code:static true ;`);
        }

        if (method.async) {
          methodTriples.push(`  code:async true ;`);
        }

        methodTriples.push(`  code:paramCount ${method.params?.length || 0} .`);
        triples.push(...methodTriples);
        triples.push('');
      });

      // Remove trailing semicolon
      const lastLine = triples[triples.length - 1];
      if (lastLine.includes(';')) {
        triples[triples.length - 1] = lastLine.replace(/\s;$/, ' .');
      }

    } else if (exp.type === 'variable') {
      triples.push(`  a code:Variable ;`);
      triples.push(`  code:name ${escapeLiteral(exp.name)} ;`);
      triples.push(`  code:valueType ${escapeLiteral(exp.valueType)} .`);
    }

    triples.push('');
  });

  // Imports (for cross-referencing)
  parsedFile.imports?.forEach((imp, idx) => {
    const importNode = blankNode(`import_${idx}`);

    triples.push(`# Import from: ${imp.source}`);
    triples.push(`${fileURI}`);
    triples.push(`  code:imports ${importNode} .`);
    triples.push('');

    triples.push(`${importNode}`);
    triples.push(`  a code:Import ;`);
    triples.push(`  code:source ${escapeLiteral(imp.source)} ;`);

    imp.specifiers.forEach((spec, specIdx) => {
      const specNode = blankNode(`import_${idx}_spec_${specIdx}`);
      triples.push(`  code:specifier ${specNode} ;`);

      triples.push(`${specNode}`);
      triples.push(`  a code:ImportSpecifier ;`);
      triples.push(`  code:imported ${escapeLiteral(spec.imported)} ;`);
      triples.push(`  code:local ${escapeLiteral(spec.local)} ;`);
      triples.push(`  code:specifierType ${escapeLiteral(spec.type)} .`);
      triples.push('');
    });

    triples.push('');
  });

  // Build complete Turtle document
  const prefixDeclarations = Object.entries(PREFIXES)
    .map(([prefix, uri]) => `@prefix ${prefix}: <${uri}> .`)
    .join('\n');

  return `${prefixDeclarations}\n\n${triples.join('\n')}`;
}

/**
 * Build RDF graph for multiple files
 * @param {Array} parsedFiles - Array of parsed file data
 * @returns {string} Combined Turtle RDF
 */
export function buildRDFGraphs(parsedFiles) {
  const graphs = parsedFiles
    .filter(file => !file.error)
    .map(file => buildRDFGraph(file));

  const prefixDeclarations = Object.entries(PREFIXES)
    .map(([prefix, uri]) => `@prefix ${prefix}: <${uri}> .`)
    .join('\n');

  // Remove duplicate prefix declarations from individual graphs
  const graphsWithoutPrefixes = graphs.map(graph =>
    graph.replace(/@prefix[^\n]+\n/g, '').trim()
  );

  return `${prefixDeclarations}\n\n${graphsWithoutPrefixes.join('\n\n')}`;
}

/**
 * Build JSON-LD representation
 * @param {Object} parsedFile - Parsed file data
 * @returns {Object} JSON-LD document
 */
export function buildJSONLD(parsedFile) {
  const fileURI = generateURI(parsedFile.relativePath);

  const document = {
    '@context': PREFIXES,
    '@id': fileURI,
    '@type': ['fs:File', 'code:Module'],
    'fs:path': parsedFile.relativePath,
    'fs:absolutePath': parsedFile.file,
    'code:commentCount': parsedFile.comments,
    'code:exports': [],
  };

  parsedFile.exports.forEach(exp => {
    const exportObj = {
      '@id': `${fileURI}#${exp.name}`,
      '@type': `code:${exp.type.charAt(0).toUpperCase() + exp.type.slice(1)}`,
      'code:name': exp.name,
    };

    if (exp.description) {
      exportObj['doc:description'] = exp.description;
    }

    if (exp.type === 'function') {
      if (exp.params) {
        exportObj['code:param'] = exp.params.map(param => ({
          '@type': 'code:Parameter',
          'code:name': param.name,
          'code:type': param.type,
          'code:optional': param.optional,
          'doc:description': param.description,
        }));
      }

      if (exp.returns) {
        exportObj['code:returns'] = exp.returns.type;
      }

      if (exp.examples?.length) {
        exportObj['doc:example'] = exp.examples;
      }
    }

    document['code:exports'].push(exportObj);
  });

  return document;
}

export default { buildRDFGraph, buildRDFGraphs, buildJSONLD, PREFIXES };
