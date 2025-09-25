/**
 * @fileoverview Definitive Provenance System
 * 
 * Comprehensive provenance tracking for every write operation.
 * Integrates with event system for real-time monitoring and validation.
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { DataFactory } from "n3";

const { namedNode, literal } = DataFactory;

/**
 * Write quads to store with comprehensive provenance tracking
 * @param {RdfEngine} engine - RDF engine instance
 * @param {import('n3').Quad[]} quads - Quads to write
 * @param {string} source - Source identifier (e.g., 'ingress', 'reasoning', 'update')
 * @param {Object} [metadata] - Additional metadata
 * @param {string} [metadata.operation] - Operation type
 * @param {string} [metadata.user] - User identifier
 * @param {string} [metadata.session] - Session identifier
 * @param {string} [metadata.batch] - Batch identifier
 * @param {Object} [metadata.custom] - Custom metadata
 */
export async function writeWithProv(engine, quads, source = 'ingress', metadata = {}) {
  const now = new Date().toISOString();
  const operationId = `op_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
  
  // Emit before write event
  await engine.eventBus.emit('beforeWrite', {
    event: 'beforeWrite',
    quad: quads,
    context: engine.eventBus.createContext('provenance', {
      source,
      operationId,
      quadCount: quads.length,
      ...metadata
    }),
    store: engine.store,
    engine
  });

  // Add all quads to store
  for (const q of quads) {
    await engine.store.addQuad(q.subject, q.predicate, q.object, q.graph);
  }
  
  // Add comprehensive provenance triples per subject
  const seen = new Set(quads.map(q => q.subject.value));
  
  for (const s of seen) {
    // Source provenance
    await engine.store.addQuad(
      namedNode(s),
      namedNode('http://ex/prov#source'),
      literal(source)
    );
    
    // Timestamp provenance
    await engine.store.addQuad(
      namedNode(s),
      namedNode('http://ex/prov#ts'),
      literal(now)
    );

    // Operation ID provenance
    await engine.store.addQuad(
      namedNode(s),
      namedNode('http://ex/prov#operationId'),
      literal(operationId)
    );
    
    // Additional metadata if provided
    if (metadata.operation) {
      await engine.store.addQuad(
        namedNode(s),
        namedNode('http://ex/prov#operation'),
        literal(metadata.operation)
      );
    }
    
    if (metadata.user) {
      await engine.store.addQuad(
        namedNode(s),
        namedNode('http://ex/prov#user'),
        literal(metadata.user)
      );
    }
    
    if (metadata.session) {
      await engine.store.addQuad(
        namedNode(s),
        namedNode('http://ex/prov#session'),
        literal(metadata.session)
      );
    }

    if (metadata.batch) {
      await engine.store.addQuad(
        namedNode(s),
        namedNode('http://ex/prov#batch'),
        literal(metadata.batch)
      );
    }

    // Custom metadata
    if (metadata.custom && typeof metadata.custom === 'object') {
      for (const [key, value] of Object.entries(metadata.custom)) {
        await engine.store.addQuad(
          namedNode(s),
          namedNode(`http://ex/prov#${key}`),
          literal(String(value))
        );
      }
    }
  }

  // Emit after write event
  await engine.eventBus.emit('afterWrite', {
    event: 'afterWrite',
    quad: quads,
    context: engine.eventBus.createContext('provenance', {
      source,
      operationId,
      quadCount: quads.length,
      completed: true,
      ...metadata
    }),
    store: engine.store,
    engine
  });
}

/**
 * Query provenance information for a subject
 * @param {RdfEngine} engine - RDF engine instance
 * @param {string} subject - Subject URI
 * @returns {Promise<Object>} Provenance information
 */
export async function getProvenance(engine, subject) {
  const query = `
    SELECT ?predicate ?object WHERE {
      <${subject}> ?predicate ?object .
      FILTER(STRSTARTS(STR(?predicate), "http://ex/prov#"))
    }
  `;

  const result = await engine.query(query);
  const provenance = {};

  for (const binding of result.results) {
    const predicate = binding.predicate.value;
    const object = binding.object.value;
    const key = predicate.split('#').pop();
    provenance[key] = object;
  }

  return provenance;
}

/**
 * Query all subjects with their provenance
 * @param {RdfEngine} engine - RDF engine instance
 * @param {Object} [filters] - Filter options
 * @param {string} [filters.source] - Filter by source
 * @param {string} [filters.user] - Filter by user
 * @param {string} [filters.operation] - Filter by operation
 * @param {Date} [filters.since] - Filter by timestamp
 * @returns {Promise<Array>} Array of subjects with provenance
 */
export async function getAllProvenance(engine, filters = {}) {
  let query = `
    SELECT ?subject ?predicate ?object WHERE {
      ?subject ?predicate ?object .
      FILTER(STRSTARTS(STR(?predicate), "http://ex/prov#"))
  `;

  if (filters.source) {
    query += `\n      ?subject <http://ex/prov#source> "${filters.source}" .`;
  }

  if (filters.user) {
    query += `\n      ?subject <http://ex/prov#user> "${filters.user}" .`;
  }

  if (filters.operation) {
    query += `\n      ?subject <http://ex/prov#operation> "${filters.operation}" .`;
  }

  if (filters.since) {
    const sinceStr = filters.since.toISOString();
    query += `\n      ?subject <http://ex/prov#ts> ?ts .`;
    query += `\n      FILTER(?ts >= "${sinceStr}")`;
  }

  query += `\n    }`;

  const result = await engine.query(query);
  const subjects = new Map();

  for (const binding of result.results) {
    const subject = binding.subject.value;
    const predicate = binding.predicate.value;
    const object = binding.object.value;
    const key = predicate.split('#').pop();

    if (!subjects.has(subject)) {
      subjects.set(subject, { subject });
    }
    subjects.get(subject)[key] = object;
  }

  return Array.from(subjects.values());
}

/**
 * Create a provenance report
 * @param {RdfEngine} engine - RDF engine instance
 * @param {Object} [options] - Report options
 * @param {Date} [options.since] - Start date for report
 * @param {Date} [options.until] - End date for report
 * @returns {Promise<Object>} Provenance report
 */
export async function createProvenanceReport(engine, options = {}) {
  const since = options.since || new Date(Date.now() - 24 * 60 * 60 * 1000); // Default: last 24 hours
  const until = options.until || new Date();

  const query = `
    SELECT ?source ?operation ?user (COUNT(?subject) as ?count) WHERE {
      ?subject <http://ex/prov#source> ?source .
      ?subject <http://ex/prov#operation> ?operation .
      ?subject <http://ex/prov#user> ?user .
      ?subject <http://ex/prov#ts> ?ts .
      FILTER(?ts >= "${since.toISOString()}")
      FILTER(?ts <= "${until.toISOString()}")
    }
    GROUP BY ?source ?operation ?user
  `;

  const result = await engine.query(query);
  const report = {
    period: { since: since.toISOString(), until: until.toISOString() },
    summary: {
      totalOperations: 0,
      uniqueUsers: new Set(),
      uniqueSources: new Set(),
      uniqueOperations: new Set()
    },
    details: []
  };

  for (const binding of result.results) {
    const source = binding.source.value;
    const operation = binding.operation.value;
    const user = binding.user.value;
    const count = parseInt(binding.count.value);

    report.summary.totalOperations += count;
    report.summary.uniqueUsers.add(user);
    report.summary.uniqueSources.add(source);
    report.summary.uniqueOperations.add(operation);

    report.details.push({
      source,
      operation,
      user,
      count
    });
  }

  // Convert sets to arrays for JSON serialization
  report.summary.uniqueUsers = Array.from(report.summary.uniqueUsers);
  report.summary.uniqueSources = Array.from(report.summary.uniqueSources);
  report.summary.uniqueOperations = Array.from(report.summary.uniqueOperations);

  return report;
}
