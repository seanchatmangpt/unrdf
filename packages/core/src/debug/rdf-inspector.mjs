/**
 * @file RDF Graph Inspector - Statistics, Quality Metrics, Schema Validation
 * @module @unrdf/core/debug/rdf-inspector
 * @description Pure function RDF graph inspection and quality analysis
 */

import { z } from 'zod';

/**
 * Inspector options schema
 */
const InspectorOptionsSchema = z.object({
  includeNamespaces: z.boolean().optional(),
  includeOrphans: z.boolean().optional(),
  includeQuality: z.boolean().optional(),
  sampleSize: z.number().int().positive().optional(),
}).optional();

/**
 * Get comprehensive graph statistics
 * @param {Object} store - RDF store
 * @param {Object} [options] - Inspector options
 * @returns {Object} Graph statistics
 *
 * @example
 * const stats = getGraphStatistics(store);
 * console.log(`Triples: ${stats.tripleCount}`);
 */
export function getGraphStatistics(store, options = {}) {
  if (!store || typeof store.match !== 'function') {
    throw new TypeError('store must be a valid RDF store');
  }

  const opts = InspectorOptionsSchema.parse(options);

  const subjects = new Set();
  const predicates = new Set();
  const objects = new Set();
  const literals = new Set();
  const bnodes = new Set();
  const graphs = new Set();

  let tripleCount = 0;

  for (const quad of store.match(null, null, null, null)) {
    tripleCount++;

    subjects.add(quad.subject.value);
    predicates.add(quad.predicate.value);
    objects.add(quad.object.value);

    if (quad.object.termType === 'Literal') {
      literals.add(quad.object.value);
    }

    if (quad.subject.termType === 'BlankNode') {
      bnodes.add(quad.subject.value);
    }

    if (quad.graph && quad.graph.value) {
      graphs.add(quad.graph.value);
    }
  }

  const stats = {
    tripleCount,
    subjectCount: subjects.size,
    predicateCount: predicates.size,
    objectCount: objects.size,
    literalCount: literals.size,
    blankNodeCount: bnodes.size,
    graphCount: graphs.size,
    averageDegree: subjects.size > 0 ? tripleCount / subjects.size : 0,
  };

  if (opts?.includeNamespaces) {
    stats.namespaces = analyzeNamespaces(store);
  }

  if (opts?.includeOrphans) {
    stats.orphans = detectOrphans(store);
  }

  if (opts?.includeQuality) {
    stats.quality = assessDataQuality(store);
  }

  return stats;
}

/**
 * Analyze namespace usage
 * @param {Object} store - RDF store
 * @returns {Object} Namespace analysis
 *
 * @example
 * const namespaces = analyzeNamespaces(store);
 * console.log(namespaces);
 */
export function analyzeNamespaces(store) {
  if (!store || typeof store.match !== 'function') {
    throw new TypeError('store must be a valid RDF store');
  }

  const namespaces = new Map();

  for (const quad of store.match(null, null, null, null)) {
    const extractNS = (iri) => {
      if (!iri || typeof iri !== 'string') return null;
      const match = iri.match(/^(.+[/#])([^/#]+)$/);
      return match ? match[1] : null;
    };

    [quad.subject.value, quad.predicate.value, quad.object.value]
      .map(extractNS)
      .filter(ns => ns !== null)
      .forEach(ns => {
        namespaces.set(ns, (namespaces.get(ns) || 0) + 1);
      });
  }

  return Array.from(namespaces.entries())
    .map(([namespace, count]) => ({ namespace, count }))
    .sort((a, b) => b.count - a.count);
}

/**
 * Detect orphaned nodes (no incoming edges)
 * @param {Object} store - RDF store
 * @returns {Array} Array of orphaned node IRIs
 *
 * @example
 * const orphans = detectOrphans(store);
 * console.log(`Found ${orphans.length} orphaned nodes`);
 */
export function detectOrphans(store) {
  if (!store || typeof store.match !== 'function') {
    throw new TypeError('store must be a valid RDF store');
  }

  const subjects = new Set();
  const objects = new Set();

  for (const quad of store.match(null, null, null, null)) {
    subjects.add(quad.subject.value);
    if (quad.object.termType !== 'Literal') {
      objects.add(quad.object.value);
    }
  }

  const orphans = [];
  for (const subject of subjects) {
    if (!objects.has(subject)) {
      orphans.push(subject);
    }
  }

  return orphans;
}

/**
 * Assess data quality metrics
 * @param {Object} store - RDF store
 * @returns {Object} Quality assessment
 *
 * @example
 * const quality = assessDataQuality(store);
 * console.log(`Quality score: ${quality.score}/100`);
 */
export function assessDataQuality(store) {
  if (!store || typeof store.match !== 'function') {
    throw new TypeError('store must be a valid RDF store');
  }

  let totalTriples = 0;
  let triplesWithLabels = 0;
  let triplesWithTypes = 0;
  let literalsWithLanguage = 0;
  let literalsWithDatatype = 0;
  let totalLiterals = 0;

  const subjects = new Set();

  for (const quad of store.match(null, null, null, null)) {
    totalTriples++;
    subjects.add(quad.subject.value);

    if (quad.predicate.value === 'http://www.w3.org/2000/01/rdf-schema#label') {
      triplesWithLabels++;
    }

    if (quad.predicate.value === 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type') {
      triplesWithTypes++;
    }

    if (quad.object.termType === 'Literal') {
      totalLiterals++;
      if (quad.object.language) {
        literalsWithLanguage++;
      }
      if (quad.object.datatype && quad.object.datatype.value) {
        literalsWithDatatype++;
      }
    }
  }

  const labelCoverage = subjects.size > 0 ? Math.min((triplesWithLabels / subjects.size) * 100, 100) : 0;
  const typeCoverage = subjects.size > 0 ? Math.min((triplesWithTypes / subjects.size) * 100, 100) : 0;
  const literalQuality = totalLiterals > 0
    ? Math.min(((literalsWithLanguage + literalsWithDatatype) / totalLiterals) * 100, 100)
    : 100;

  const score = Math.round((labelCoverage + typeCoverage + literalQuality) / 3);

  return {
    score,
    labelCoverage: Math.round(labelCoverage),
    typeCoverage: Math.round(typeCoverage),
    literalQuality: Math.round(literalQuality),
    metrics: {
      totalTriples,
      uniqueSubjects: subjects.size,
      triplesWithLabels,
      triplesWithTypes,
      literalsWithLanguage,
      literalsWithDatatype,
    },
  };
}

/**
 * Check schema conformance (SHACL-like basic validation)
 * @param {Object} store - RDF store
 * @param {Object} schema - Schema definition
 * @returns {Object} Conformance report
 *
 * @example
 * const report = checkSchemaConformance(store, {
 *   requiredPredicates: ['http://www.w3.org/1999/02/22-rdf-syntax-ns#type']
 * });
 */
export function checkSchemaConformance(store, schema) {
  if (!store || typeof store.match !== 'function') {
    throw new TypeError('store must be a valid RDF store');
  }

  if (!schema || typeof schema !== 'object') {
    throw new TypeError('schema must be an object');
  }

  const violations = [];
  const subjects = new Set();

  for (const quad of store.match(null, null, null, null)) {
    subjects.add(quad.subject.value);
  }

  if (schema.requiredPredicates) {
    for (const subject of subjects) {
      for (const predicate of schema.requiredPredicates) {
        const matches = Array.from(store.match(
          { termType: 'NamedNode', value: subject },
          { termType: 'NamedNode', value: predicate },
          null,
          null
        ));

        if (matches.length === 0) {
          violations.push({
            subject,
            type: 'MISSING_REQUIRED_PREDICATE',
            predicate,
            message: `Subject ${subject} missing required predicate ${predicate}`,
          });
        }
      }
    }
  }

  if (schema.minTriples && Array.from(store.match(null, null, null, null)).length < schema.minTriples) {
    violations.push({
      type: 'MIN_TRIPLES_VIOLATION',
      message: `Graph has fewer than ${schema.minTriples} triples`,
    });
  }

  return {
    conforms: violations.length === 0,
    violationCount: violations.length,
    violations: violations.slice(0, 100),
  };
}

/**
 * Generate comprehensive inspection report
 * @param {Object} store - RDF store
 * @param {Object} [options] - Inspector options
 * @returns {string} Formatted report
 *
 * @example
 * const report = generateInspectionReport(store, { includeQuality: true });
 * console.log(report);
 */
export function generateInspectionReport(store, options = {}) {
  if (!store || typeof store.match !== 'function') {
    throw new TypeError('store must be a valid RDF store');
  }

  const stats = getGraphStatistics(store, {
    ...options,
    includeNamespaces: true,
    includeOrphans: true,
    includeQuality: true,
  });

  let report = '╔═══════════════════════════════════════════════════════╗\n';
  report += '║           RDF GRAPH INSPECTION REPORT                 ║\n';
  report += '╠═══════════════════════════════════════════════════════╣\n';
  report += `║ Total Triples:        ${String(stats.tripleCount).padStart(10)} triples     ║\n`;
  report += `║ Unique Subjects:      ${String(stats.subjectCount).padStart(10)} subjects    ║\n`;
  report += `║ Unique Predicates:    ${String(stats.predicateCount).padStart(10)} predicates  ║\n`;
  report += `║ Unique Objects:       ${String(stats.objectCount).padStart(10)} objects     ║\n`;
  report += `║ Literals:             ${String(stats.literalCount).padStart(10)} literals    ║\n`;
  report += `║ Blank Nodes:          ${String(stats.blankNodeCount).padStart(10)} bnodes      ║\n`;
  report += `║ Named Graphs:         ${String(stats.graphCount).padStart(10)} graphs      ║\n`;
  report += `║ Average Degree:       ${String(stats.averageDegree.toFixed(2)).padStart(10)} edges/node  ║\n`;
  report += '╠═══════════════════════════════════════════════════════╣\n';

  if (stats.quality) {
    report += `║ QUALITY SCORE:        ${String(stats.quality.score).padStart(10)}/100         ║\n`;
    report += `║   Label Coverage:     ${String(stats.quality.labelCoverage).padStart(10)}%           ║\n`;
    report += `║   Type Coverage:      ${String(stats.quality.typeCoverage).padStart(10)}%           ║\n`;
    report += `║   Literal Quality:    ${String(stats.quality.literalQuality).padStart(10)}%           ║\n`;
    report += '╠═══════════════════════════════════════════════════════╣\n';
  }

  if (stats.orphans && stats.orphans.length > 0) {
    report += `║ Orphaned Nodes:       ${String(stats.orphans.length).padStart(10)} found       ║\n`;
  }

  if (stats.namespaces && stats.namespaces.length > 0) {
    report += '║ Top Namespaces:                                       ║\n';
    for (const ns of stats.namespaces.slice(0, 3)) {
      const nsShort = ns.namespace.substring(0, 30);
      report += `║   ${nsShort.padEnd(35)} ${String(ns.count).padStart(10)} ║\n`;
    }
  }

  report += '╚═══════════════════════════════════════════════════════╝\n';

  return report;
}
