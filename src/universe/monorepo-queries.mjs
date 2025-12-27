/**
 * @file monorepo-queries.mjs
 * @description SPARQL query library for the monorepo universe
 *
 * Provides pre-built queries for common monorepo analysis:
 * - Dependency analysis
 * - Package discovery
 * - Partition analysis
 * - Coverage and testing queries
 * - API surface analysis
 */

import { PKG_NS, DEP_NS, MOD_NS } from './package-rdf-model.mjs';
import { SPARQL_PREFIXES } from './monorepo-universe.mjs';

// ============================================================================
// Query Templates
// ============================================================================

/**
 * SPARQL query templates for common monorepo questions
 */
export const QueryTemplates = {
  // =========================================================================
  // Package Discovery Queries
  // =========================================================================

  /**
   * List all packages in the universe
   */
  ALL_PACKAGES: `
${SPARQL_PREFIXES}
SELECT ?pkg ?name ?version ?description WHERE {
  ?pkg a pkg:Package .
  ?pkg pkg:name ?name .
  OPTIONAL { ?pkg pkg:version ?version }
  OPTIONAL { ?pkg pkg:description ?description }
}
ORDER BY ?name
`,

  /**
   * Count packages by partition
   */
  PACKAGES_BY_PARTITION: `
${SPARQL_PREFIXES}
SELECT ?partition (COUNT(?pkg) AS ?count) WHERE {
  ?pkg a pkg:Package .
  ?pkg pkg:partition ?partition .
}
GROUP BY ?partition
ORDER BY ?partition
`,

  /**
   * List packages in a specific partition (parameterized)
   * @param partitionName - Name of the partition
   */
  PACKAGES_IN_PARTITION: (partitionName) => `
${SPARQL_PREFIXES}
SELECT ?pkg ?name ?version WHERE {
  ?pkg a pkg:Package .
  ?pkg pkg:name ?name .
  ?pkg pkg:partition <${PKG_NS}partition/${partitionName}> .
  OPTIONAL { ?pkg pkg:version ?version }
}
ORDER BY ?name
`,

  // =========================================================================
  // Dependency Analysis Queries
  // =========================================================================

  /**
   * Find all packages that depend on a specific package
   * @param packageName - Package name (e.g., "@unrdf/core")
   */
  DEPENDENTS_OF: (packageName) => {
    const safeName = packageName.replace('@', '').replace('/', '-');
    return `
${SPARQL_PREFIXES}
SELECT ?pkg ?name WHERE {
  ?pkg a pkg:Package .
  ?pkg pkg:name ?name .
  ?pkg dep:dependsOn <${PKG_NS}package/${safeName}> .
}
ORDER BY ?name
`;
  },

  /**
   * Find all dependencies of a specific package
   * @param packageName - Package name
   */
  DEPENDENCIES_OF: (packageName) => {
    const safeName = packageName.replace('@', '').replace('/', '-');
    return `
${SPARQL_PREFIXES}
SELECT ?depName WHERE {
  <${PKG_NS}package/${safeName}> dep:dependsOn ?dep .
  ?dep pkg:name ?depName .
}
ORDER BY ?depName
`;
  },

  /**
   * Find all workspace dependencies
   */
  WORKSPACE_DEPENDENCIES: `
${SPARQL_PREFIXES}
SELECT ?srcName ?depName WHERE {
  ?src a pkg:Package .
  ?src pkg:name ?srcName .
  ?src dep:dependsOn ?dep .
  ?dep a pkg:Package .
  ?dep pkg:name ?depName .
}
ORDER BY ?srcName ?depName
`,

  /**
   * Find packages with no dependencies
   */
  LEAF_PACKAGES: `
${SPARQL_PREFIXES}
SELECT ?pkg ?name WHERE {
  ?pkg a pkg:Package .
  ?pkg pkg:name ?name .
  FILTER NOT EXISTS {
    ?pkg dep:dependsOn ?dep .
    ?dep a pkg:Package .
  }
}
ORDER BY ?name
`,

  /**
   * Find packages that nothing depends on (potential orphans)
   */
  ROOT_PACKAGES: `
${SPARQL_PREFIXES}
SELECT ?pkg ?name WHERE {
  ?pkg a pkg:Package .
  ?pkg pkg:name ?name .
  FILTER NOT EXISTS {
    ?other dep:dependsOn ?pkg .
    ?other a pkg:Package .
  }
}
ORDER BY ?name
`,

  /**
   * Count dependencies per package
   */
  DEPENDENCY_COUNT: `
${SPARQL_PREFIXES}
SELECT ?name (COUNT(?dep) AS ?depCount) WHERE {
  ?pkg a pkg:Package .
  ?pkg pkg:name ?name .
  OPTIONAL {
    ?pkg dep:dependsOn ?dep .
    ?dep a pkg:Package .
  }
}
GROUP BY ?name ?pkg
ORDER BY DESC(?depCount)
`,

  // =========================================================================
  // Transitive Dependency Queries (using property paths)
  // =========================================================================

  /**
   * Find transitive dependencies of a package
   * @param packageName - Package name
   */
  TRANSITIVE_DEPENDENCIES: (packageName) => {
    const safeName = packageName.replace('@', '').replace('/', '-');
    return `
${SPARQL_PREFIXES}
SELECT DISTINCT ?depName WHERE {
  <${PKG_NS}package/${safeName}> dep:dependsOn+ ?dep .
  ?dep pkg:name ?depName .
}
ORDER BY ?depName
`;
  },

  /**
   * Find transitive dependents of a package
   * @param packageName - Package name
   */
  TRANSITIVE_DEPENDENTS: (packageName) => {
    const safeName = packageName.replace('@', '').replace('/', '-');
    return `
${SPARQL_PREFIXES}
SELECT DISTINCT ?name WHERE {
  ?pkg dep:dependsOn+ <${PKG_NS}package/${safeName}> .
  ?pkg pkg:name ?name .
}
ORDER BY ?name
`;
  },

  // =========================================================================
  // Partition Analysis Queries
  // =========================================================================

  /**
   * List all partitions with their properties
   */
  ALL_PARTITIONS: `
${SPARQL_PREFIXES}
SELECT ?partition ?name ?order ?readOnly ?description WHERE {
  ?partition a pkg:Partition .
  ?partition pkg:name ?name .
  ?partition pkg:partitionOrder ?order .
  ?partition pkg:partitionReadOnly ?readOnly .
  OPTIONAL { ?partition pkg:description ?description }
}
ORDER BY ?order
`,

  /**
   * Find cross-partition dependencies
   */
  CROSS_PARTITION_DEPENDENCIES: `
${SPARQL_PREFIXES}
SELECT ?srcName ?srcPartition ?depName ?depPartition WHERE {
  ?src a pkg:Package .
  ?src pkg:name ?srcName .
  ?src pkg:partition ?srcPart .
  ?srcPart pkg:name ?srcPartition .

  ?src dep:dependsOn ?dep .
  ?dep a pkg:Package .
  ?dep pkg:name ?depName .
  ?dep pkg:partition ?depPart .
  ?depPart pkg:name ?depPartition .

  FILTER(?srcPartition != ?depPartition)
}
ORDER BY ?srcPartition ?srcName
`,

  // =========================================================================
  // Test and Coverage Queries
  // =========================================================================

  /**
   * Find packages with test files
   */
  PACKAGES_WITH_TESTS: `
${SPARQL_PREFIXES}
SELECT ?name (COUNT(?test) AS ?testCount) WHERE {
  ?pkg a pkg:Package .
  ?pkg pkg:name ?name .
  OPTIONAL {
    ?pkg pkg:testFile ?test .
  }
}
GROUP BY ?name ?pkg
HAVING (COUNT(?test) > 0)
ORDER BY DESC(?testCount)
`,

  /**
   * Find packages without test files
   */
  PACKAGES_WITHOUT_TESTS: `
${SPARQL_PREFIXES}
SELECT ?name WHERE {
  ?pkg a pkg:Package .
  ?pkg pkg:name ?name .
  FILTER NOT EXISTS {
    ?pkg pkg:testFile ?test .
  }
}
ORDER BY ?name
`,

  /**
   * Find packages with test:coverage script
   */
  PACKAGES_WITH_COVERAGE: `
${SPARQL_PREFIXES}
SELECT ?name ?coverageScript WHERE {
  ?pkg a pkg:Package .
  ?pkg pkg:name ?name .
  ?pkg pkg:hasScript ?script .
  ?script pkg:scriptName "test:coverage" .
  ?script pkg:scriptCommand ?coverageScript .
}
ORDER BY ?name
`,

  // =========================================================================
  // Export/API Surface Queries
  // =========================================================================

  /**
   * List all exports per package
   */
  PACKAGE_EXPORTS: `
${SPARQL_PREFIXES}
SELECT ?name ?exportPath ?exportTarget WHERE {
  ?pkg a pkg:Package .
  ?pkg pkg:name ?name .
  ?pkg mod:exports ?export .
  ?export mod:exportsPath ?exportPath .
  ?export mod:exportsTarget ?exportTarget .
}
ORDER BY ?name ?exportPath
`,

  /**
   * Count exports per package
   */
  EXPORT_COUNT: `
${SPARQL_PREFIXES}
SELECT ?name (COUNT(?export) AS ?exportCount) WHERE {
  ?pkg a pkg:Package .
  ?pkg pkg:name ?name .
  OPTIONAL {
    ?pkg mod:exports ?export .
  }
}
GROUP BY ?name ?pkg
ORDER BY DESC(?exportCount)
`,

  /**
   * Find packages with side effects
   */
  PACKAGES_WITH_SIDE_EFFECTS: `
${SPARQL_PREFIXES}
SELECT ?name WHERE {
  ?pkg a pkg:Package .
  ?pkg pkg:name ?name .
  ?pkg pkg:sideEffects "true"^^xsd:boolean .
}
ORDER BY ?name
`,

  // =========================================================================
  // Keyword and Categorization Queries
  // =========================================================================

  /**
   * Group packages by keyword
   */
  PACKAGES_BY_KEYWORD: `
${SPARQL_PREFIXES}
SELECT ?keyword (GROUP_CONCAT(?name; SEPARATOR=", ") AS ?packages) (COUNT(?pkg) AS ?count) WHERE {
  ?pkg a pkg:Package .
  ?pkg pkg:name ?name .
  ?pkg pkg:keyword ?keyword .
}
GROUP BY ?keyword
ORDER BY DESC(?count)
`,

  /**
   * Find packages with a specific keyword
   * @param keyword - Keyword to search for
   */
  PACKAGES_WITH_KEYWORD: (keyword) => `
${SPARQL_PREFIXES}
SELECT ?name ?description WHERE {
  ?pkg a pkg:Package .
  ?pkg pkg:name ?name .
  ?pkg pkg:keyword "${keyword}" .
  OPTIONAL { ?pkg pkg:description ?description }
}
ORDER BY ?name
`,

  // =========================================================================
  // Version and License Queries
  // =========================================================================

  /**
   * List packages by version
   */
  PACKAGES_BY_VERSION: `
${SPARQL_PREFIXES}
SELECT ?version (GROUP_CONCAT(?name; SEPARATOR=", ") AS ?packages) (COUNT(?pkg) AS ?count) WHERE {
  ?pkg a pkg:Package .
  ?pkg pkg:name ?name .
  ?pkg pkg:version ?version .
}
GROUP BY ?version
ORDER BY ?version
`,

  /**
   * List packages by license
   */
  PACKAGES_BY_LICENSE: `
${SPARQL_PREFIXES}
SELECT ?license (GROUP_CONCAT(?name; SEPARATOR=", ") AS ?packages) (COUNT(?pkg) AS ?count) WHERE {
  ?pkg a pkg:Package .
  ?pkg pkg:name ?name .
  OPTIONAL { ?pkg pkg:license ?license }
}
GROUP BY ?license
ORDER BY DESC(?count)
`,

  // =========================================================================
  // Script Analysis Queries
  // =========================================================================

  /**
   * Find all scripts across packages
   */
  ALL_SCRIPTS: `
${SPARQL_PREFIXES}
SELECT ?pkgName ?scriptName ?command WHERE {
  ?pkg a pkg:Package .
  ?pkg pkg:name ?pkgName .
  ?pkg pkg:hasScript ?script .
  ?script pkg:scriptName ?scriptName .
  ?script pkg:scriptCommand ?command .
}
ORDER BY ?pkgName ?scriptName
`,

  /**
   * Find packages with a specific script
   * @param scriptName - Script name (e.g., "test", "build")
   */
  PACKAGES_WITH_SCRIPT: (scriptName) => `
${SPARQL_PREFIXES}
SELECT ?name ?command WHERE {
  ?pkg a pkg:Package .
  ?pkg pkg:name ?name .
  ?pkg pkg:hasScript ?script .
  ?script pkg:scriptName "${scriptName}" .
  ?script pkg:scriptCommand ?command .
}
ORDER BY ?name
`,
};

// ============================================================================
// Query Builders
// ============================================================================

/**
 * Build a custom package filter query
 *
 * @param {Object} filters - Filter options
 * @param {string} [filters.partition] - Filter by partition
 * @param {string} [filters.keyword] - Filter by keyword
 * @param {string} [filters.version] - Filter by version
 * @param {boolean} [filters.hasTests] - Filter by test presence
 * @returns {string} SPARQL query
 *
 * @example
 * ```javascript
 * const query = buildPackageQuery({
 *   partition: 'O_foundational',
 *   hasTests: true
 * });
 * ```
 */
export function buildPackageQuery(filters = {}) {
  const conditions = ['?pkg a pkg:Package .', '?pkg pkg:name ?name .'];
  const optionals = [];

  if (filters.partition) {
    conditions.push(`?pkg pkg:partition <${PKG_NS}partition/${filters.partition}> .`);
  }

  if (filters.keyword) {
    conditions.push(`?pkg pkg:keyword "${filters.keyword}" .`);
  }

  if (filters.version) {
    conditions.push(`?pkg pkg:version "${filters.version}" .`);
  } else {
    optionals.push('OPTIONAL { ?pkg pkg:version ?version }');
  }

  if (filters.hasTests === true) {
    conditions.push('?pkg pkg:testFile ?test .');
  } else if (filters.hasTests === false) {
    conditions.push('FILTER NOT EXISTS { ?pkg pkg:testFile ?test }');
  }

  optionals.push('OPTIONAL { ?pkg pkg:description ?description }');

  const whereClause = [...conditions, ...optionals].join('\n  ');

  return `
${SPARQL_PREFIXES}
SELECT DISTINCT ?pkg ?name ?version ?description WHERE {
  ${whereClause}
}
ORDER BY ?name
`;
}

/**
 * Build a dependency path query
 *
 * @param {string} fromPackage - Source package name
 * @param {string} toPackage - Target package name
 * @returns {string} SPARQL query to find dependency path
 */
export function buildDependencyPathQuery(fromPackage, toPackage) {
  const safeFrom = fromPackage.replace('@', '').replace('/', '-');
  const safeTo = toPackage.replace('@', '').replace('/', '-');

  return `
${SPARQL_PREFIXES}
SELECT ?path WHERE {
  <${PKG_NS}package/${safeFrom}> dep:dependsOn* ?path .
  ?path dep:dependsOn* <${PKG_NS}package/${safeTo}> .
}
`;
}

/**
 * Build an impact analysis query
 * (Find all packages affected if a package changes)
 *
 * @param {string} packageName - Package name to analyze
 * @returns {string} SPARQL query
 */
export function buildImpactAnalysisQuery(packageName) {
  const safeName = packageName.replace('@', '').replace('/', '-');

  return `
${SPARQL_PREFIXES}
SELECT DISTINCT ?affectedName ?partition ?depth WHERE {
  {
    # Direct dependents
    ?affected dep:dependsOn <${PKG_NS}package/${safeName}> .
    ?affected pkg:name ?affectedName .
    OPTIONAL { ?affected pkg:partition ?part . ?part pkg:name ?partition }
    BIND(1 AS ?depth)
  }
  UNION
  {
    # Transitive dependents (depth 2)
    ?intermediate dep:dependsOn <${PKG_NS}package/${safeName}> .
    ?affected dep:dependsOn ?intermediate .
    ?affected pkg:name ?affectedName .
    OPTIONAL { ?affected pkg:partition ?part . ?part pkg:name ?partition }
    BIND(2 AS ?depth)
  }
}
ORDER BY ?depth ?affectedName
`;
}

// ============================================================================
// Result Formatters
// ============================================================================

/**
 * Format SPARQL SELECT results as a simple array of objects
 *
 * @param {Array} results - SPARQL query results
 * @returns {Array<Object>} Formatted results
 *
 * @example
 * ```javascript
 * const results = universe.query(QueryTemplates.ALL_PACKAGES);
 * const formatted = formatResults(results);
 * console.log(formatted);
 * // [{ pkg: '...', name: '@unrdf/core', version: '5.0.1' }, ...]
 * ```
 */
export function formatResults(results) {
  if (!Array.isArray(results)) {
    return results;
  }

  return results.map((binding) => {
    const obj = {};
    for (const [key, value] of Object.entries(binding)) {
      obj[key] = value?.value ?? value;
    }
    return obj;
  });
}

/**
 * Format results as a table string
 *
 * @param {Array} results - SPARQL query results
 * @param {Array<string>} [columns] - Column names to display
 * @returns {string} Formatted table
 */
export function formatAsTable(results, columns) {
  const formatted = formatResults(results);
  if (formatted.length === 0) return 'No results';

  const cols = columns || Object.keys(formatted[0]);
  const widths = cols.map((col) => {
    const maxContent = Math.max(
      col.length,
      ...formatted.map((row) => String(row[col] || '').length)
    );
    return Math.min(maxContent, 50);
  });

  const header = cols.map((col, i) => col.padEnd(widths[i])).join(' | ');
  const separator = widths.map((w) => '-'.repeat(w)).join('-+-');

  const rows = formatted.map((row) =>
    cols.map((col, i) => {
      const val = String(row[col] || '');
      return val.length > widths[i]
        ? val.substring(0, widths[i] - 3) + '...'
        : val.padEnd(widths[i]);
    }).join(' | ')
  );

  return [header, separator, ...rows].join('\n');
}

/**
 * Format results as CSV
 *
 * @param {Array} results - SPARQL query results
 * @returns {string} CSV string
 */
export function formatAsCsv(results) {
  const formatted = formatResults(results);
  if (formatted.length === 0) return '';

  const cols = Object.keys(formatted[0]);
  const header = cols.map((c) => `"${c}"`).join(',');
  const rows = formatted.map((row) =>
    cols.map((c) => `"${String(row[c] || '').replace(/"/g, '""')}"`).join(',')
  );

  return [header, ...rows].join('\n');
}

/**
 * Format results as JSON
 *
 * @param {Array} results - SPARQL query results
 * @param {number} [indent=2] - JSON indentation
 * @returns {string} JSON string
 */
export function formatAsJson(results, indent = 2) {
  const formatted = formatResults(results);
  return JSON.stringify(formatted, null, indent);
}

// ============================================================================
// Query Execution Helpers
// ============================================================================

/**
 * Execute a query and format results
 *
 * @param {import('./monorepo-universe.mjs').MonorepoUniverse} universe - Universe instance
 * @param {string} query - SPARQL query
 * @param {Object} [options] - Options
 * @param {'table' | 'json' | 'csv' | 'raw'} [options.format='raw'] - Output format
 * @returns {string | Array} Formatted results
 *
 * @example
 * ```javascript
 * const result = executeQuery(universe, QueryTemplates.ALL_PACKAGES, { format: 'table' });
 * console.log(result);
 * ```
 */
export function executeQuery(universe, query, options = {}) {
  const results = universe.query(query);
  const format = options.format || 'raw';

  switch (format) {
    case 'table':
      return formatAsTable(results);
    case 'json':
      return formatAsJson(results);
    case 'csv':
      return formatAsCsv(results);
    default:
      return formatResults(results);
  }
}

/**
 * Batch execute multiple queries
 *
 * @param {import('./monorepo-universe.mjs').MonorepoUniverse} universe - Universe instance
 * @param {Object<string, string>} queries - Map of query name to query string
 * @returns {Object<string, Array>} Map of query name to results
 *
 * @example
 * ```javascript
 * const results = batchExecuteQueries(universe, {
 *   packages: QueryTemplates.ALL_PACKAGES,
 *   partitions: QueryTemplates.ALL_PARTITIONS,
 * });
 * ```
 */
export function batchExecuteQueries(universe, queries) {
  const results = {};
  for (const [name, query] of Object.entries(queries)) {
    results[name] = formatResults(universe.query(query));
  }
  return results;
}

// ============================================================================
// Common Analysis Functions
// ============================================================================

/**
 * Analyze the dependency health of the universe
 *
 * @param {import('./monorepo-universe.mjs').MonorepoUniverse} universe - Universe instance
 * @returns {Object} Health analysis report
 */
export function analyzeDependencyHealth(universe) {
  const allPackages = formatResults(universe.query(QueryTemplates.ALL_PACKAGES));
  const leafPackages = formatResults(universe.query(QueryTemplates.LEAF_PACKAGES));
  const rootPackages = formatResults(universe.query(QueryTemplates.ROOT_PACKAGES));
  const depCounts = formatResults(universe.query(QueryTemplates.DEPENDENCY_COUNT));
  const crossPartition = formatResults(universe.query(QueryTemplates.CROSS_PARTITION_DEPENDENCIES));

  const avgDeps = depCounts.reduce((sum, r) => sum + Number(r.depCount || 0), 0) / depCounts.length;
  const maxDeps = Math.max(...depCounts.map((r) => Number(r.depCount || 0)));

  return {
    totalPackages: allPackages.length,
    leafPackages: leafPackages.length,
    rootPackages: rootPackages.length,
    averageDependencies: avgDeps.toFixed(2),
    maxDependencies: maxDeps,
    crossPartitionDependencies: crossPartition.length,
    health: {
      leafRatio: (leafPackages.length / allPackages.length).toFixed(2),
      rootRatio: (rootPackages.length / allPackages.length).toFixed(2),
      avgDepsPerPackage: avgDeps.toFixed(2),
    },
  };
}

/**
 * Generate a dependency report for a specific package
 *
 * @param {import('./monorepo-universe.mjs').MonorepoUniverse} universe - Universe instance
 * @param {string} packageName - Package to analyze
 * @returns {Object} Package dependency report
 */
export function generatePackageReport(universe, packageName) {
  const dependents = formatResults(universe.query(QueryTemplates.DEPENDENTS_OF(packageName)));
  const dependencies = formatResults(universe.query(QueryTemplates.DEPENDENCIES_OF(packageName)));
  const transitiveDeps = formatResults(universe.query(QueryTemplates.TRANSITIVE_DEPENDENCIES(packageName)));
  const transitiveDependents = formatResults(universe.query(QueryTemplates.TRANSITIVE_DEPENDENTS(packageName)));

  return {
    package: packageName,
    directDependencies: dependencies.map((d) => d.depName),
    directDependents: dependents.map((d) => d.name),
    transitiveDependencies: transitiveDeps.map((d) => d.depName),
    transitiveDependents: transitiveDependents.map((d) => d.name),
    impact: {
      directImpact: dependents.length,
      transitiveImpact: transitiveDependents.length,
      impactScore: dependents.length + transitiveDependents.length * 0.5,
    },
  };
}

// ============================================================================
// Exports
// ============================================================================

export default {
  // Query templates
  QueryTemplates,

  // Query builders
  buildPackageQuery,
  buildDependencyPathQuery,
  buildImpactAnalysisQuery,

  // Formatters
  formatResults,
  formatAsTable,
  formatAsCsv,
  formatAsJson,

  // Execution helpers
  executeQuery,
  batchExecuteQueries,

  // Analysis functions
  analyzeDependencyHealth,
  generatePackageReport,
};
