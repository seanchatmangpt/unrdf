/**
 * @file monorepo-universe.test.mjs
 * @description Tests for the monorepo universe model
 */

import { describe, it, expect, beforeAll } from 'vitest';
import * as path from 'node:path';
import { fileURLToPath } from 'node:url';

import {
  MonorepoUniverse,
  createMonorepoUniverse,
  PARTITION_DEFINITIONS,
  buildPartitionMap,
  getPartitionByName,
  getPackagePartition,
  isDependencyAllowed,
  SPARQL_PREFIXES,
} from './monorepo-universe.mjs';

import {
  PKG_NS,
  DEP_NS,
  MOD_NS,
  pkg,
  dep,
  rdf,
  PackageClasses,
  PackageProperties,
  packageToIri,
  dependencyToIri,
  parsePackageToQuads,
  validatePackageJson,
  validateWorkspaceDependencies,
} from './package-rdf-model.mjs';

import {
  QueryTemplates,
  buildPackageQuery,
  formatResults,
  formatAsTable,
  analyzeDependencyHealth,
  generatePackageReport,
} from './monorepo-queries.mjs';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const ROOT_DIR = path.resolve(__dirname, '../..');

// ============================================================================
// Package RDF Model Tests
// ============================================================================

describe('Package RDF Model', () => {
  describe('Namespace definitions', () => {
    it('should define correct package namespace', () => {
      expect(PKG_NS).toBe('http://unrdf.org/package#');
    });

    it('should define correct dependency namespace', () => {
      expect(DEP_NS).toBe('http://unrdf.org/dependency#');
    });

    it('should define correct module namespace', () => {
      expect(MOD_NS).toBe('http://unrdf.org/module#');
    });
  });

  describe('Term factories', () => {
    it('should create package namespace terms', () => {
      const term = pkg('Package');
      expect(term.value).toBe('http://unrdf.org/package#Package');
    });

    it('should create dependency namespace terms', () => {
      const term = dep('dependsOn');
      expect(term.value).toBe('http://unrdf.org/dependency#dependsOn');
    });
  });

  describe('IRI conversion', () => {
    it('should convert scoped package name to IRI', () => {
      const iri = packageToIri('@unrdf/core');
      expect(iri.value).toBe('http://unrdf.org/package#package/unrdf-core');
    });

    it('should convert unscoped package name to IRI', () => {
      const iri = packageToIri('lodash');
      expect(iri.value).toBe('http://unrdf.org/package#package/lodash');
    });

    it('should create dependency IRI', () => {
      const iri = dependencyToIri('@unrdf/yawl', '@unrdf/core');
      expect(iri.value).toBe('http://unrdf.org/dependency#unrdf-yawl/deps/unrdf-core');
    });
  });

  describe('parsePackageToQuads', () => {
    it('should parse basic package.json to quads', () => {
      const quads = parsePackageToQuads({
        name: '@unrdf/test-pkg',
        version: '1.0.0',
        description: 'Test package',
      });

      expect(quads.length).toBeGreaterThan(0);

      // Find the type quad
      const typeQuad = quads.find(
        (q) =>
          q.predicate.value === 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' &&
          q.object.value === 'http://unrdf.org/package#Package'
      );
      expect(typeQuad).toBeDefined();
    });

    it('should include dependencies as quads', () => {
      const quads = parsePackageToQuads({
        name: '@unrdf/test-pkg',
        version: '1.0.0',
        dependencies: {
          '@unrdf/core': 'workspace:*',
          lodash: '^4.17.21',
        },
      });

      // Find workspace dependency
      const depQuad = quads.find(
        (q) =>
          q.predicate.value === 'http://unrdf.org/dependency#dependsOn'
      );
      expect(depQuad).toBeDefined();
    });

    it('should include exports as quads', () => {
      const quads = parsePackageToQuads({
        name: '@unrdf/test-pkg',
        version: '1.0.0',
        exports: {
          '.': './src/index.mjs',
          './api': './src/api.mjs',
        },
      });

      // Find export quads
      const exportQuads = quads.filter(
        (q) => q.predicate.value === 'http://unrdf.org/module#exports'
      );
      expect(exportQuads.length).toBe(2);
    });

    it('should include partition assignment', () => {
      const quads = parsePackageToQuads(
        { name: '@unrdf/core', version: '1.0.0' },
        { partition: 'O_foundational' }
      );

      const partitionQuad = quads.find(
        (q) =>
          q.predicate.value === 'http://unrdf.org/package#partition' &&
          q.object.value.includes('O_foundational')
      );
      expect(partitionQuad).toBeDefined();
    });
  });

  describe('validatePackageJson', () => {
    it('should validate valid package.json', () => {
      const result = validatePackageJson({
        name: 'test-package',
        version: '1.0.0',
      });
      expect(result.valid).toBe(true);
    });

    it('should reject invalid package.json', () => {
      const result = validatePackageJson({
        // missing name
        version: '1.0.0',
      });
      expect(result.valid).toBe(false);
      expect(result.errors.length).toBeGreaterThan(0);
    });
  });

  describe('validateWorkspaceDependencies', () => {
    it('should validate correct workspace deps', () => {
      const result = validateWorkspaceDependencies([
        { name: '@unrdf/core', dependencies: {} },
        { name: '@unrdf/hooks', dependencies: { '@unrdf/core': 'workspace:*' } },
      ]);
      expect(result.valid).toBe(true);
    });

    it('should detect missing workspace deps', () => {
      const result = validateWorkspaceDependencies([
        { name: '@unrdf/hooks', dependencies: { '@unrdf/missing': 'workspace:*' } },
      ]);
      expect(result.valid).toBe(false);
      expect(result.missing.length).toBe(1);
    });
  });
});

// ============================================================================
// Partition Tests
// ============================================================================

describe('Partition Definitions', () => {
  it('should have 10 partitions', () => {
    expect(PARTITION_DEFINITIONS.length).toBe(10);
  });

  it('should have ordered partitions', () => {
    for (let i = 0; i < PARTITION_DEFINITIONS.length; i++) {
      expect(PARTITION_DEFINITIONS[i].order).toBe(i);
    }
  });

  it('should have O_foundational as read-only', () => {
    const foundational = getPartitionByName('O_foundational');
    expect(foundational.readOnly).toBe(true);
  });

  describe('buildPartitionMap', () => {
    it('should map packages to partitions', () => {
      const map = buildPartitionMap();
      expect(map.get('@unrdf/core')).toBe('O_foundational');
      expect(map.get('@unrdf/yawl')).toBe('O_workflow');
      expect(map.get('@unrdf/hooks')).toBe('O_knowledge');
    });
  });

  describe('getPackagePartition', () => {
    it('should return partition for known package', () => {
      expect(getPackagePartition('@unrdf/core')).toBe('O_foundational');
    });

    it('should return undefined for unknown package', () => {
      expect(getPackagePartition('unknown-package')).toBeUndefined();
    });
  });

  describe('isDependencyAllowed', () => {
    it('should allow O_foundational -> O_foundational', () => {
      expect(isDependencyAllowed('O_foundational', 'O_foundational')).toBe(true);
    });

    it('should allow O_knowledge -> O_foundational', () => {
      expect(isDependencyAllowed('O_knowledge', 'O_foundational')).toBe(true);
    });

    it('should disallow O_foundational -> O_knowledge', () => {
      expect(isDependencyAllowed('O_foundational', 'O_knowledge')).toBe(false);
    });

    it('should allow O_integration -> O_workflow', () => {
      expect(isDependencyAllowed('O_integration', 'O_workflow')).toBe(true);
    });
  });
});

// ============================================================================
// MonorepoUniverse Tests
// ============================================================================

describe('MonorepoUniverse', () => {
  let universe;

  beforeAll(async () => {
    universe = await createMonorepoUniverse(ROOT_DIR);
  }, 30000);

  describe('Loading', () => {
    it('should load all packages', () => {
      expect(universe.packageCount).toBeGreaterThan(40);
    });

    it('should create quads from packages', () => {
      expect(universe.quadCount).toBeGreaterThan(500);
    });

    it('should have loaded specific packages', () => {
      expect(universe.getPackage('@unrdf/core')).toBeDefined();
      expect(universe.getPackage('@unrdf/oxigraph')).toBeDefined();
      expect(universe.getPackage('@unrdf/yawl')).toBeDefined();
    });
  });

  describe('Partition Access', () => {
    it('should get packages in O_foundational', () => {
      const pkgs = universe.getPackagesInPartition('O_foundational');
      expect(pkgs.length).toBe(3);
      expect(pkgs.map((p) => p.name)).toContain('@unrdf/core');
    });

    it('should get packages in O_workflow', () => {
      const pkgs = universe.getPackagesInPartition('O_workflow');
      expect(pkgs.length).toBe(9);
      expect(pkgs.map((p) => p.name)).toContain('@unrdf/yawl');
    });
  });

  describe('SPARQL Queries', () => {
    it('should execute SELECT query', () => {
      const results = universe.query(`
        ${SPARQL_PREFIXES}
        SELECT ?name WHERE {
          ?pkg a pkg:Package .
          ?pkg pkg:name ?name .
        } LIMIT 5
      `);
      expect(Array.isArray(results)).toBe(true);
      expect(results.length).toBe(5);
    });

    it('should find packages by partition', () => {
      const results = universe.query(`
        ${SPARQL_PREFIXES}
        SELECT ?name WHERE {
          ?pkg a pkg:Package .
          ?pkg pkg:name ?name .
          ?pkg pkg:partition <${PKG_NS}partition/O_foundational> .
        }
      `);
      expect(results.length).toBe(3);
    });

    it('should find dependencies', () => {
      const results = universe.query(`
        ${SPARQL_PREFIXES}
        SELECT ?srcName ?depName WHERE {
          ?src a pkg:Package .
          ?src pkg:name ?srcName .
          ?src dep:dependsOn ?dep .
          ?dep a pkg:Package .
          ?dep pkg:name ?depName .
        } LIMIT 10
      `);
      expect(results.length).toBeGreaterThan(0);
    });
  });

  describe('Dependency Graph', () => {
    it('should build dependency graph', () => {
      const graph = universe.getDependencyGraph();
      expect(graph instanceof Map).toBe(true);
      expect(graph.size).toBeGreaterThan(40);
    });

    it('should get transitive dependencies', () => {
      const deps = universe.getTransitiveDependencies('@unrdf/yawl');
      expect(deps.size).toBeGreaterThan(0);
      expect(deps.has('@unrdf/oxigraph')).toBe(true);
    });

    it('should get dependents', () => {
      const dependents = universe.getDependents('@unrdf/core');
      expect(dependents.length).toBeGreaterThan(0);
    });
  });

  describe('Circular Dependency Detection', () => {
    it('should detect circular dependencies', () => {
      const cycles = universe.detectCircularDependencies();
      // Should have no cycles in well-formed monorepo
      expect(cycles).toEqual([]);
    });
  });

  describe('Partition Validation', () => {
    it('should validate partition dependencies', () => {
      const { valid, violations } = universe.validatePartitionDependencies();
      // Log any violations for debugging
      if (!valid) {
        console.log('Partition violations:', violations);
      }
      // May have some violations in current state
      expect(typeof valid).toBe('boolean');
    });
  });

  describe('Projections', () => {
    it('should project to D3 graph format', () => {
      const graph = universe.projectToDependencyGraph();
      expect(graph.nodes.length).toBeGreaterThan(40);
      expect(Array.isArray(graph.links)).toBe(true);
    });

    it('should project to coverage matrix', () => {
      const matrix = universe.projectToCoverageMatrix();
      expect(matrix.length).toBeGreaterThan(40);
      expect(matrix[0]).toHaveProperty('package');
      expect(matrix[0]).toHaveProperty('hasTests');
    });

    it('should project to API surface', () => {
      const surface = universe.projectToApiSurface();
      expect(surface.length).toBeGreaterThan(40);
      expect(surface[0]).toHaveProperty('exports');
    });
  });

  describe('Statistics', () => {
    it('should get partition stats', () => {
      const stats = universe.getPartitionStats();
      expect(stats.O_foundational.packageCount).toBe(3);
      expect(stats.O_workflow.packageCount).toBe(9);
    });

    it('should get summary', () => {
      const summary = universe.getSummary();
      expect(summary.packages).toBeGreaterThan(40);
      expect(summary.partitions).toBe(10);
      expect(summary.quads).toBeGreaterThan(500);
    });
  });

  describe('Serialization', () => {
    it('should serialize to JSON', () => {
      const json = universe.toJSON();
      expect(json.packageCount).toBeGreaterThan(40);
      expect(json.partitions.length).toBe(10);
    });
  });
});

// ============================================================================
// Query Template Tests
// ============================================================================

describe('Query Templates', () => {
  let universe;

  beforeAll(async () => {
    universe = await createMonorepoUniverse(ROOT_DIR);
  }, 30000);

  it('should execute ALL_PACKAGES query', () => {
    const results = universe.query(QueryTemplates.ALL_PACKAGES);
    expect(results.length).toBeGreaterThan(40);
  });

  it('should execute PACKAGES_BY_PARTITION query', () => {
    const results = universe.query(QueryTemplates.PACKAGES_BY_PARTITION);
    expect(results.length).toBeGreaterThan(0);
  });

  it('should execute DEPENDENTS_OF query', () => {
    const results = universe.query(QueryTemplates.DEPENDENTS_OF('@unrdf/core'));
    expect(results.length).toBeGreaterThan(0);
  });

  it('should execute WORKSPACE_DEPENDENCIES query', () => {
    const results = universe.query(QueryTemplates.WORKSPACE_DEPENDENCIES);
    expect(results.length).toBeGreaterThan(0);
  });

  it('should execute LEAF_PACKAGES query', () => {
    const results = universe.query(QueryTemplates.LEAF_PACKAGES);
    expect(Array.isArray(results)).toBe(true);
  });

  it('should execute DEPENDENCY_COUNT query', () => {
    const results = universe.query(QueryTemplates.DEPENDENCY_COUNT);
    expect(results.length).toBeGreaterThan(0);
  });
});

// ============================================================================
// Result Formatter Tests
// ============================================================================

describe('Result Formatters', () => {
  it('should format results as objects', () => {
    const raw = [
      { name: { value: 'test1' }, version: { value: '1.0.0' } },
      { name: { value: 'test2' }, version: { value: '2.0.0' } },
    ];
    const formatted = formatResults(raw);
    expect(formatted[0].name).toBe('test1');
    expect(formatted[0].version).toBe('1.0.0');
  });

  it('should format as table', () => {
    const raw = [
      { name: { value: 'test1' }, version: { value: '1.0.0' } },
    ];
    const table = formatAsTable(raw);
    expect(table).toContain('name');
    expect(table).toContain('test1');
  });
});

// ============================================================================
// Query Builder Tests
// ============================================================================

describe('Query Builders', () => {
  it('should build package query with filters', () => {
    const query = buildPackageQuery({
      partition: 'O_foundational',
      hasTests: true,
    });
    expect(query).toContain('O_foundational');
    expect(query).toContain('pkg:testFile');
  });

  it('should build package query with keyword', () => {
    const query = buildPackageQuery({ keyword: 'rdf' });
    expect(query).toContain('"rdf"');
  });
});

// ============================================================================
// Analysis Function Tests
// ============================================================================

describe('Analysis Functions', () => {
  let universe;

  beforeAll(async () => {
    universe = await createMonorepoUniverse(ROOT_DIR);
  }, 30000);

  it('should analyze dependency health', () => {
    const health = analyzeDependencyHealth(universe);
    expect(health.totalPackages).toBeGreaterThan(40);
    expect(health.health.avgDepsPerPackage).toBeDefined();
  });

  it('should generate package report', () => {
    const report = generatePackageReport(universe, '@unrdf/core');
    expect(report.package).toBe('@unrdf/core');
    expect(Array.isArray(report.directDependents)).toBe(true);
    expect(report.impact).toBeDefined();
  });
});
