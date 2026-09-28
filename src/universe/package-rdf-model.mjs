/**
 * @file package-rdf-model.mjs
 * @description RDF schema and parsers for package manifests, modules, and dependencies
 *
 * Defines the RDF vocabulary for representing monorepo packages:
 * - Classes: Package, Module, Function, Type, Test, Documentation
 * - Properties: dependsOn, exports, tests, coverage, version, etc.
 * - Parsers: package.json -> RDF quads
 */

import { createStore, dataFactory } from '@unrdf/oxigraph';
import { z } from 'zod';

const { namedNode, literal, quad, defaultGraph } = dataFactory;

// ============================================================================
// RDF Namespaces
// ============================================================================

/**
 * Package namespace for UNRDF package vocabulary
 * @type {string}
 */
export const PKG_NS = 'http://unrdf.org/package#';

/**
 * Dependency namespace for dependency relationships
 * @type {string}
 */
export const DEP_NS = 'http://unrdf.org/dependency#';

/**
 * Module namespace for module definitions
 * @type {string}
 */
export const MOD_NS = 'http://unrdf.org/module#';

/**
 * Common RDF namespaces
 */
export const RDF_NS = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';
export const RDFS_NS = 'http://www.w3.org/2000/01/rdf-schema#';
export const XSD_NS = 'http://www.w3.org/2001/XMLSchema#';
export const PROV_NS = 'http://www.w3.org/ns/prov#';

// ============================================================================
// RDF Term Factories
// ============================================================================

/**
 * Create package namespace terms
 * @param {string} localName - Local name within package namespace
 * @returns {import('oxigraph').NamedNode}
 */
export function pkg(localName) {
  return namedNode(`${PKG_NS}${localName}`);
}

/**
 * Create dependency namespace terms
 * @param {string} localName - Local name within dependency namespace
 * @returns {import('oxigraph').NamedNode}
 */
export function dep(localName) {
  return namedNode(`${DEP_NS}${localName}`);
}

/**
 * Create module namespace terms
 * @param {string} localName - Local name within module namespace
 * @returns {import('oxigraph').NamedNode}
 */
export function mod(localName) {
  return namedNode(`${MOD_NS}${localName}`);
}

/**
 * Create RDF namespace terms
 * @param {string} localName - Local name within RDF namespace
 * @returns {import('oxigraph').NamedNode}
 */
export function rdf(localName) {
  return namedNode(`${RDF_NS}${localName}`);
}

/**
 * Create RDFS namespace terms
 * @param {string} localName - Local name within RDFS namespace
 * @returns {import('oxigraph').NamedNode}
 */
export function rdfs(localName) {
  return namedNode(`${RDFS_NS}${localName}`);
}

/**
 * Create XSD namespace terms
 * @param {string} localName - Local name within XSD namespace
 * @returns {import('oxigraph').NamedNode}
 */
export function xsd(localName) {
  return namedNode(`${XSD_NS}${localName}`);
}

// ============================================================================
// Package RDF Classes
// ============================================================================

/**
 * RDF Classes for the package vocabulary
 */
export const PackageClasses = {
  /** Package class - represents an npm package */
  Package: pkg('Package'),
  /** Module class - represents a JavaScript module */
  Module: pkg('Module'),
  /** Function class - represents an exported function */
  Function: pkg('Function'),
  /** Type class - represents a TypeScript/JSDoc type */
  Type: pkg('Type'),
  /** Test class - represents a test file */
  Test: pkg('Test'),
  /** Documentation class - represents documentation */
  Documentation: pkg('Documentation'),
  /** Partition class - represents a package partition group */
  Partition: pkg('Partition'),
  /** Export class - represents a package export */
  Export: pkg('Export'),
  /** Script class - represents an npm script */
  Script: pkg('Script'),
  /** Dependency class - represents a dependency relationship */
  Dependency: dep('Dependency'),
  /** WorkspaceDependency - internal monorepo dependency */
  WorkspaceDependency: dep('WorkspaceDependency'),
  /** ExternalDependency - external npm dependency */
  ExternalDependency: dep('ExternalDependency'),
};

/**
 * RDF Properties for the package vocabulary
 */
export const PackageProperties = {
  // Package properties
  name: pkg('name'),
  version: pkg('version'),
  description: pkg('description'),
  main: pkg('main'),
  type: pkg('type'),
  license: pkg('license'),
  homepage: pkg('homepage'),
  repository: pkg('repository'),
  sideEffects: pkg('sideEffects'),

  // Dependency relationships
  dependsOn: dep('dependsOn'),
  devDependsOn: dep('devDependsOn'),
  peerDependsOn: dep('peerDependsOn'),
  dependencyVersion: dep('version'),
  dependencyType: dep('type'),
  isWorkspaceDependency: dep('isWorkspace'),

  // Module properties
  exports: mod('exports'),
  exportsPath: mod('exportsPath'),
  exportsTarget: mod('exportsTarget'),

  // Test properties
  testFile: pkg('testFile'),
  testCount: pkg('testCount'),
  coverage: pkg('coverage'),

  // Documentation properties
  hasDocumentation: pkg('hasDocumentation'),
  documentationPath: pkg('documentationPath'),

  // Partition properties
  partition: pkg('partition'),
  partitionOrder: pkg('partitionOrder'),
  partitionReadOnly: pkg('partitionReadOnly'),

  // Script properties
  hasScript: pkg('hasScript'),
  scriptName: pkg('scriptName'),
  scriptCommand: pkg('scriptCommand'),

  // Keyword properties
  keyword: pkg('keyword'),

  // Engine properties
  nodeEngine: pkg('nodeEngine'),
  pnpmEngine: pkg('pnpmEngine'),
};

// ============================================================================
// Zod Schemas for Validation
// ============================================================================

/**
 * Package.json dependency schema
 */
const DependencyMapSchema = z.record(z.string()).optional().default({});

/**
 * Package.json exports schema (simplified)
 */
const ExportsSchema = z.union([
  z.string(),
  z.record(z.string()),
]).optional();

/**
 * Package.json scripts schema
 */
const ScriptsSchema = z.record(z.string()).optional().default({});

/**
 * Package.json engines schema
 */
const EnginesSchema = z.object({
  node: z.string().optional(),
  pnpm: z.string().optional(),
}).optional();

/**
 * Repository schema
 */
const RepositorySchema = z.union([
  z.string(),
  z.object({
    type: z.string().optional(),
    url: z.string().optional(),
    directory: z.string().optional(),
  }),
]).optional();

/**
 * Complete package.json schema
 */
export const PackageJsonSchema = z.object({
  name: z.string(),
  version: z.string().optional().default('0.0.0'),
  description: z.string().optional().default(''),
  type: z.string().optional().default('module'),
  main: z.string().optional(),
  exports: ExportsSchema,
  types: z.string().optional(),
  sideEffects: z.boolean().optional().default(false),
  scripts: ScriptsSchema,
  keywords: z.array(z.string()).optional().default([]),
  dependencies: DependencyMapSchema,
  devDependencies: DependencyMapSchema,
  peerDependencies: DependencyMapSchema,
  engines: EnginesSchema,
  repository: RepositorySchema,
  license: z.string().optional(),
  homepage: z.string().optional(),
  files: z.array(z.string()).optional().default([]),
});

/**
 * @typedef {z.infer<typeof PackageJsonSchema>} PackageJson
 */

// ============================================================================
// Package to RDF Conversion
// ============================================================================

/**
 * Convert a package name to a valid IRI
 * @param {string} packageName - Package name (e.g., "@unrdf/core")
 * @returns {import('oxigraph').NamedNode} Package IRI
 */
export function packageToIri(packageName) {
  // Remove @ prefix and replace / with -
  const safeName = packageName.replace('@', '').replace('/', '-');
  return namedNode(`${PKG_NS}package/${safeName}`);
}

/**
 * Convert a dependency to a valid IRI
 * @param {string} packageName - Source package name
 * @param {string} depName - Dependency name
 * @returns {import('oxigraph').NamedNode} Dependency IRI
 */
export function dependencyToIri(packageName, depName) {
  const safePkg = packageName.replace('@', '').replace('/', '-');
  const safeDep = depName.replace('@', '').replace('/', '-');
  return namedNode(`${DEP_NS}${safePkg}/deps/${safeDep}`);
}

/**
 * Convert a module export to a valid IRI
 * @param {string} packageName - Package name
 * @param {string} exportPath - Export path (e.g., "./api")
 * @returns {import('oxigraph').NamedNode} Export IRI
 */
export function exportToIri(packageName, exportPath) {
  const safePkg = packageName.replace('@', '').replace('/', '-');
  const safeExport = exportPath.replace('./', '').replace('/', '-') || 'main';
  return namedNode(`${MOD_NS}${safePkg}/exports/${safeExport}`);
}

/**
 * Convert a script to a valid IRI
 * @param {string} packageName - Package name
 * @param {string} scriptName - Script name (e.g., "test")
 * @returns {import('oxigraph').NamedNode} Script IRI
 */
export function scriptToIri(packageName, scriptName) {
  const safePkg = packageName.replace('@', '').replace('/', '-');
  return namedNode(`${PKG_NS}${safePkg}/scripts/${scriptName}`);
}

/**
 * Parse package.json into RDF quads
 *
 * @param {PackageJson} packageJson - Parsed package.json content
 * @param {Object} [options] - Parsing options
 * @param {string} [options.packagePath] - Path to package directory
 * @param {string} [options.partition] - Partition this package belongs to
 * @returns {Array<import('oxigraph').Quad>} Array of RDF quads
 *
 * @example
 * ```javascript
 * const quads = parsePackageToQuads({
 *   name: "@unrdf/core",
 *   version: "5.0.1",
 *   dependencies: { "@unrdf/oxigraph": "workspace:*" }
 * });
 * console.log(`Generated ${quads.length} quads`);
 * ```
 */
export function parsePackageToQuads(packageJson, options = {}) {
  const validated = PackageJsonSchema.parse(packageJson);
  const quads = [];
  const graph = defaultGraph();

  const pkgIri = packageToIri(validated.name);

  // Package type and basic properties
  quads.push(quad(pkgIri, rdf('type'), PackageClasses.Package, graph));
  quads.push(quad(pkgIri, PackageProperties.name, literal(validated.name), graph));
  quads.push(quad(pkgIri, PackageProperties.version, literal(validated.version), graph));

  if (validated.description) {
    quads.push(quad(pkgIri, PackageProperties.description, literal(validated.description), graph));
  }

  if (validated.main) {
    quads.push(quad(pkgIri, PackageProperties.main, literal(validated.main), graph));
  }

  quads.push(quad(pkgIri, PackageProperties.type, literal(validated.type), graph));

  if (validated.license) {
    quads.push(quad(pkgIri, PackageProperties.license, literal(validated.license), graph));
  }

  if (validated.homepage) {
    quads.push(quad(pkgIri, PackageProperties.homepage, namedNode(validated.homepage), graph));
  }

  quads.push(quad(
    pkgIri,
    PackageProperties.sideEffects,
    literal(String(validated.sideEffects), xsd('boolean')),
    graph
  ));

  // Partition assignment
  if (options.partition) {
    const partitionIri = namedNode(`${PKG_NS}partition/${options.partition}`);
    quads.push(quad(pkgIri, PackageProperties.partition, partitionIri, graph));
  }

  // Keywords
  for (const keyword of validated.keywords) {
    quads.push(quad(pkgIri, PackageProperties.keyword, literal(keyword), graph));
  }

  // Exports
  if (validated.exports) {
    if (typeof validated.exports === 'string') {
      const expIri = exportToIri(validated.name, '.');
      quads.push(quad(expIri, rdf('type'), PackageClasses.Export, graph));
      quads.push(quad(pkgIri, PackageProperties.exports, expIri, graph));
      quads.push(quad(expIri, PackageProperties.exportsPath, literal('.'), graph));
      quads.push(quad(expIri, PackageProperties.exportsTarget, literal(validated.exports), graph));
    } else {
      for (const [exportPath, exportTarget] of Object.entries(validated.exports)) {
        const expIri = exportToIri(validated.name, exportPath);
        quads.push(quad(expIri, rdf('type'), PackageClasses.Export, graph));
        quads.push(quad(pkgIri, PackageProperties.exports, expIri, graph));
        quads.push(quad(expIri, PackageProperties.exportsPath, literal(exportPath), graph));
        quads.push(quad(expIri, PackageProperties.exportsTarget, literal(exportTarget), graph));
      }
    }
  }

  // Scripts
  for (const [scriptName, scriptCommand] of Object.entries(validated.scripts)) {
    const scriptIri = scriptToIri(validated.name, scriptName);
    quads.push(quad(scriptIri, rdf('type'), PackageClasses.Script, graph));
    quads.push(quad(pkgIri, PackageProperties.hasScript, scriptIri, graph));
    quads.push(quad(scriptIri, PackageProperties.scriptName, literal(scriptName), graph));
    quads.push(quad(scriptIri, PackageProperties.scriptCommand, literal(scriptCommand), graph));
  }

  // Dependencies
  for (const [depName, depVersion] of Object.entries(validated.dependencies)) {
    const depIri = dependencyToIri(validated.name, depName);
    const targetIri = packageToIri(depName);
    const isWorkspace = depVersion === 'workspace:*' || depVersion.startsWith('workspace:');

    quads.push(quad(depIri, rdf('type'), PackageClasses.Dependency, graph));
    quads.push(quad(
      depIri,
      rdf('type'),
      isWorkspace ? PackageClasses.WorkspaceDependency : PackageClasses.ExternalDependency,
      graph
    ));
    quads.push(quad(pkgIri, PackageProperties.dependsOn, targetIri, graph));
    quads.push(quad(depIri, PackageProperties.dependencyVersion, literal(depVersion), graph));
    quads.push(quad(
      depIri,
      PackageProperties.isWorkspaceDependency,
      literal(String(isWorkspace), xsd('boolean')),
      graph
    ));
  }

  // Dev dependencies
  for (const [depName, depVersion] of Object.entries(validated.devDependencies)) {
    const depIri = dependencyToIri(validated.name, `dev-${depName}`);
    const targetIri = packageToIri(depName);

    quads.push(quad(depIri, rdf('type'), PackageClasses.Dependency, graph));
    quads.push(quad(pkgIri, PackageProperties.devDependsOn, targetIri, graph));
    quads.push(quad(depIri, PackageProperties.dependencyVersion, literal(depVersion), graph));
    quads.push(quad(depIri, PackageProperties.dependencyType, literal('dev'), graph));
  }

  // Engines
  if (validated.engines?.node) {
    quads.push(quad(pkgIri, PackageProperties.nodeEngine, literal(validated.engines.node), graph));
  }
  if (validated.engines?.pnpm) {
    quads.push(quad(pkgIri, PackageProperties.pnpmEngine, literal(validated.engines.pnpm), graph));
  }

  return quads;
}

/**
 * Load a package.json file and convert to RDF quads
 *
 * @param {string} packageJsonPath - Path to package.json file
 * @param {Object} [options] - Loading options
 * @returns {Promise<Array<import('oxigraph').Quad>>} Array of RDF quads
 *
 * @example
 * ```javascript
 * const quads = await loadPackageJsonToQuads('/path/to/package.json');
 * ```
 */
export async function loadPackageJsonToQuads(packageJsonPath, options = {}) {
  const fs = await import('node:fs/promises');
  const content = await fs.readFile(packageJsonPath, 'utf-8');
  const packageJson = JSON.parse(content);
  return parsePackageToQuads(packageJson, options);
}

// ============================================================================
// Store Creation
// ============================================================================

/**
 * Create an RDF store from package.json data
 *
 * @param {Array<PackageJson>} packages - Array of package.json objects
 * @param {Object} [options] - Options
 * @param {Map<string, string>} [options.partitionMap] - Map of package name to partition
 * @returns {import('@unrdf/oxigraph').OxigraphStore} RDF store with package data
 *
 * @example
 * ```javascript
 * const store = createPackageStore([pkg1, pkg2, pkg3]);
 * console.log(`Store has ${store.size} quads`);
 * ```
 */
export function createPackageStore(packages, options = {}) {
  const store = createStore();
  const partitionMap = options.partitionMap || new Map();

  for (const pkg of packages) {
    const partition = partitionMap.get(pkg.name);
    const quads = parsePackageToQuads(pkg, { partition });
    for (const q of quads) {
      store.add(q);
    }
  }

  return store;
}

/**
 * Add partition schema quads to a store
 *
 * @param {import('@unrdf/oxigraph').OxigraphStore} store - Target store
 * @param {Array<Object>} partitions - Partition definitions
 *
 * @example
 * ```javascript
 * addPartitionSchema(store, [
 *   { name: 'O_foundational', order: 0, readOnly: true },
 *   { name: 'O_knowledge', order: 1, readOnly: false },
 * ]);
 * ```
 */
export function addPartitionSchema(store, partitions) {
  const graph = defaultGraph();

  for (const partition of partitions) {
    const partitionIri = namedNode(`${PKG_NS}partition/${partition.name}`);

    store.add(quad(partitionIri, rdf('type'), PackageClasses.Partition, graph));
    store.add(quad(partitionIri, PackageProperties.name, literal(partition.name), graph));
    store.add(quad(
      partitionIri,
      PackageProperties.partitionOrder,
      literal(String(partition.order), xsd('integer')),
      graph
    ));
    store.add(quad(
      partitionIri,
      PackageProperties.partitionReadOnly,
      literal(String(partition.readOnly), xsd('boolean')),
      graph
    ));

    if (partition.description) {
      store.add(quad(partitionIri, PackageProperties.description, literal(partition.description), graph));
    }
  }
}

// ============================================================================
// Validation Functions
// ============================================================================

/**
 * Validate package.json content
 *
 * @param {unknown} content - Content to validate
 * @returns {{ valid: boolean, errors: Array<string>, data?: PackageJson }}
 */
export function validatePackageJson(content) {
  const result = PackageJsonSchema.safeParse(content);

  if (result.success) {
    return { valid: true, errors: [], data: result.data };
  }

  return {
    valid: false,
    errors: result.error.errors.map(e => `${e.path.join('.')}: ${e.message}`),
  };
}

/**
 * Validate that all workspace dependencies exist
 *
 * @param {Array<PackageJson>} packages - Array of packages
 * @returns {{ valid: boolean, missing: Array<{from: string, to: string}> }}
 */
export function validateWorkspaceDependencies(packages) {
  const packageNames = new Set(packages.map(p => p.name));
  const missing = [];

  for (const pkg of packages) {
    for (const [depName, depVersion] of Object.entries(pkg.dependencies || {})) {
      if (depVersion === 'workspace:*' || depVersion.startsWith('workspace:')) {
        if (!packageNames.has(depName)) {
          missing.push({ from: pkg.name, to: depName });
        }
      }
    }
  }

  return {
    valid: missing.length === 0,
    missing,
  };
}

// ============================================================================
// Export Types (via JSDoc)
// ============================================================================

/**
 * @typedef {Object} PartitionDefinition
 * @property {string} name - Partition name
 * @property {number} order - Partition order (lower = higher priority)
 * @property {boolean} readOnly - Whether partition is read-only
 * @property {string} [description] - Partition description
 * @property {Array<string>} [packages] - Package names in this partition
 */

/**
 * @typedef {Object} DependencyInfo
 * @property {string} source - Source package name
 * @property {string} target - Target package name
 * @property {string} version - Dependency version
 * @property {boolean} isWorkspace - Whether it's a workspace dependency
 * @property {'runtime' | 'dev' | 'peer'} type - Dependency type
 */

export default {
  // Namespaces
  PKG_NS,
  DEP_NS,
  MOD_NS,
  RDF_NS,
  RDFS_NS,
  XSD_NS,

  // Term factories
  pkg,
  dep,
  mod,
  rdf,
  rdfs,
  xsd,

  // Classes and properties
  PackageClasses,
  PackageProperties,

  // IRI conversion
  packageToIri,
  dependencyToIri,
  exportToIri,
  scriptToIri,

  // Parsing
  parsePackageToQuads,
  loadPackageJsonToQuads,

  // Store creation
  createPackageStore,
  addPartitionSchema,

  // Validation
  PackageJsonSchema,
  validatePackageJson,
  validateWorkspaceDependencies,
};
