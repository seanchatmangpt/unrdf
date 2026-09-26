import { afterEach, describe, expect, it } from 'vitest';
import { mkdtempSync, mkdirSync, rmSync, writeFileSync } from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {
  classifyTier,
  observePackageGraph,
  renderPackageTurtle,
  stronglyConnectedComponents,
} from '../scripts/unrdf-package-discovery.mjs';

const roots = [];
function fixture(packages) {
  const root = mkdtempSync(path.join(os.tmpdir(), 'unrdf-package-factory-'));
  roots.push(root);
  mkdirSync(path.join(root, 'packages'), { recursive: true });
  for (const [dir, manifest] of Object.entries(packages)) {
    mkdirSync(path.join(root, 'packages', dir, 'src'), { recursive: true });
    if (manifest) {
      writeFileSync(path.join(root, 'packages', dir, 'package.json'), JSON.stringify(manifest, null, 2));
      writeFileSync(path.join(root, 'packages', dir, 'src', 'index.mjs'), 'export const alive = true;\n');
    }
  }
  return root;
}

afterEach(() => { while (roots.length) rmSync(roots.pop(), { recursive: true, force: true }); });

describe('package observation calculus', () => {
  it('classifies compatibility tiers from graph position rather than package names', () => {
    expect(classifyTier({ isPrivate: true, reverseDependencyCount: 99, internalDependencyCount: 0 })).toBe('Internal');
    expect(classifyTier({ isPrivate: false, reverseDependencyCount: 3, internalDependencyCount: 1 })).toBe('Essential');
    expect(classifyTier({ isPrivate: false, reverseDependencyCount: 0, internalDependencyCount: 0 })).toBe('Optional');
    expect(classifyTier({ isPrivate: false, reverseDependencyCount: 1, internalDependencyCount: 1 })).toBe('Extended');
  });

  it('preserves a cycle as one strongly-connected component', () => {
    const adjacency = new Map([
      ['a', ['b']], ['b', ['c']], ['c', ['a']], ['d', []],
    ]);
    expect(stronglyConnectedComponents([...adjacency.keys()], adjacency)).toEqual([['a', 'b', 'c'], ['d']]);
  });

  it('manufactures dependency and reverse-dependency evidence from manifests', () => {
    const root = fixture({
      core: { name: '@unrdf/core', version: '1.0.0', type: 'module', main: './src/index.mjs' },
      app: { name: '@unrdf/app', version: '1.0.0', type: 'module', main: './src/index.mjs', dependencies: { '@unrdf/core': 'workspace:*' } },
    });
    const graph = observePackageGraph(root);
    const app = graph.packages.find(pkg => pkg.name === '@unrdf/app');
    const core = graph.packages.find(pkg => pkg.name === '@unrdf/core');
    expect(app.internalDependencies).toEqual(['@unrdf/core']);
    expect(core.reverseDependencies).toEqual(['@unrdf/app']);
    expect(graph.state).toBe('ALIVE');
  });

  it('does not confuse a package-like directory with an admitted package', () => {
    const root = fixture({
      good: { name: '@unrdf/good', version: '1.0.0', type: 'module', main: './src/index.mjs' },
      designOnly: null,
    });
    const graph = observePackageGraph(root);
    expect(graph.packages.map(pkg => pkg.name)).toEqual(['@unrdf/good']);
    expect(graph.anomalies).toContainEqual(expect.objectContaining({ code: 'PACKAGE_MANIFEST_MISSING', path: 'packages/designOnly' }));
    expect(graph.state).toBe('PARTIAL_ALIVE');
  });

  it('refuses dangling @unrdf dependencies as a broken observation boundary', () => {
    const root = fixture({
      app: { name: '@unrdf/app', version: '1.0.0', type: 'module', main: './src/index.mjs', dependencies: { '@unrdf/missing': 'workspace:*' } },
    });
    const graph = observePackageGraph(root);
    expect(graph.state).toBe('BUILD_BROKEN');
    expect(graph.anomalies).toContainEqual(expect.objectContaining({ code: 'INTERNAL_DEPENDENCY_MISSING', dependency: '@unrdf/missing' }));
  });

  it('emits standalone public-ontology Turtle rather than regex-shaped pseudo-RDF', () => {
    const root = fixture({
      core: { name: '@unrdf/core', version: '1.0.0', description: 'Core', type: 'module', main: './src/index.mjs' },
    });
    const turtle = renderPackageTurtle(observePackageGraph(root));
    expect(turtle).toContain('@prefix prov: <http://www.w3.org/ns/prov#> .');
    expect(turtle).toContain('@prefix doap: <http://usefulinc.com/ns/doap#> .');
    expect(turtle).toContain('<urn:unrdf:package:%40unrdf%2Fcore>');
    expect(turtle).toContain('unrdf:manifestDigest');
  });
});
