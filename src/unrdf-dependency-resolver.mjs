/**
 * @file src/unrdf-dependency-resolver.mjs
 * @description Ontology-driven dependency resolution from RDF package data
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const projectRoot = path.resolve(__dirname, '..');

class DependencyResolver {
  constructor() {
    this.turtleData = null;
    this.dependencies = new Map();
    this.initialized = false;
  }

  async initialize() {
    if (this.initialized) return;

    const turtlePath = path.join(projectRoot, 'schema', 'packages-discovered.ttl');

    if (!fs.existsSync(turtlePath)) {
      throw new Error(`Ontology file not found: ${turtlePath}`);
    }

    this.turtleData = fs.readFileSync(turtlePath, 'utf-8');
    this._parseOntology();
    this.initialized = true;
  }

  _parseOntology() {
    const packageBlocks = this.turtleData.split(/unrdf:\w+Package a unrdf:Package/);

    for (const block of packageBlocks.slice(1)) {
      const nameMatch = block.match(/unrdf:packageName "([^"]+)"/);
      if (!nameMatch) continue;

      const pkgName = nameMatch[1];
      const depMatches = [...block.matchAll(/unrdf:hasDependency unrdf:(\w+)Package\s*;/g)];

      const deps = [];
      for (const match of depMatches) {
        const depVarName = match[1];
        const depName = this._varNameToPackageName(depVarName);
        if (depName) deps.push(depName);
      }

      this.dependencies.set(pkgName, deps);
    }
  }

  _varNameToPackageName(varName) {
    // Convert CamelCase back to kebab-case
    const withHyphens = varName.replace(/([A-Z])/g, '-$1').toLowerCase();
    const cleaned = withHyphens.replace(/^-/, '').replace(/-package$/, '');
    return `@unrdf/${cleaned}`;
  }

  async resolve(packageName, options = {}) {
    if (!this.initialized) await this.initialize();

    const { includeOptional = true, checkConflicts = true } = options;

    const resolved = new Set();
    const queue = [packageName];
    const visited = new Set();

    while (queue.length > 0) {
      const current = queue.shift();

      if (visited.has(current)) {
        continue;
      }
      visited.add(current);

      const deps = this.dependencies.get(current) || [];

      for (const dep of deps) {
        resolved.add(dep);
        queue.push(dep);
      }
    }

    resolved.add(packageName);

    const sortedDeps = this._topologicalSort(Array.from(resolved));

    if (checkConflicts) {
      const conflicts = this._findConflicts(sortedDeps);
      if (conflicts.length > 0) {
        return {
          success: false,
          conflicts,
          resolved: null,
        };
      }
    }

    return {
      success: true,
      resolved: sortedDeps,
      conflicts: [],
    };
  }

  _topologicalSort(packages) {
    const graph = new Map();
    const inDegree = new Map();

    for (const pkg of packages) {
      graph.set(pkg, this.dependencies.get(pkg) || []);
      inDegree.set(pkg, 0);
    }

    for (const pkg of packages) {
      const deps = graph.get(pkg) || [];
      for (const dep of deps) {
        if (inDegree.has(dep)) {
          inDegree.set(dep, (inDegree.get(dep) || 0) + 1);
        }
      }
    }

    const queue = [];
    for (const pkg of packages) {
      if ((inDegree.get(pkg) || 0) === 0) {
        queue.push(pkg);
      }
    }

    const sorted = [];
    while (queue.length > 0) {
      const current = queue.shift();
      sorted.push(current);

      for (const pkg of graph.get(current) || []) {
        inDegree.set(pkg, (inDegree.get(pkg) || 0) - 1);
        if (inDegree.get(pkg) === 0) {
          queue.push(pkg);
        }
      }
    }

    return sorted;
  }

  _findConflicts(packages) {
    const conflicts = [];
    const seen = new Set();

    for (const pkg of packages) {
      if (seen.has(pkg)) {
        conflicts.push({
          type: 'duplicate',
          package: pkg,
          message: `Package ${pkg} appears multiple times in dependency tree`,
        });
      }
      seen.add(pkg);
    }

    return conflicts;
  }

  async getFullDependencyTree(packageName) {
    if (!this.initialized) await this.initialize();

    const tree = {};
    const queue = [{ name: packageName, level: 0 }];
    const visited = new Set();

    while (queue.length > 0) {
      const { name, level } = queue.shift();

      if (visited.has(name)) {
        continue;
      }
      visited.add(name);

      tree[name] = {
        level,
        dependencies: this.dependencies.get(name) || [],
      };

      const deps = this.dependencies.get(name) || [];
      for (const dep of deps) {
        queue.push({ name: dep, level: level + 1 });
      }
    }

    return tree;
  }

  async getDirectDependencies(packageName) {
    if (!this.initialized) await this.initialize();
    return this.dependencies.get(packageName) || [];
  }

  async getReverseDependencies(packageName) {
    if (!this.initialized) await this.initialize();

    const reverseDeps = [];
    for (const [pkg, deps] of this.dependencies.entries()) {
      if (deps.includes(packageName)) {
        reverseDeps.push(pkg);
      }
    }
    return reverseDeps;
  }

  async getTotalDependencyCount(packageName) {
    if (!this.initialized) await this.initialize();

    const result = await this.resolve(packageName);
    if (!result.success) {
      throw new Error(`Failed to resolve dependencies: ${result.conflicts.map((c) => c.message).join(', ')}`);
    }

    return result.resolved.length - 1;
  }

  async getSharedDependencies(packageNames) {
    if (!this.initialized) await this.initialize();

    if (packageNames.length === 0) return [];
    if (packageNames.length === 1) return [];

    const depSets = [];
    for (const pkg of packageNames) {
      const result = await this.resolve(pkg);
      if (result.success) {
        depSets.push(new Set(result.resolved));
      }
    }

    if (depSets.length === 0) return [];

    let shared = depSets[0];
    for (let i = 1; i < depSets.length; i++) {
      shared = new Set([...shared].filter((x) => depSets[i].has(x)));
    }

    return Array.from(shared).sort();
  }

  async analyzeDepthAndBreadth(packageName) {
    if (!this.initialized) await this.initialize();

    const tree = await this.getFullDependencyTree(packageName);
    const maxDepth = Math.max(0, ...Object.values(tree).map((node) => node.level));
    const breadth = Object.keys(tree).length - 1;

    const depCountByLevel = {};
    for (const [pkg, node] of Object.entries(tree)) {
      if (!depCountByLevel[node.level]) {
        depCountByLevel[node.level] = 0;
      }
      depCountByLevel[node.level]++;
    }

    return {
      depth: maxDepth,
      breadth,
      totalDependencies: breadth,
      depthDistribution: depCountByLevel,
    };
  }
}

export const resolver = new DependencyResolver();

export async function getResolver() {
  await resolver.initialize();
  return resolver;
}

export default resolver;
