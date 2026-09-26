/**
 * Dependency resolver over the admitted ggen package projection.
 * Cycles are preserved as topology and surfaced as SCCs; they are not silently
 * discarded or misclassified as duplicate-package conflicts.
 */
import { getRegistry } from './unrdf-package-registry.mjs';

function tarjan(nodes, adjacency) {
  const indices = new Map(), low = new Map(), stack = [], onStack = new Set(), components = [];
  let cursor = 0;
  const visit = node => {
    indices.set(node, cursor); low.set(node, cursor); cursor += 1; stack.push(node); onStack.add(node);
    for (const next of [...(adjacency.get(node) || [])].sort()) {
      if (!indices.has(next)) { visit(next); low.set(node, Math.min(low.get(node), low.get(next))); }
      else if (onStack.has(next)) low.set(node, Math.min(low.get(node), indices.get(next)));
    }
    if (low.get(node) === indices.get(node)) {
      const component = [];
      while (stack.length) { const member = stack.pop(); onStack.delete(member); component.push(member); if (member === node) break; }
      components.push(component.sort());
    }
  };
  for (const node of [...nodes].sort()) if (!indices.has(node)) visit(node);
  return components.sort((a, b) => a[0].localeCompare(b[0]));
}

export class DependencyResolver {
  constructor() { this.registry = null; this.dependencies = new Map(); this.initialized = false; }

  async initialize() {
    if (this.initialized) return;
    this.registry = await getRegistry();
    this.dependencies = new Map(this.registry.getAllPackages().map(pkg => [pkg.name, [...(pkg.dependencies || [])]]));
    this.initialized = true;
  }

  async resolve(packageName, options = {}) {
    if (!this.initialized) await this.initialize();
    if (!this.dependencies.has(packageName)) return { success: false, conflicts: [{ type: 'missing-package', package: packageName, message: `Package ${packageName} is not in the admitted graph` }], resolved: null, cycles: [] };
    const visited = new Set();
    const missing = new Set();
    const visit = name => {
      if (visited.has(name)) return;
      visited.add(name);
      for (const dep of this.dependencies.get(name) || []) {
        if (!this.dependencies.has(dep)) missing.add(dep);
        else visit(dep);
      }
    };
    visit(packageName);
    const closure = [...visited].sort();
    const closureSet = new Set(closure);
    const adjacency = new Map(closure.map(name => [name, (this.dependencies.get(name) || []).filter(dep => closureSet.has(dep))]));
    const cycles = tarjan(closure, adjacency).filter(component => component.length > 1 || (adjacency.get(component[0]) || []).includes(component[0]));
    const conflicts = [...missing].sort().map(dep => ({ type: 'missing-dependency', package: dep, message: `Dependency ${dep} is not in the admitted graph` }));
    return { success: options.checkConflicts !== false ? conflicts.length === 0 : true, resolved: conflicts.length && options.checkConflicts !== false ? null : closure, conflicts, cycles };
  }

  async getFullDependencyTree(packageName) {
    if (!this.initialized) await this.initialize();
    const tree = {};
    const queue = [{ name: packageName, level: 0 }];
    const visited = new Set();
    while (queue.length) {
      const { name, level } = queue.shift();
      if (visited.has(name)) continue;
      visited.add(name);
      const dependencies = [...(this.dependencies.get(name) || [])];
      tree[name] = { level, dependencies };
      for (const dep of dependencies) if (this.dependencies.has(dep)) queue.push({ name: dep, level: level + 1 });
    }
    return tree;
  }

  async getDirectDependencies(packageName) { if (!this.initialized) await this.initialize(); return [...(this.dependencies.get(packageName) || [])]; }
  async getReverseDependencies(packageName) { if (!this.initialized) await this.initialize(); return this.registry.getReverseDependencies(packageName); }

  async getTotalDependencyCount(packageName) {
    const result = await this.resolve(packageName);
    if (!result.success) throw new Error(result.conflicts.map(item => item.message).join(', '));
    return result.resolved.length - 1;
  }

  async getSharedDependencies(packageNames) {
    if (!packageNames.length) return [];
    const closures = [];
    for (const name of packageNames) { const result = await this.resolve(name); if (result.success) closures.push(new Set(result.resolved)); }
    if (!closures.length) return [];
    return [...closures.slice(1).reduce((shared, next) => new Set([...shared].filter(item => next.has(item))), closures[0])].sort();
  }

  async getStronglyConnectedComponents() {
    if (!this.initialized) await this.initialize();
    return tarjan([...this.dependencies.keys()], this.dependencies);
  }

  async analyzeDepthAndBreadth(packageName) {
    const tree = await this.getFullDependencyTree(packageName);
    const result = await this.resolve(packageName, { checkConflicts: false });
    const maxDepth = Math.max(0, ...Object.values(tree).map(node => node.level));
    const depthDistribution = {};
    for (const node of Object.values(tree)) depthDistribution[node.level] = (depthDistribution[node.level] || 0) + 1;
    return { depth: maxDepth, breadth: Math.max(0, Object.keys(tree).length - 1), totalDependencies: Math.max(0, Object.keys(tree).length - 1), depthDistribution, cycles: result.cycles };
  }
}

export const resolver = new DependencyResolver();
export async function getResolver() { await resolver.initialize(); return resolver; }
export default resolver;
