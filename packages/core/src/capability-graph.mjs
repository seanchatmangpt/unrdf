/** Directed capability dependency graph. */
export class CapabilityGraph {
  #nodes = new Map();
  #out = new Map();
  #in = new Map();

  addNode(id, metadata = {}) {
    if (!id) throw new TypeError('node id is required');
    if (this.#nodes.has(id)) throw new Error(`NODE_DUPLICATE:${id}`);
    this.#nodes.set(id, structuredClone(metadata));
    this.#out.set(id, new Set());
    this.#in.set(id, new Set());
    return this;
  }

  addDependency(capability, dependency) {
    this.#require(capability);
    this.#require(dependency);
    if (capability === dependency) throw new Error(`SELF_DEPENDENCY:${capability}`);
    this.#out.get(dependency).add(capability);
    this.#in.get(capability).add(dependency);
    this.order();
    return this;
  }

  dependencies(id, { transitive = false } = {}) {
    this.#require(id);
    return this.#walk(id, this.#in, transitive);
  }

  dependents(id, { transitive = false } = {}) {
    this.#require(id);
    return this.#walk(id, this.#out, transitive);
  }

  impact(changed) {
    const seeds = [...new Set(changed)].sort();
    const impacted = new Set(seeds);
    for (const id of seeds) for (const dependent of this.dependents(id, { transitive: true })) impacted.add(dependent);
    return this.order().filter(id => impacted.has(id));
  }

  order() {
    const indegree = new Map([...this.#nodes.keys()].map(id => [id, this.#in.get(id).size]));
    const ready = [...indegree].filter(([, degree]) => degree === 0).map(([id]) => id).sort();
    const result = [];
    while (ready.length) {
      const id = ready.shift();
      result.push(id);
      for (const next of [...this.#out.get(id)].sort()) {
        indegree.set(next, indegree.get(next) - 1);
        if (indegree.get(next) === 0) {
          ready.push(next);
          ready.sort();
        }
      }
    }
    if (result.length !== this.#nodes.size) throw new Error('CAPABILITY_GRAPH_CYCLE');
    return result;
  }

  toJSON() {
    return { nodes: this.order().map(id => ({ id, metadata: structuredClone(this.#nodes.get(id)), dependencies: this.dependencies(id) })) };
  }

  #walk(id, edges, transitive) {
    const direct = [...edges.get(id)].sort();
    if (!transitive) return direct;
    const seen = new Set();
    const queue = [...direct];
    while (queue.length) {
      const current = queue.shift();
      if (seen.has(current)) continue;
      seen.add(current);
      queue.push(...[...edges.get(current)].sort());
    }
    return [...seen].sort();
  }

  #require(id) { if (!this.#nodes.has(id)) throw new Error(`NODE_NOT_FOUND:${id}`); }
}

export function createCapabilityGraph() { return new CapabilityGraph(); }
