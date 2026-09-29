/** Directed capability dependency graph. */
export class CapabilityGraph {
  #nodes = new Map();
  #out = new Map();
  #in = new Map();

  /**
   * Add a capability node.
   * @param {string} id - Unique node id.
   * @param {Object} [metadata] - Metadata, stored as a structured clone.
   * @returns {CapabilityGraph} This graph, for chaining.
   * @throws {TypeError} If `id` is missing.
   * @throws {Error} If the id already exists.
   */
  addNode(id, metadata = {}) {
    if (!id) throw new TypeError('node id is required');
    if (this.#nodes.has(id)) throw new Error(`NODE_DUPLICATE:${id}`);
    this.#nodes.set(id, structuredClone(metadata));
    this.#out.set(id, new Set());
    this.#in.set(id, new Set());
    return this;
  }

  /**
   * Declare that a capability depends on another; rejects self-dependencies and cycles.
   * @param {string} capability - Id of the dependent node.
   * @param {string} dependency - Id of the node it depends on.
   * @returns {CapabilityGraph} This graph, for chaining.
   * @throws {Error} If a node is unknown, the edge is a self-dependency, or it creates a cycle (the edge is left in place when a cycle is detected).
   */
  addDependency(capability, dependency) {
    this.#require(capability);
    this.#require(dependency);
    if (capability === dependency) throw new Error(`SELF_DEPENDENCY:${capability}`);
    this.#out.get(dependency).add(capability);
    this.#in.get(capability).add(dependency);
    this.order();
    return this;
  }

  /**
   * List the nodes a capability depends on.
   * @param {string} id - Node id.
   * @param {Object} [options] - Options.
   * @param {boolean} [options.transitive] - Include indirect dependencies.
   * @returns {string[]} Sorted node ids.
   * @throws {Error} If the node is unknown.
   */
  dependencies(id, { transitive = false } = {}) {
    this.#require(id);
    return this.#walk(id, this.#in, transitive);
  }

  /**
   * List the nodes that depend on a capability.
   * @param {string} id - Node id.
   * @param {Object} [options] - Options.
   * @param {boolean} [options.transitive] - Include indirect dependents.
   * @returns {string[]} Sorted node ids.
   * @throws {Error} If the node is unknown.
   */
  dependents(id, { transitive = false } = {}) {
    this.#require(id);
    return this.#walk(id, this.#out, transitive);
  }

  /**
   * Compute the changed nodes plus everything that transitively depends on them.
   * @param {Iterable<string>} changed - Ids of changed nodes.
   * @returns {string[]} Impacted ids in dependency order.
   * @throws {Error} If an id is unknown or the graph has a cycle.
   */
  impact(changed) {
    const seeds = [...new Set(changed)].sort();
    const impacted = new Set(seeds);
    for (const id of seeds) for (const dependent of this.dependents(id, { transitive: true })) impacted.add(dependent);
    return this.order().filter(id => impacted.has(id));
  }

  /**
   * Topologically sort nodes (dependencies first), breaking ties alphabetically.
   * @returns {string[]} Node ids in dependency order.
   * @throws {Error} With message CAPABILITY_GRAPH_CYCLE if the graph is cyclic.
   */
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

  /**
   * Serialize the graph deterministically.
   * @returns {{nodes: Array<{id: string, metadata: Object, dependencies: string[]}>}} Nodes in dependency order.
   */
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

/**
 * Create an empty capability graph.
 * @returns {CapabilityGraph} A new graph.
 */
export function createCapabilityGraph() { return new CapabilityGraph(); }
