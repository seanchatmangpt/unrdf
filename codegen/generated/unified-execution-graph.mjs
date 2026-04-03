// unified-execution-graph.mjs
// Map how systems must interact; expose where they can't

export class UnifiedExecutionGraph {
  constructor() {
    this.nodes = new Map();
    this.edges = [];
    this.dataflows = [];
    this.impossibilities = [];
  }

  addSystem(name, computation) {
    this.nodes.set(name, {
      name,
      produces: computation.produces,
      consumes: computation.consumes,
      isAsync: computation.isAsync,
      dependencies: []
    });
  }

  connectSystems(from, to, dataType) {
    const fromNode = this.nodes.get(from);
    const toNode = this.nodes.get(to);

    if (!fromNode || !toNode) {
      return { connected: false, reason: 'missing_node' };
    }

    // Check if production matches consumption
    const fromProduces = fromNode.produces;
    const toConsumes = toNode.consumes;

    if (!this._typesCompatible(fromProduces, toConsumes)) {
      this.impossibilities.push({
        from,
        to,
        reason: `${from} produces ${fromProduces} but ${to} consumes ${toConsumes}`,
        dataType
      });
      return { connected: false, reason: 'type_mismatch' };
    }

    if (this._wouldCreateDeadlock(from, to)) {
      this.impossibilities.push({
        from,
        to,
        reason: 'would create execution deadlock',
        dataType
      });
      return { connected: false, reason: 'deadlock_risk' };
    }

    this.edges.push({ from, to, dataType });
    fromNode.dependencies.push(to);
    this.dataflows.push({
      source: from,
      target: to,
      data: dataType
    });

    return { connected: true };
  }

  _typesCompatible(produces, consumes) {
    if (consumes === 'unknown' || produces === 'unknown') {
      return true; // Assume compatibility when uncertain
    }

    const compatible = {
      'instance': ['instance', 'structure', 'transformed_data'],
      'result': ['results', 'content', 'analysis'],
      'structure': ['transformed_data', 'instance'],
      'content': ['serialized', 'file'],
      'analysis': ['structure', 'content']
    };

    return compatible[produces]?.includes(consumes) || false;
  }

  _wouldCreateDeadlock(from, to) {
    // Simple: check if to already depends on from
    const visited = new Set();
    const stack = [to];

    while (stack.length) {
      const current = stack.pop();
      if (current === from) return true;
      if (visited.has(current)) continue;

      visited.add(current);
      const deps = this.nodes.get(current)?.dependencies || [];
      stack.push(...deps);
    }

    return false;
  }

  getExecutionOrder() {
    const sorted = [];
    const visited = new Set();
    const visiting = new Set();

    const visit = (name) => {
      if (visited.has(name)) return;
      if (visiting.has(name)) {
        this.impossibilities.push({
          type: 'circular_dependency',
          node: name
        });
        return;
      }

      visiting.add(name);
      const node = this.nodes.get(name);
      (node?.dependencies || []).forEach(visit);
      visiting.delete(name);

      visited.add(name);
      sorted.push(name);
    };

    for (const name of this.nodes.keys()) {
      visit(name);
    }

    return sorted;
  }

  getImpossibilities() {
    return this.impossibilities;
  }

  visualize() {
    const structure = {
      nodes: Array.from(this.nodes.entries()).map(([name, node]) => ({
        name,
        produces: node.produces,
        consumes: node.consumes,
        async: node.isAsync
      })),
      edges: this.edges,
      dataflows: this.dataflows,
      impossibilities: this.impossibilities
    };
    return structure;
  }
}
