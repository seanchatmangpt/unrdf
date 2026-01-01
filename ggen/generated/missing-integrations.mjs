// missing-integrations.mjs
// Find what must exist but doesn't

export class MissingIntegrationAnalyzer {
  constructor(executionGraph, semanticMap) {
    this.graph = executionGraph;
    this.semantics = semanticMap;
    this.gaps = [];
  }

  analyze() {
    // For each dataflow in the graph, check if a bridge exists
    for (const flow of this.graph.dataflows) {
      const sourceSemantics = this.semantics.get(flow.source);
      const targetSemantics = this.semantics.get(flow.target);

      if (!sourceSemantics || !targetSemantics) {
        this.gaps.push({
          type: 'missing_semantics',
          source: flow.source,
          target: flow.target
        });
        continue;
      }

      // Check if there's a type transformer
      const needsTransform = !this._directlyCompatible(
        sourceSemantics,
        targetSemantics
      );

      if (needsTransform) {
        const transformer = this._findTransformer(sourceSemantics, targetSemantics);
        if (!transformer) {
          this.gaps.push({
            type: 'missing_transformer',
            from: sourceSemantics.produces,
            to: targetSemantics.consumes,
            source: flow.source,
            target: flow.target
          });
        }
      }
    }

    // Check for systems that consume but nothing produces
    for (const [name, semantics] of this.semantics.entries()) {
      for (const computation of semantics) {
        const hasProducer = this.graph.dataflows.some(
          f => this._satisfies(f, computation.consumes)
        );

        if (!hasProducer && computation.consumes !== 'unknown') {
          this.gaps.push({
            type: 'missing_producer',
            system: name,
            computation: computation.name,
            needs: computation.consumes
          });
        }
      }
    }

    return this.gaps;
  }

  _directlyCompatible(source, target) {
    if (!source || !target) return false;

    const sourceProduces = source[0]?.produces;
    const targetConsumes = target[0]?.consumes;

    return sourceProduces === targetConsumes;
  }

  _findTransformer(source, target) {
    // Stub: would look for existing adapters
    return null;
  }

  _satisfies(flow, need) {
    // Check if this dataflow provides what's needed
    return flow.data === need;
  }

  getMissingTransformers() {
    return this.gaps.filter(g => g.type === 'missing_transformer');
  }

  getMissingProducers() {
    return this.gaps.filter(g => g.type === 'missing_producer');
  }

  getGaps() {
    return this.gaps;
  }
}
