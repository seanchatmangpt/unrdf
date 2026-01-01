// contradiction-manifest.mjs
// Explicit contradictions that shape architecture

export const CONTRADICTIONS = [
  {
    name: 'static_vs_dynamic',
    statement: 'Package graph is statically known at build time, but execution order must be dynamic based on runtime analysis',
    pressure: 'Cannot pre-compute optimal order; must analyze at runtime',
    resolution: 'None. Accept both: ship static graph + runtime reordering engine'
  },
  {
    name: 'isolation_vs_coupling',
    statement: 'Each system (AI, Vector, Sync, Queue) is designed as isolated component, but they must couple tightly to function',
    pressure: 'No single system is useful alone; all power comes from integration',
    resolution: 'None. Build bridges that make coupling explicit and observable'
  },
  {
    name: 'observability_tax',
    statement: 'Instrumentation (Observability system) must wrap all operations, but wrapping adds latency and complexity',
    pressure: 'Cannot know if system works without observability; cannot observe without overhead',
    resolution: 'None. Accept overhead as cost of coherence; make it configurable'
  },
  {
    name: 'async_deadlock',
    statement: 'Circular dataflows are detectable (JobQueue → GraphAnalysis → AIIntegration → JobQueue), but may be necessary for feedback',
    pressure: 'No cycles = no learning; with cycles = potential deadlock',
    resolution: 'None. Require explicit backpressure and timeout strategies'
  },
  {
    name: 'knowledge_loss',
    statement: 'Generated utilities.mjs wraps real packages but hides their semantics behind factory functions',
    pressure: 'Abstraction gains convenience but loses discoverability of what packages actually compute',
    resolution: 'None. Maintain both: registry exports AND direct package access'
  },
  {
    name: 'perfect_information',
    statement: 'SemanticExtractor must infer what packages do by analyzing exports, but actual computation is unknown without execution',
    pressure: 'Cannot fully map system without running it; running changes it',
    resolution: 'None. Make inference explicit as probabilistic; require verification through execution'
  }
];
