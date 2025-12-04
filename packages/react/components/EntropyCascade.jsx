'use client';

export default function EntropyCascade({ data }) {
  const stages = [
    { stage: 0, entropy: 50, label: 'User Intent (λ)', description: 'Initial information content' },
    {
      stage: 1,
      entropy: 45.8,
      label: 'μ₁: Subject IRI Validation',
      description: 'Remove invalid subjects',
    },
    {
      stage: 2,
      entropy: 40.0,
      label: 'μ₂: Predicate Validation',
      description: 'Remove invalid predicates',
    },
    { stage: 3, entropy: 32.9, label: 'μ₃: Object Type Check', description: 'Type enforcement' },
    { stage: 4, entropy: 26.6, label: 'μ₄: Graph Resolution', description: 'Named graph binding' },
    {
      stage: 5,
      entropy: 20.4,
      label: 'μ₅: Semantic Validation',
      description: 'Ontology constraints',
    },
    {
      stage: 6,
      entropy: 14.2,
      label: 'μ₆: Transform Rules',
      description: 'Applied transformations',
    },
    { stage: 7, entropy: 8.0, label: 'μ₇: Notification', description: 'Change propagation' },
    { stage: 8, entropy: 1.0, label: 'Outcome (A)', description: 'Final observable state' },
  ];

  const maxEntropy = 50;

  return (
    <div className="space-y-6">
      <div className="section-header">Entropy Cascade Analysis</div>

      <p className="text-slate-400 text-sm max-w-2xl">
        The μ(O) calculus processes user intent through 8 information-theoretic operators, each
        reducing entropy. The cascade demonstrates how deterministic transformations compress 50
        nats of initial intent into 1 nat of observable outcome.
      </p>

      {/* Cascade Visualization */}
      <div className="space-y-3">
        {stages.map((stage, idx) => (
          <div key={idx} className="space-y-2">
            <div className="flex items-center justify-between">
              <div>
                <div className="font-semibold text-slate-100">{stage.label}</div>
                <div className="text-xs text-slate-400">{stage.description}</div>
              </div>
              <div className="text-right">
                <div className="text-lg font-bold text-cyan-400">{stage.entropy} nats</div>
                <div className="text-xs text-slate-400">
                  H(λ) = {((100 * stage.entropy) / maxEntropy).toFixed(0)}%
                </div>
              </div>
            </div>
            <div className="relative h-8 bg-slate-800 rounded-lg overflow-hidden">
              <div
                className="h-full bg-gradient-to-r from-cyan-500 via-blue-500 to-purple-500 transition-all"
                style={{
                  width: `${(stage.entropy / maxEntropy) * 100}%`,
                }}
              />
              <div className="absolute inset-0 flex items-center px-3 text-xs font-semibold text-white/70">
                {stage.entropy !== 1.0 &&
                  `Δ ${(stages[idx].entropy - stages[idx + 1]?.entropy || 0).toFixed(1)} nats`}
              </div>
            </div>
          </div>
        ))}
      </div>

      {/* Key Metrics */}
      <div className="grid grid-cols-1 md:grid-cols-3 gap-6 pt-6 border-t border-slate-700">
        <div className="chart-container">
          <div className="text-sm text-slate-400 mb-2">Total Entropy Reduction</div>
          <div className="text-3xl font-bold text-cyan-400">49 nats</div>
          <div className="text-xs text-slate-400 mt-2">From 50 to 1 nat</div>
        </div>
        <div className="chart-container">
          <div className="text-sm text-slate-400 mb-2">Average per Operator</div>
          <div className="text-3xl font-bold text-cyan-400">6.1 nats</div>
          <div className="text-xs text-slate-400 mt-2">49 ÷ 8 operators</div>
        </div>
        <div className="chart-container">
          <div className="text-sm text-slate-400 mb-2">Reduction Rate</div>
          <div className="text-3xl font-bold text-cyan-400">7.2 nats/μs</div>
          <div className="text-xs text-slate-400 mt-2">49 nats ÷ 6.82 μs</div>
        </div>
      </div>

      {/* Information Theory Explanation */}
      <div className="chart-container space-y-4">
        <h3 className="font-semibold text-lg">Information-Theoretic Properties</h3>
        <div className="space-y-4 text-sm">
          <div className="border-l-2 border-cyan-500 pl-4">
            <p className="font-semibold text-slate-200 mb-1">Shannon Entropy Reduction</p>
            <p className="text-slate-400">
              H(λ) = 50 nats → H(A) ≤ 1 nat represents the compression of user intent through
              deterministic operators. Each operator reduces uncertainty about the final outcome.
            </p>
          </div>
          <div className="border-l-2 border-blue-500 pl-4">
            <p className="font-semibold text-slate-200 mb-1">Mutual Information Flow</p>
            <p className="text-slate-400">
              I(λ; Aᵢ) quantifies information shared between user intent and operator i's observable
              effects. Operators with higher mutual information have greater impact on final
              outcome.
            </p>
          </div>
          <div className="border-l-2 border-purple-500 pl-4">
            <p className="font-semibold text-slate-200 mb-1">Channel Capacity Constraint</p>
            <p className="text-slate-400">
              C = 6.1 nats/operator limits information transfer. Opacity emerges because some intent
              information is irreversibly lost (diverges to causally-unreachable space).
            </p>
          </div>
        </div>
      </div>
    </div>
  );
}
