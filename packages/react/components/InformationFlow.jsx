'use client';

export default function InformationFlow({ data }) {
  return (
    <div className="space-y-6">
      <div className="section-header">Information-Theoretic Foundation</div>

      <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
        {/* Shannon Entropy Analysis */}
        <div className="chart-container">
          <h3 className="font-semibold mb-4 text-lg">Shannon Entropy Compression</h3>
          <div className="space-y-4">
            <div className="space-y-2">
              <div className="flex justify-between items-baseline">
                <span className="text-sm text-slate-400">Initial Intent Entropy H(λ)</span>
                <span className="text-lg font-bold text-red-400">
                  {data.entropyReduction.initialEntropyNats} nats
                </span>
              </div>
              <div className="h-3 bg-slate-800 rounded-full overflow-hidden">
                <div className="h-full bg-red-500 w-full" />
              </div>
            </div>

            <div className="space-y-2 py-4 border-y border-slate-700">
              <div className="flex justify-between items-baseline">
                <span className="text-sm text-slate-400">Entropy Reduction</span>
                <span className="text-lg font-bold text-cyan-400">
                  {data.entropyReduction.initialEntropyNats -
                    data.entropyReduction.finalEntropyNats}{' '}
                  nats
                </span>
              </div>
              <div className="text-xs text-slate-400">
                Through {data.entropyReduction.cascadeStages} deterministic operators
              </div>
            </div>

            <div className="space-y-2">
              <div className="flex justify-between items-baseline">
                <span className="text-sm text-slate-400">Final Observable Entropy H(A)</span>
                <span className="text-lg font-bold text-green-400">
                  {data.entropyReduction.finalEntropyNats} nat
                </span>
              </div>
              <div className="h-3 bg-slate-800 rounded-full overflow-hidden">
                <div className="h-full bg-green-500" style={{ width: '2%' }} />
              </div>
            </div>
          </div>
        </div>

        {/* Mutual Information */}
        <div className="chart-container">
          <h3 className="font-semibold mb-4 text-lg">Mutual Information I(λ; A)</h3>
          <div className="space-y-4">
            <div className="text-sm text-slate-400 mb-4">
              Quantifies information shared between user intent and observable outcome across all
              operators.
            </div>

            <div className="bg-slate-800/50 border border-slate-700 rounded-lg p-4 font-mono text-xs space-y-2 text-cyan-300">
              <p>I(λ; A) = H(λ) + H(A) - H(λ, A)</p>
              <p>I(λ; A) = 50 + 1 - 2</p>
              <p className="text-cyan-400 font-bold">I(λ; A) ≈ 49 nats</p>
            </div>

            <div className="text-xs text-slate-400">
              <p className="mb-2">
                <span className="font-semibold text-slate-300">Interpretation:</span> Nearly all
                (49/50 = 98%) of user intent information deterministically influences the final
                outcome.
              </p>
              <p>
                The 2% irreversible loss represents information diverging to causally-unreachable
                space (source of inevitable opacity).
              </p>
            </div>
          </div>
        </div>
      </div>

      {/* Channel Capacity Analysis */}
      <div className="chart-container">
        <h3 className="font-semibold mb-4 text-lg">Information Channel Capacity Constraint</h3>
        <div className="space-y-4">
          <p className="text-sm text-slate-400">
            Each operator acts as an information channel with maximum capacity C. Shannon's noisy
            channel coding theorem limits how much entropy can be reduced per operator.
          </p>

          <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
            <div className="bg-slate-800/50 border border-slate-700 rounded-lg p-4 text-center">
              <div className="text-xs text-slate-400 uppercase tracking-wider mb-2">
                Channel Capacity
              </div>
              <div className="text-2xl font-bold text-cyan-400">6.1 nats</div>
              <div className="text-xs text-slate-400 mt-2">per operator</div>
            </div>

            <div className="bg-slate-800/50 border border-slate-700 rounded-lg p-4 text-center">
              <div className="text-xs text-slate-400 uppercase tracking-wider mb-2">
                Total Operators
              </div>
              <div className="text-2xl font-bold text-cyan-400">8</div>
              <div className="text-xs text-slate-400 mt-2">operators minimum required</div>
            </div>

            <div className="bg-slate-800/50 border border-slate-700 rounded-lg p-4 text-center">
              <div className="text-xs text-slate-400 uppercase tracking-wider mb-2">
                Theoretical Max
              </div>
              <div className="text-2xl font-bold text-cyan-400">48.8 nats</div>
              <div className="text-xs text-slate-400 mt-2">total reduction possible</div>
            </div>
          </div>

          <div className="border-t border-slate-700 pt-4 space-y-2 text-sm">
            <p className="font-semibold text-slate-200">Why Opacity is Inevitable</p>
            <p className="text-slate-400">
              Information cannot be perfectly preserved through channels with limited capacity. Some
              information must be lost (compressed). This loss creates opacity: the user cannot
              perfectly reverse-engineer the internal operator chain from observable outcomes alone.
            </p>
          </div>
        </div>
      </div>

      {/* Key Insight */}
      <div className="border-l-4 border-cyan-500 bg-cyan-500/5 rounded-r-lg p-6">
        <h4 className="font-semibold text-lg text-cyan-400 mb-3">
          Key Insight: Opacity as Mathematical Inevitability
        </h4>
        <p className="text-slate-300 text-sm leading-relaxed">
          The μ(O) calculus proves that opacity (inability to observe operators) is not a design
          choice but a mathematical consequence of information-theoretic constraints. With 50 nats
          of user intent compressed to 1 nat of observable outcome through limited-capacity
          channels, some internal information is irreversibly lost. This 2% information loss (0.98
          nat) represents user intent diverging to causally-unreachable space—it cannot be recovered
          from observations alone. Therefore, the operators μ₁...μ₈ remain fundamentally opaque:
          perfect reverse-engineering is impossible. The user only observes outcomes A, never the
          transformation process itself.
        </p>
      </div>
    </div>
  );
}
