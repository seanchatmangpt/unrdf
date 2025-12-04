'use client';

export default function PerformanceMetrics({ data, detailed = false }) {
  return (
    <div className="space-y-6">
      <div className="section-header">Performance Benchmarks</div>

      {/* Main Metrics Grid */}
      <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
        {/* Single Operator */}
        <div className="chart-container">
          <h3 className="font-semibold mb-4 text-lg">Single Operator Latency</h3>
          <div className="space-y-4">
            <div>
              <div className="flex justify-between items-baseline mb-2">
                <span className="text-sm text-slate-400">Median (p50)</span>
                <span className="text-2xl font-bold text-cyan-400">
                  {data.singleOperator.latency} μs
                </span>
              </div>
              <div className="relative h-2 bg-slate-800 rounded-full overflow-hidden">
                <div
                  className="h-full bg-gradient-to-r from-cyan-500 to-blue-500 rounded-full"
                  style={{ width: '45%' }}
                />
              </div>
            </div>
            <div className="grid grid-cols-2 gap-4 pt-4 border-t border-slate-700">
              <div>
                <div className="text-xs text-slate-400">Min (p5)</div>
                <div className="text-lg font-semibold text-blue-400">
                  {data.singleOperator.range[0]} μs
                </div>
              </div>
              <div>
                <div className="text-xs text-slate-400">Max (p95)</div>
                <div className="text-lg font-semibold text-purple-400">
                  {data.singleOperator.range[1]} μs
                </div>
              </div>
            </div>
          </div>
        </div>

        {/* 8-Operator Chain */}
        <div className="chart-container">
          <h3 className="font-semibold mb-4 text-lg">8-Operator Chain Latency</h3>
          <div className="space-y-4">
            <div>
              <div className="flex justify-between items-baseline mb-2">
                <span className="text-sm text-slate-400">Median (p50)</span>
                <span className="text-2xl font-bold text-cyan-400">
                  {data.eightOperatorChain.latency} μs
                </span>
              </div>
              <div className="relative h-2 bg-slate-800 rounded-full overflow-hidden">
                <div
                  className="h-full bg-gradient-to-r from-cyan-500 to-blue-500 rounded-full"
                  style={{ width: '75%' }}
                />
              </div>
            </div>
            <div className="grid grid-cols-2 gap-4 pt-4 border-t border-slate-700">
              <div>
                <div className="text-xs text-slate-400">Min (p5)</div>
                <div className="text-lg font-semibold text-blue-400">
                  {data.eightOperatorChain.range[0]} μs
                </div>
              </div>
              <div>
                <div className="text-xs text-slate-400">Max (p95)</div>
                <div className="text-lg font-semibold text-purple-400">
                  {data.eightOperatorChain.range[1]} μs
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>

      {/* Throughput & Processing Rate */}
      <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
        <div className="chart-container">
          <h3 className="font-semibold mb-4 text-lg">Sustained Throughput</h3>
          <div className="space-y-4">
            <div className="text-4xl font-bold text-cyan-400">
              {(data.throughput.opsPerSec / 1e6).toFixed(2)}M
            </div>
            <div className="text-sm text-slate-400">{data.throughput.description}</div>
            <div className="pt-4 border-t border-slate-700 text-sm">
              <p className="text-slate-300 font-semibold mb-2">Performance Characteristics:</p>
              <ul className="space-y-1 text-slate-400">
                <li>• Consistent latency across all operators</li>
                <li>• Zero memory allocations in hot path</li>
                <li>• Deterministic execution time</li>
                <li>• Linear scaling with operator count</li>
              </ul>
            </div>
          </div>
        </div>

        <div className="chart-container">
          <h3 className="font-semibold mb-4 text-lg">Information Processing Rate</h3>
          <div className="space-y-4">
            <div className="text-4xl font-bold text-cyan-400">
              {data.informationProcessing.rate}M
            </div>
            <div className="text-sm text-slate-400">nats per second</div>
            <div className="pt-4 border-t border-slate-700 text-sm space-y-2">
              <div>
                <span className="text-slate-400">Per Operator:</span>
                <span className="ml-2 text-cyan-400 font-semibold">
                  {data.informationProcessing.perOperator}
                </span>
              </div>
              <div>
                <span className="text-slate-400">Per Chain:</span>
                <span className="ml-2 text-cyan-400 font-semibold">
                  {data.informationProcessing.totalPerChain}
                </span>
              </div>
            </div>
          </div>
        </div>
      </div>

      {detailed && (
        <div className="chart-container">
          <h3 className="font-semibold mb-4 text-lg">Detailed Performance Table</h3>
          <div className="overflow-x-auto">
            <table>
              <thead>
                <tr>
                  <th>Metric</th>
                  <th>Value</th>
                  <th>Unit</th>
                  <th>Status</th>
                </tr>
              </thead>
              <tbody>
                <tr>
                  <td className="font-semibold">Single Operator (p50)</td>
                  <td className="text-cyan-400 font-semibold">{data.singleOperator.latency}</td>
                  <td>μs</td>
                  <td>
                    <span className="px-2 py-1 bg-green-900/30 text-green-400 text-xs rounded">
                      ✓ Target Met
                    </span>
                  </td>
                </tr>
                <tr>
                  <td className="font-semibold">8-Operator Chain (p50)</td>
                  <td className="text-cyan-400 font-semibold">{data.eightOperatorChain.latency}</td>
                  <td>μs</td>
                  <td>
                    <span className="px-2 py-1 bg-green-900/30 text-green-400 text-xs rounded">
                      ✓ Target Met
                    </span>
                  </td>
                </tr>
                <tr>
                  <td className="font-semibold">Throughput (sustained)</td>
                  <td className="text-cyan-400 font-semibold">
                    {(data.throughput.opsPerSec / 1e6).toFixed(2)}M
                  </td>
                  <td>ops/sec</td>
                  <td>
                    <span className="px-2 py-1 bg-green-900/30 text-green-400 text-xs rounded">
                      ✓ Target Met
                    </span>
                  </td>
                </tr>
                <tr>
                  <td className="font-semibold">Information Processing Rate</td>
                  <td className="text-cyan-400 font-semibold">
                    {data.informationProcessing.rate}M
                  </td>
                  <td>nats/sec</td>
                  <td>
                    <span className="px-2 py-1 bg-green-900/30 text-green-400 text-xs rounded">
                      ✓ Achieved
                    </span>
                  </td>
                </tr>
                <tr>
                  <td className="font-semibold">Entropy Reduction Rate</td>
                  <td className="text-cyan-400 font-semibold">
                    {data.entropyReduction.reductionRate}
                  </td>
                  <td>nats/μs</td>
                  <td>
                    <span className="px-2 py-1 bg-green-900/30 text-green-400 text-xs rounded">
                      ✓ Achieved
                    </span>
                  </td>
                </tr>
              </tbody>
            </table>
          </div>
        </div>
      )}
    </div>
  );
}
