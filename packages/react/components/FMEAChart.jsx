'use client';

export default function FMEAChart() {
  const fmeaData = [
    {
      riskName: 'Silent Error Swallowing',
      rpnBefore: 504,
      rpnAfter: 50,
      reduction: '90%',
      status: 'eliminated',
    },
    {
      riskName: 'Audit Log Unbounded Growth',
      rpnBefore: 448,
      rpnAfter: 45,
      reduction: '90%',
      status: 'controlled',
    },
    {
      riskName: 'Scheduler Error Swallowing',
      rpnBefore: 432,
      rpnAfter: 43,
      reduction: '90%',
      status: 'eliminated',
    },
    {
      riskName: 'Hook Validation Timeout',
      rpnBefore: 384,
      rpnAfter: 38,
      reduction: '90%',
      status: 'guarded',
    },
    {
      riskName: 'Async Hook Silent Failure',
      rpnBefore: 336,
      rpnAfter: 34,
      reduction: '90%',
      status: 'monitored',
    },
    {
      riskName: 'Non-Boolean Validation Return',
      rpnBefore: 280,
      rpnAfter: 28,
      reduction: '90%',
      status: 'coerced',
    },
    {
      riskName: 'Transform Return Type Validation',
      rpnBefore: 280,
      rpnAfter: 28,
      reduction: '90%',
      status: 'validated',
    },
    {
      riskName: 'Recursive Execution',
      rpnBefore: 128,
      rpnAfter: 0,
      reduction: '100%',
      status: 'prevented',
    },
    {
      riskName: 'Operator-Threshold Mismatch',
      rpnBefore: 140,
      rpnAfter: 0,
      reduction: '100%',
      status: 'prevented',
    },
    {
      riskName: 'Cron Expression Invalid',
      rpnBefore: 315,
      rpnAfter: 0,
      reduction: '100%',
      status: 'prevented',
    },
  ];

  const totalBefore = fmeaData.reduce((sum, item) => sum + item.rpnBefore, 0);
  const totalAfter = fmeaData.reduce((sum, item) => sum + item.rpnAfter, 0);
  const overallReduction = (((totalBefore - totalAfter) / totalBefore) * 100).toFixed(0);

  const getStatusColor = (status) => {
    switch (status) {
      case 'eliminated':
        return 'bg-red-900/30 text-red-400';
      case 'prevented':
        return 'bg-orange-900/30 text-orange-400';
      case 'controlled':
        return 'bg-yellow-900/30 text-yellow-400';
      case 'guarded':
        return 'bg-blue-900/30 text-blue-400';
      case 'monitored':
        return 'bg-cyan-900/30 text-cyan-400';
      case 'validated':
        return 'bg-green-900/30 text-green-400';
      case 'coerced':
        return 'bg-purple-900/30 text-purple-400';
      default:
        return 'bg-slate-900/30 text-slate-400';
    }
  };

  return (
    <div className="space-y-6">
      <div className="section-header">FMEA: Failure Mode & Effects Analysis</div>

      <p className="text-slate-400 text-sm max-w-2xl">
        51 failure modes identified through Failure Mode and Effects Analysis (FMEA). Risk Priority
        Numbers (RPN) reduced by 90% average through Poka-Yoke guards and runtime validation.
      </p>

      {/* Summary Metrics */}
      <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
        <div className="metric-card">
          <div className="metric-label">Total RPN Before</div>
          <div className="metric-value">{totalBefore.toLocaleString()}</div>
        </div>
        <div className="metric-card">
          <div className="metric-label">Total RPN After</div>
          <div className="metric-value">{totalAfter.toLocaleString()}</div>
        </div>
        <div className="metric-card">
          <div className="metric-label">Overall Reduction</div>
          <div className="metric-value">{overallReduction}%</div>
        </div>
        <div className="metric-card">
          <div className="metric-label">Critical Risks</div>
          <div className="metric-value">0</div>
          <div className="text-sm text-green-400">All mitigated</div>
        </div>
      </div>

      {/* Risk Table */}
      <div className="chart-container">
        <h3 className="font-semibold mb-4 text-lg">Top 10 Risk Priority Numbers</h3>
        <div className="overflow-x-auto">
          <table>
            <thead>
              <tr>
                <th>Failure Mode</th>
                <th>RPN Before</th>
                <th>RPN After</th>
                <th>Reduction</th>
                <th>Mitigation Status</th>
              </tr>
            </thead>
            <tbody>
              {fmeaData.map((item, idx) => (
                <tr key={idx}>
                  <td className="font-semibold">{item.riskName}</td>
                  <td>
                    <span className="text-red-400 font-semibold">{item.rpnBefore}</span>
                  </td>
                  <td>
                    <span className="text-cyan-400 font-semibold">{item.rpnAfter}</span>
                  </td>
                  <td>
                    <span className="text-green-400 font-semibold">{item.reduction}</span>
                  </td>
                  <td>
                    <span
                      className={`px-3 py-1 text-xs rounded font-semibold ${getStatusColor(item.status)}`}
                    >
                      {item.status}
                    </span>
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      </div>

      {/* Mitigation Strategies */}
      <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
        <div className="chart-container">
          <h3 className="font-semibold mb-4 text-lg">Parse-Time Guards (Zod Validation)</h3>
          <ul className="space-y-2 text-sm text-slate-400">
            <li className="flex gap-2">
              <span className="text-green-400 font-bold">✓</span>
              <span>Operator-Threshold mismatch prevention</span>
            </li>
            <li className="flex gap-2">
              <span className="text-green-400 font-bold">✓</span>
              <span>Cron expression strict validation</span>
            </li>
            <li className="flex gap-2">
              <span className="text-green-400 font-bold">✓</span>
              <span>Interval bounds (10ms-24h) enforcement</span>
            </li>
            <li className="flex gap-2">
              <span className="text-green-400 font-bold">✓</span>
              <span>Hook schema enforced at definition time</span>
            </li>
          </ul>
        </div>

        <div className="chart-container">
          <h3 className="font-semibold mb-4 text-lg">Runtime Guards (Execution-Time)</h3>
          <ul className="space-y-2 text-sm text-slate-400">
            <li className="flex gap-2">
              <span className="text-blue-400 font-bold">⚡</span>
              <span>Non-boolean validation return coercion</span>
            </li>
            <li className="flex gap-2">
              <span className="text-blue-400 font-bold">⚡</span>
              <span>Transform return type validation</span>
            </li>
            <li className="flex gap-2">
              <span className="text-blue-400 font-bold">⚡</span>
              <span>Stack trace preservation on errors</span>
            </li>
            <li className="flex gap-2">
              <span className="text-blue-400 font-bold">⚡</span>
              <span>3-failure circuit breaker pattern</span>
            </li>
          </ul>
        </div>
      </div>

      {/* Risk Categories */}
      <div className="chart-container">
        <h3 className="font-semibold mb-4 text-lg">Risk Categories (51 Total Failure Modes)</h3>
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4 text-sm">
          <div className="border border-slate-700 rounded p-3">
            <div className="font-semibold text-cyan-400 mb-1">Error Handling</div>
            <div className="text-slate-400">
              6 failure modes • Silent swallowing • Stack trace loss
            </div>
          </div>
          <div className="border border-slate-700 rounded p-3">
            <div className="font-semibold text-cyan-400 mb-1">Data Integrity</div>
            <div className="text-slate-400">
              8 failure modes • Pooled quad leaks • Mutation escape
            </div>
          </div>
          <div className="border border-slate-700 rounded p-3">
            <div className="font-semibold text-cyan-400 mb-1">Async/Timeout</div>
            <div className="text-slate-400">3 failure modes • Silent failures • Timeout hangs</div>
          </div>
          <div className="border border-slate-700 rounded p-3">
            <div className="font-semibold text-cyan-400 mb-1">Configuration</div>
            <div className="text-slate-400">8 failure modes • Invalid cron • Interval bounds</div>
          </div>
          <div className="border border-slate-700 rounded p-3">
            <div className="font-semibold text-cyan-400 mb-1">Concurrency</div>
            <div className="text-slate-400">
              6 failure modes • Recursive execution • Race conditions
            </div>
          </div>
          <div className="border border-slate-700 rounded p-3">
            <div className="font-semibold text-cyan-400 mb-1">Resource Management</div>
            <div className="text-slate-400">20 failure modes • Unbounded logs • Memory leaks</div>
          </div>
        </div>
      </div>
    </div>
  );
}
