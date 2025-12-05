'use client';

import { useMemo } from 'react';
import { Bar } from 'react-chartjs-2';
import {
  Chart as ChartJS,
  CategoryScale,
  LinearScale,
  BarElement,
  Title,
  Tooltip,
  Legend,
} from 'chart.js';

ChartJS.register(CategoryScale, LinearScale, BarElement, Title, Tooltip, Legend);

export default function FMEADashboard({ data }) {
  const riskChartData = useMemo(() => {
    const risks = data.fmea.topRisks;
    return {
      labels: risks.map((r) => r.name),
      datasets: [
        {
          label: 'RPN Before Mitigation',
          data: risks.map((r) => r.before),
          backgroundColor: 'rgba(239, 68, 68, 0.7)',
          borderColor: 'rgb(239, 68, 68)',
          borderWidth: 2,
        },
        {
          label: 'RPN After Mitigation',
          data: risks.map((r) => r.after),
          backgroundColor: 'rgba(34, 197, 94, 0.7)',
          borderColor: 'rgb(34, 197, 94)',
          borderWidth: 2,
        },
      ],
    };
  }, [data]);

  const options = {
    responsive: true,
    maintainAspectRatio: true,
    indexAxis: 'y',
    plugins: {
      legend: {
        labels: { color: '#cbd5e1', font: { size: 12 } },
      },
      tooltip: {
        callbacks: {
          label: (context) => `RPN: ${context.parsed.x}`,
        },
      },
    },
    scales: {
      x: {
        ticks: { color: '#94a3b8' },
        grid: { color: 'rgba(148, 163, 184, 0.1)' },
      },
      y: {
        ticks: { color: '#94a3b8' },
        grid: { display: false },
      },
    },
  };

  return (
    <div className="space-y-6">
      <div className="section-header">FMEA Risk Analysis</div>

      {/* Summary Stats */}
      <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
        <div className="card">
          <div className="text-xs text-slate-400 uppercase">Total Failure Modes</div>
          <div className="text-3xl font-bold text-cyan-400 mt-2">{data.fmea.totalFailureModes}</div>
          <div className="text-xs text-slate-500 mt-1">Identified and analyzed</div>
        </div>

        <div className="card">
          <div className="text-xs text-slate-400 uppercase">RPN Reduction</div>
          <div className="text-3xl font-bold text-green-400 mt-2">{data.fmea.riskReduction}%</div>
          <div className="text-xs text-slate-500 mt-1">Average risk decrease</div>
        </div>

        <div className="card">
          <div className="text-xs text-slate-400 uppercase">Risk Categories</div>
          <div className="text-3xl font-bold text-purple-400 mt-2">{data.fmea.categories}</div>
          <div className="text-xs text-slate-500 mt-1">Mitigation strategies applied</div>
        </div>
      </div>

      {/* Risk Chart */}
      <div className="card">
        <h3 className="card-header">Top 5 Risk Priority Numbers (RPN)</h3>
        <div className="relative h-80">
          <Bar data={riskChartData} options={options} />
        </div>
      </div>

      {/* Detailed Risk Table */}
      <div className="card">
        <h3 className="card-header">Failure Mode Risk Assessment</h3>
        <div className="overflow-x-auto">
          <table>
            <thead>
              <tr>
                <th>ID</th>
                <th>Failure Mode</th>
                <th>RPN Before</th>
                <th>RPN After</th>
                <th>Reduction</th>
                <th>Mitigation Status</th>
              </tr>
            </thead>
            <tbody>
              {data.fmea.topRisks.map((risk) => {
                const reduction = (((risk.before - risk.after) / risk.before) * 100).toFixed(0);
                return (
                  <tr key={risk.id}>
                    <td className="font-mono font-bold text-cyan-400">{risk.id}</td>
                    <td className="font-semibold">{risk.name}</td>
                    <td className="text-red-400 font-bold">{risk.before}</td>
                    <td className="text-green-400 font-bold">{risk.after}</td>
                    <td className="text-green-400 font-semibold">{reduction}%</td>
                    <td>
                      <span className="metric-badge success">✓ Implemented</span>
                    </td>
                  </tr>
                );
              })}
            </tbody>
          </table>
        </div>
      </div>

      {/* Mitigation Strategies */}
      <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
        <div className="card">
          <h3 className="card-header">Parse-Time Mitigations</h3>
          <ul className="space-y-2 text-sm">
            <li className="flex gap-2">
              <span className="inline-block w-1.5 h-1.5 rounded-full bg-cyan-400 mt-1.5"></span>
              <div>
                <div className="font-semibold text-slate-200">Operator-Threshold Validation</div>
                <div className="text-xs text-slate-400">
                  Prevent invalid operator/threshold combinations
                </div>
              </div>
            </li>
            <li className="flex gap-2">
              <span className="inline-block w-1.5 h-1.5 rounded-full bg-cyan-400 mt-1.5"></span>
              <div>
                <div className="font-semibold text-slate-200">Cron Expression Validation</div>
                <div className="text-xs text-slate-400">
                  Strict validation of scheduling expressions
                </div>
              </div>
            </li>
            <li className="flex gap-2">
              <span className="inline-block w-1.5 h-1.5 rounded-full bg-cyan-400 mt-1.5"></span>
              <div>
                <div className="font-semibold text-slate-200">Interval Bounds Validation</div>
                <div className="text-xs text-slate-400">
                  Enforce safe timing constraints (10ms - 24hrs)
                </div>
              </div>
            </li>
          </ul>
        </div>

        <div className="card">
          <h3 className="card-header">Runtime Guards</h3>
          <ul className="space-y-2 text-sm">
            <li className="flex gap-2">
              <span className="inline-block w-1.5 h-1.5 rounded-full bg-green-400 mt-1.5"></span>
              <div>
                <div className="font-semibold text-slate-200">Non-Boolean Validation Guard</div>
                <div className="text-xs text-slate-400">
                  Coerce and warn on non-boolean validation returns
                </div>
              </div>
            </li>
            <li className="flex gap-2">
              <span className="inline-block w-1.5 h-1.5 rounded-full bg-green-400 mt-1.5"></span>
              <div>
                <div className="font-semibold text-slate-200">Recursive Execution Guard</div>
                <div className="text-xs text-slate-400">Prevent nested hook executions</div>
              </div>
            </li>
            <li className="flex gap-2">
              <span className="inline-block w-1.5 h-1.5 rounded-full bg-green-400 mt-1.5"></span>
              <div>
                <div className="font-semibold text-slate-200">Stack Trace Preservation</div>
                <div className="text-xs text-slate-400">
                  Maintain error context through hook chains
                </div>
              </div>
            </li>
          </ul>
        </div>
      </div>
    </div>
  );
}
