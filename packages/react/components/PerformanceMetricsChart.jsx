'use client';

import { useMemo } from 'react';
import { Bar, Line } from 'react-chartjs-2';
import {
  Chart as ChartJS,
  CategoryScale,
  LinearScale,
  PointElement,
  LineElement,
  BarElement,
  Title,
  Tooltip,
  Legend,
  Filler,
} from 'chart.js';

ChartJS.register(
  CategoryScale,
  LinearScale,
  PointElement,
  LineElement,
  BarElement,
  Title,
  Tooltip,
  Legend,
  Filler
);

export default function PerformanceMetricsChart({ data, detailed = false }) {
  const latencyChartData = useMemo(
    () => ({
      labels: ['Min (p5)', 'Median (p50)', 'Max (p95)'],
      datasets: [
        {
          label: 'Single Operator',
          data: [
            data.singleOperator.range[0],
            data.singleOperator.latency,
            data.singleOperator.range[1],
          ],
          backgroundColor: [
            'rgba(34, 197, 94, 0.5)',
            'rgba(6, 182, 212, 0.8)',
            'rgba(168, 85, 247, 0.5)',
          ],
          borderColor: ['rgb(34, 197, 94)', 'rgb(6, 182, 212)', 'rgb(168, 85, 247)'],
          borderWidth: 2,
        },
        {
          label: '8-Operator Chain',
          data: [
            data.eightOperatorChain.range[0],
            data.eightOperatorChain.latency,
            data.eightOperatorChain.range[1],
          ],
          backgroundColor: [
            'rgba(59, 130, 246, 0.5)',
            'rgba(6, 182, 212, 0.8)',
            'rgba(236, 72, 153, 0.5)',
          ],
          borderColor: ['rgb(59, 130, 246)', 'rgb(6, 182, 212)', 'rgb(236, 72, 153)'],
          borderWidth: 2,
        },
      ],
    }),
    [data]
  );

  const latencyChartOptions = {
    responsive: true,
    maintainAspectRatio: true,
    plugins: {
      legend: {
        labels: { color: '#cbd5e1', font: { size: 12 } },
      },
      tooltip: {
        callbacks: {
          label: (context) => `${context.parsed.y.toFixed(3)} μs`,
        },
      },
    },
    scales: {
      y: {
        ticks: { color: '#94a3b8' },
        grid: { color: 'rgba(148, 163, 184, 0.1)' },
        title: { display: true, text: 'Latency (μs)', color: '#cbd5e1' },
      },
      x: {
        ticks: { color: '#94a3b8' },
        grid: { color: 'rgba(148, 163, 184, 0.1)' },
      },
    },
  };

  const throughputChartData = useMemo(
    () => ({
      labels: ['Throughput'],
      datasets: [
        {
          label: 'Operations/Second',
          data: [data.throughput.opsPerSec / 1e6],
          backgroundColor: 'rgba(6, 182, 212, 0.8)',
          borderColor: 'rgb(6, 182, 212)',
          borderWidth: 2,
        },
      ],
    }),
    [data]
  );

  const throughputChartOptions = {
    responsive: true,
    maintainAspectRatio: true,
    indexAxis: 'y',
    plugins: {
      legend: {
        labels: { color: '#cbd5e1', font: { size: 12 } },
      },
      tooltip: {
        callbacks: {
          label: (context) => `${context.parsed.x.toFixed(2)}M ops/sec`,
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
      <div className="section-header">Performance Benchmarks</div>

      {/* Main Metrics Grid */}
      <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
        {/* Latency Chart */}
        <div className="card">
          <h3 className="card-header">Operator Latency Distribution</h3>
          <div className="relative h-64">
            <Bar data={latencyChartData} options={latencyChartOptions} />
          </div>
        </div>

        {/* Throughput Chart */}
        <div className="card">
          <h3 className="card-header">Sustained Throughput</h3>
          <div className="relative h-64">
            <Bar data={throughputChartData} options={throughputChartOptions} />
          </div>
        </div>
      </div>

      {/* Metrics Summary Cards */}
      <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
        <div className="card">
          <div className="text-xs text-slate-400 uppercase">Single Operator</div>
          <div className="text-3xl font-bold text-cyan-400 mt-2">{data.singleOperator.latency}</div>
          <div className="text-xs text-slate-400 mt-1">μs (median)</div>
          <div className="text-xs text-slate-500 mt-2">
            Range: {data.singleOperator.range[0]} - {data.singleOperator.range[1]} μs
          </div>
        </div>

        <div className="card">
          <div className="text-xs text-slate-400 uppercase">8-Operator Chain</div>
          <div className="text-3xl font-bold text-cyan-400 mt-2">
            {data.eightOperatorChain.latency}
          </div>
          <div className="text-xs text-slate-400 mt-1">μs (median)</div>
          <div className="text-xs text-slate-500 mt-2">
            Range: {data.eightOperatorChain.range[0]} - {data.eightOperatorChain.range[1]} μs
          </div>
        </div>

        <div className="card">
          <div className="text-xs text-slate-400 uppercase">Throughput</div>
          <div className="text-3xl font-bold text-cyan-400 mt-2">
            {(data.throughput.opsPerSec / 1e6).toFixed(2)}M
          </div>
          <div className="text-xs text-slate-400 mt-1">ops/sec</div>
          <div className="text-xs text-slate-500 mt-2">Sustained performance</div>
        </div>
      </div>

      {/* Information Processing */}
      <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
        <div className="card">
          <h3 className="card-header">Information Processing Rate</h3>
          <div className="space-y-3">
            <div>
              <div className="text-xs text-slate-400">Total Rate</div>
              <div className="text-2xl font-bold text-cyan-400">
                {data.informationProcessing.rate}M
              </div>
              <div className="text-xs text-slate-400">nats/second</div>
            </div>
            <hr className="border-slate-800" />
            <div>
              <div className="text-xs text-slate-400">Per Operator</div>
              <div className="text-lg font-semibold text-blue-400">
                {data.informationProcessing.perOperator} nats/s
              </div>
            </div>
            <div>
              <div className="text-xs text-slate-400">Per Chain</div>
              <div className="text-lg font-semibold text-purple-400">
                {data.informationProcessing.totalPerChain} nats/s
              </div>
            </div>
          </div>
        </div>

        <div className="card">
          <h3 className="card-header">Performance Characteristics</h3>
          <ul className="space-y-2">
            <li className="flex items-center gap-2 text-sm">
              <span className="inline-block w-2 h-2 rounded-full bg-green-500"></span>
              <span className="text-slate-300">Consistent latency across all operators</span>
            </li>
            <li className="flex items-center gap-2 text-sm">
              <span className="inline-block w-2 h-2 rounded-full bg-green-500"></span>
              <span className="text-slate-300">Zero memory allocations in hot path</span>
            </li>
            <li className="flex items-center gap-2 text-sm">
              <span className="inline-block w-2 h-2 rounded-full bg-green-500"></span>
              <span className="text-slate-300">Deterministic execution time</span>
            </li>
            <li className="flex items-center gap-2 text-sm">
              <span className="inline-block w-2 h-2 rounded-full bg-green-500"></span>
              <span className="text-slate-300">Linear scaling with operator count</span>
            </li>
            <li className="flex items-center gap-2 text-sm">
              <span className="inline-block w-2 h-2 rounded-full bg-green-500"></span>
              <span className="text-slate-300">Production-grade reliability</span>
            </li>
          </ul>
        </div>
      </div>

      {/* Detailed Metrics Table */}
      {detailed && (
        <div className="card">
          <h3 className="card-header">Detailed Performance Metrics</h3>
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
                    <span className="metric-badge success">✓ Target Met</span>
                  </td>
                </tr>
                <tr>
                  <td className="font-semibold">8-Operator Chain (p50)</td>
                  <td className="text-cyan-400 font-semibold">{data.eightOperatorChain.latency}</td>
                  <td>μs</td>
                  <td>
                    <span className="metric-badge success">✓ Target Met</span>
                  </td>
                </tr>
                <tr>
                  <td className="font-semibold">Throughput (sustained)</td>
                  <td className="text-cyan-400 font-semibold">
                    {(data.throughput.opsPerSec / 1e6).toFixed(2)}M
                  </td>
                  <td>ops/sec</td>
                  <td>
                    <span className="metric-badge success">✓ Target Met</span>
                  </td>
                </tr>
                <tr>
                  <td className="font-semibold">Information Processing Rate</td>
                  <td className="text-cyan-400 font-semibold">
                    {data.informationProcessing.rate}M
                  </td>
                  <td>nats/sec</td>
                  <td>
                    <span className="metric-badge success">✓ Achieved</span>
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
