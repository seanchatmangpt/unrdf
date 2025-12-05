'use client';

import { useMemo } from 'react';
import { Line } from 'react-chartjs-2';
import {
  Chart as ChartJS,
  CategoryScale,
  LinearScale,
  PointElement,
  LineElement,
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
  Title,
  Tooltip,
  Legend,
  Filler
);

export default function EntropyCascadeVisualization({ data }) {
  const chartData = useMemo(() => {
    const stages = data.entropyReduction.stages;
    return {
      labels: stages.map((s) => s.operator),
      datasets: [
        {
          label: 'Entropy (nats)',
          data: [
            50,
            50 - stages[0].reduction,
            50 - stages[0].reduction - stages[1].reduction,
            50 - stages.slice(0, 2).reduce((sum, s) => sum + s.reduction, 0) - stages[2].reduction,
            50 - stages.slice(0, 3).reduce((sum, s) => sum + s.reduction, 0) - stages[3].reduction,
            50 - stages.slice(0, 4).reduce((sum, s) => sum + s.reduction, 0) - stages[4].reduction,
            50 - stages.slice(0, 5).reduce((sum, s) => sum + s.reduction, 0) - stages[5].reduction,
            50 - stages.slice(0, 6).reduce((sum, s) => sum + s.reduction, 0) - stages[6].reduction,
            1,
          ],
          borderColor: 'rgb(6, 182, 212)',
          backgroundColor: 'rgba(6, 182, 212, 0.1)',
          borderWidth: 3,
          fill: true,
          tension: 0.4,
          pointRadius: 6,
          pointBackgroundColor: 'rgb(6, 182, 212)',
          pointBorderColor: 'rgba(6, 182, 212, 0.8)',
          pointBorderWidth: 2,
        },
        {
          label: 'Reduction per operator (nats)',
          data: stages.map((s) => s.reduction),
          borderColor: 'rgb(168, 85, 247)',
          backgroundColor: 'rgba(168, 85, 247, 0.1)',
          borderWidth: 2,
          yAxisID: 'y1',
          tension: 0.4,
          pointRadius: 4,
          pointBackgroundColor: 'rgb(168, 85, 247)',
        },
      ],
    };
  }, [data]);

  const options = {
    responsive: true,
    maintainAspectRatio: true,
    interaction: { mode: 'index', intersect: false },
    plugins: {
      legend: {
        labels: { color: '#cbd5e1', font: { size: 12 } },
      },
      tooltip: {
        callbacks: {
          label: (context) => {
            if (context.dataset.yAxisID === 'y1') {
              return `Reduction: ${context.parsed.y.toFixed(1)} nats`;
            }
            return `Entropy: ${context.parsed.y.toFixed(1)} nats`;
          },
        },
      },
    },
    scales: {
      y: {
        position: 'left',
        ticks: { color: '#94a3b8' },
        grid: { color: 'rgba(148, 163, 184, 0.1)' },
        title: { display: true, text: 'Entropy (nats)', color: '#cbd5e1' },
      },
      y1: {
        position: 'right',
        ticks: { color: '#94a3b8' },
        grid: { drawOnChartArea: false },
        title: { display: true, text: 'Reduction (nats)', color: '#cbd5e1' },
      },
      x: {
        ticks: { color: '#94a3b8' },
        grid: { color: 'rgba(148, 163, 184, 0.1)' },
      },
    },
  };

  return (
    <div className="space-y-6">
      <div className="section-header">Entropy Reduction Cascade</div>

      <div className="card">
        <div className="relative h-96">
          <Line data={chartData} options={options} />
        </div>
      </div>

      <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
        <div className="card">
          <h3 className="card-header">Stage-by-Stage Analysis</h3>
          <div className="overflow-x-auto">
            <table>
              <thead>
                <tr>
                  <th>Operator</th>
                  <th>Entropy In</th>
                  <th>Entropy Out</th>
                  <th>Reduction</th>
                </tr>
              </thead>
              <tbody>
                <tr>
                  <td colSpan="4" className="font-semibold text-xs text-slate-400 bg-slate-800">
                    Initial State
                  </td>
                </tr>
                {data.entropyReduction.stages.map((stage, idx) => (
                  <tr key={stage.operator}>
                    <td className="font-mono font-bold text-cyan-400">{stage.operator}</td>
                    <td className="font-semibold">{stage.entropy.split(' → ')[0]}</td>
                    <td className="font-semibold">{stage.entropy.split(' → ')[1]}</td>
                    <td className="font-semibold text-green-400">{stage.reduction} nats</td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        </div>

        <div className="card space-y-4">
          <h3 className="card-header">Key Insights</h3>

          <div className="bg-slate-800/50 rounded p-3 space-y-2">
            <div className="text-xs text-slate-400 uppercase font-semibold">Total Compression</div>
            <div className="text-3xl font-bold text-cyan-400">49 nats</div>
            <div className="text-xs text-slate-400">From 50 initial → 1 observable</div>
          </div>

          <div className="bg-slate-800/50 rounded p-3 space-y-2">
            <div className="text-xs text-slate-400 uppercase font-semibold">Average Reduction</div>
            <div className="text-3xl font-bold text-purple-400">6.1 nats</div>
            <div className="text-xs text-slate-400">Per operator (49 ÷ 8)</div>
          </div>

          <div className="bg-slate-800/50 rounded p-3 space-y-2">
            <div className="text-xs text-slate-400 uppercase font-semibold">Information Loss</div>
            <div className="text-2xl font-bold text-orange-400">98%</div>
            <div className="text-xs text-slate-400">
              Irreversibly lost to causally-unreachable space
            </div>
          </div>

          <p className="text-sm text-slate-400 border-l-2 border-cyan-500 pl-3">
            The entropy cascade demonstrates that transforming intent to observable outcome requires
            progressive information reduction. Some information is irreversibly lost, making opacity
            a mathematical inevitability.
          </p>
        </div>
      </div>
    </div>
  );
}
