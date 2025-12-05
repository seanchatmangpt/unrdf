'use client';

import { useState, useMemo } from 'react';
import dynamic from 'next/dynamic';
import PerformanceMetricsChart from '../components/PerformanceMetricsChart';
import EntropyCascadeVisualization from '../components/EntropyCascadeVisualization';
import InformationFlowAnalysis from '../components/InformationFlowAnalysis';
import FMEADashboard from '../components/FMEADashboard';
import JTBDScenarios from '../components/JTBDScenarios';
import OperatorCardinality from '../components/OperatorCardinality';
import GeospatialVisualization from '../components/GeospatialVisualization';
import KnowledgeGraph from '../components/KnowledgeGraph';
import PlantUMLViewer from '../components/PlantUMLViewer';

// Dynamic import for 3D component to prevent SSR issues with Three.js
const EntropyVisualization3D = dynamic(() => import('../components/EntropyVisualization3D'), {
  ssr: false,
  loading: () => (
    <div className="card h-96 flex items-center justify-center">
      <div className="text-slate-400">Loading 3D visualization...</div>
    </div>
  ),
});

export default function Dashboard() {
  const [activeTab, setActiveTab] = useState('overview');

  const benchmarkData = useMemo(
    () => ({
      singleOperator: {
        latency: 0.853,
        range: [0.742, 0.964],
        unit: 'μs',
      },
      eightOperatorChain: {
        latency: 6.824,
        range: [5.891, 7.756],
        unit: 'μs',
      },
      throughput: {
        opsPerSec: 1.17e6,
        description: 'Sustained throughput across all operators',
      },
      informationProcessing: {
        rate: 8.4,
        perOperator: '1.05M',
        totalPerChain: '8.4M',
        unit: 'nats/sec',
      },
      entropyReduction: {
        initial: 50,
        final: 1,
        reductionRate: 0.0588,
        stages: [
          { operator: 'μ₁', reduction: 4.2, entropy: '50 → 45.8' },
          { operator: 'μ₂', reduction: 5.8, entropy: '45.8 → 40.0' },
          { operator: 'μ₃', reduction: 7.1, entropy: '40.0 → 32.9' },
          { operator: 'μ₄', reduction: 6.3, entropy: '32.9 → 26.6' },
          { operator: 'μ₅', reduction: 5.9, entropy: '26.6 → 20.4' },
          { operator: 'μ₆', reduction: 6.2, entropy: '20.4 → 14.2' },
          { operator: 'μ₇', reduction: 5.4, entropy: '14.2 → 8.0' },
          { operator: 'μ₈', reduction: 0.1, entropy: '8.0 → 1.0' },
        ],
      },
      fmea: {
        totalFailureModes: 51,
        riskReduction: 86,
        categories: 6,
        topRisks: [
          { id: 'FM-001', name: 'Silent Error Swallowing', before: 504, after: 50 },
          { id: 'FM-002', name: 'Audit Log Unbounded Growth', before: 448, after: 45 },
          { id: 'FM-003', name: 'Scheduler Error Swallowing', before: 432, after: 43 },
          { id: 'FM-004', name: 'Hook Validation Timeout', before: 384, after: 38 },
          { id: 'FM-005', name: 'Async Hook Silent Failure', before: 336, after: 34 },
        ],
      },
      jtbd: [
        { id: 'JTBD-1', name: 'Order Fulfillment', latency: 2.53, reduction: 48.2 },
        { id: 'JTBD-2', name: 'Recurring Purchase', latency: 3.12, reduction: 47.1 },
        { id: 'JTBD-3', name: 'Listing Compliance', latency: 2.89, reduction: 49.4 },
        { id: 'JTBD-5', name: 'Address Validation', latency: 1.87, reduction: 46.8 },
        { id: 'JTBD-6', name: 'Bulk Updates', latency: 18.4, reduction: 49.1 },
        { id: 'JTBD-7', name: 'Change Notifications', latency: 2.91, reduction: 48.3 },
        { id: 'JTBD-8', name: 'Account Consistency', latency: 3.45, reduction: 49.0 },
      ],
    }),
    []
  );

  const tabs = [
    { id: 'overview', label: 'Overview' },
    { id: 'performance', label: 'Performance' },
    { id: 'entropy', label: 'Entropy' },
    { id: 'entropy3d', label: '3D View' },
    { id: 'knowledge-graph', label: 'Knowledge Graph' },
    { id: 'diagrams', label: 'Diagrams' },
    { id: 'geospatial', label: 'Geospatial' },
    { id: 'fmea', label: 'FMEA' },
    { id: 'jtbd', label: 'JTBD' },
    { id: 'operators', label: 'Operators' },
  ];

  return (
    <div className="space-y-8">
      {/* Hero Section */}
      <div className="bg-gradient-to-r from-slate-800 to-slate-900 border border-slate-700 rounded-lg p-8 space-y-4">
        <h2 className="text-3xl font-bold text-cyan-400">Knowledge Hook Performance Analysis</h2>
        <p className="text-slate-300 max-w-2xl">
          Interactive visualization of μ(O) calculus research findings demonstrating deterministic
          intent-to-outcome transformation with 8 information-theoretic operators achieving 1.17M
          operations per second with entropy reduction from 50 to 1 nat.
        </p>
        <div className="grid grid-cols-2 md:grid-cols-4 gap-4 mt-6">
          <div className="bg-slate-900/50 border border-slate-700 rounded p-3">
            <div className="text-xs text-slate-400 uppercase">Single Op</div>
            <div className="text-xl font-bold text-cyan-400">0.853 μs</div>
          </div>
          <div className="bg-slate-900/50 border border-slate-700 rounded p-3">
            <div className="text-xs text-slate-400 uppercase">8-Op Chain</div>
            <div className="text-xl font-bold text-cyan-400">6.82 μs</div>
          </div>
          <div className="bg-slate-900/50 border border-slate-700 rounded p-3">
            <div className="text-xs text-slate-400 uppercase">Throughput</div>
            <div className="text-xl font-bold text-cyan-400">1.17M ops/s</div>
          </div>
          <div className="bg-slate-900/50 border border-slate-700 rounded p-3">
            <div className="text-xs text-slate-400 uppercase">Entropy Δ</div>
            <div className="text-xl font-bold text-cyan-400">50 → 1 nat</div>
          </div>
        </div>
      </div>

      {/* Tabs Navigation */}
      <div className="space-y-6">
        <div className="flex flex-wrap gap-2 bg-slate-900 border border-slate-800 rounded-lg p-1">
          {tabs.map((tab) => (
            <button
              key={tab.id}
              onClick={() => setActiveTab(tab.id)}
              className={`px-4 py-2 rounded text-sm font-medium transition-colors ${
                activeTab === tab.id
                  ? 'bg-cyan-500 text-slate-900'
                  : 'text-slate-400 hover:text-slate-200'
              }`}
            >
              {tab.label}
            </button>
          ))}
        </div>

        {/* Tab Content */}
        <div className="space-y-6">
          {activeTab === 'overview' && (
            <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
              <PerformanceMetricsChart data={benchmarkData} />
              <InformationFlowAnalysis data={benchmarkData} />
            </div>
          )}

          {activeTab === 'performance' && <PerformanceMetricsChart data={benchmarkData} detailed />}

          {activeTab === 'entropy' && <EntropyCascadeVisualization data={benchmarkData} />}

          {activeTab === 'entropy3d' && <EntropyVisualization3D data={benchmarkData} />}

          {activeTab === 'knowledge-graph' && <KnowledgeGraph data={benchmarkData} />}

          {activeTab === 'diagrams' && <PlantUMLViewer data={benchmarkData} />}

          {activeTab === 'geospatial' && <GeospatialVisualization data={benchmarkData} />}

          {activeTab === 'fmea' && <FMEADashboard data={benchmarkData} />}

          {activeTab === 'jtbd' && <JTBDScenarios data={benchmarkData} />}

          {activeTab === 'operators' && <OperatorCardinality data={benchmarkData} />}
        </div>
      </div>
    </div>
  );
}
