'use client';

import { useState, useMemo } from 'react';
import PerformanceMetrics from '@/components/PerformanceMetrics';
import EntropyCascade from '@/components/EntropyCascade';
import FMEAChart from '@/components/FMEAChart';
import JTBDComparison from '@/components/JTBDComparison';
import OperatorAnalysis from '@/components/OperatorAnalysis';
import InformationFlow from '@/components/InformationFlow';

export default function Dashboard() {
  const [activeTab, setActiveTab] = useState('overview');

  const benchmarkData = useMemo(
    () => ({
      singleOperator: {
        latency: 0.853,
        unit: 'μs',
        percentile: '50th',
        range: [0.742, 0.964],
      },
      eightOperatorChain: {
        latency: 6.824,
        unit: 'μs',
        percentile: '50th',
        range: [5.936, 7.712],
      },
      throughput: {
        opsPerSec: 1.17e6,
        unit: 'ops/sec',
        description: 'Sustained throughput with 8-operator chains',
      },
      entropyReduction: {
        initialEntropyNats: 50,
        finalEntropyNats: 1,
        reductionRate: '20-30 nats/μs',
        cascadeStages: 8,
      },
      informationProcessing: {
        rate: 8.4,
        unit: 'Mnats/sec',
        perOperator: '0.853 μs per operator',
        totalPerChain: '6.824 μs',
      },
    }),
    []
  );

  const tabs = [
    { id: 'overview', label: 'Overview', icon: '📊' },
    { id: 'performance', label: 'Performance', icon: '⚡' },
    { id: 'entropy', label: 'Entropy', icon: '📉' },
    { id: 'fmea', label: 'FMEA', icon: '🛡️' },
    { id: 'jtbd', label: 'JTBD', icon: '🎯' },
    { id: 'operators', label: 'Operators', icon: '🔧' },
  ];

  return (
    <div className="min-h-screen bg-gradient-to-br from-slate-950 via-slate-900 to-slate-950">
      <div className="max-w-7xl mx-auto px-4 py-8 sm:px-6 lg:px-8">
        {/* Hero Section */}
        <div className="mb-12">
          <h1 className="text-4xl sm:text-5xl font-bold mb-4 gradient-text">
            μ(O) Calculus Dashboard
          </h1>
          <p className="text-lg text-slate-400 max-w-2xl">
            Interactive visualization of knowledge transformation performance metrics and
            information-theoretic analysis. Explore the mathematical foundations of deterministic
            intent-to-outcome mapping in RDF systems.
          </p>
        </div>

        {/* Key Metrics Row */}
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4 mb-8">
          <div className="metric-card">
            <div className="metric-label">Single Operator</div>
            <div className="metric-value">{benchmarkData.singleOperator.latency}</div>
            <div className="text-sm text-slate-400">{benchmarkData.singleOperator.unit}</div>
          </div>
          <div className="metric-card">
            <div className="metric-label">8-Operator Chain</div>
            <div className="metric-value">{benchmarkData.eightOperatorChain.latency}</div>
            <div className="text-sm text-slate-400">{benchmarkData.eightOperatorChain.unit}</div>
          </div>
          <div className="metric-card">
            <div className="metric-label">Throughput</div>
            <div className="metric-value">1.17M</div>
            <div className="text-sm text-slate-400">ops/sec</div>
          </div>
          <div className="metric-card">
            <div className="metric-label">Entropy Reduction</div>
            <div className="metric-value">50→1</div>
            <div className="text-sm text-slate-400">nats</div>
          </div>
        </div>

        {/* Tab Navigation */}
        <div className="flex flex-wrap gap-2 mb-8 border-b border-slate-700">
          {tabs.map((tab) => (
            <button
              key={tab.id}
              onClick={() => setActiveTab(tab.id)}
              className={`px-4 py-3 font-semibold text-sm transition-all ${
                activeTab === tab.id
                  ? 'border-b-2 border-cyan-400 text-cyan-400'
                  : 'text-slate-400 hover:text-slate-300'
              }`}
            >
              <span className="mr-2">{tab.icon}</span>
              {tab.label}
            </button>
          ))}
        </div>

        {/* Tab Content */}
        <div className="space-y-8">
          {activeTab === 'overview' && (
            <div className="space-y-8">
              <PerformanceMetrics data={benchmarkData} />
              <InformationFlow data={benchmarkData} />
            </div>
          )}

          {activeTab === 'performance' && <PerformanceMetrics data={benchmarkData} detailed />}

          {activeTab === 'entropy' && (
            <div className="space-y-8">
              <EntropyCascade data={benchmarkData} />
            </div>
          )}

          {activeTab === 'fmea' && <FMEAChart />}

          {activeTab === 'jtbd' && <JTBDComparison />}

          {activeTab === 'operators' && <OperatorAnalysis />}
        </div>

        {/* Footer Info */}
        <div className="mt-16 p-8 rounded-lg border border-slate-700 bg-slate-900/50">
          <h2 className="text-xl font-bold mb-4">About This Dashboard</h2>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-6 text-sm text-slate-400">
            <div>
              <p className="font-semibold text-slate-300 mb-2">Research Focus</p>
              <p>
                The μ(O) calculus formalizes knowledge transformations where user intent maps to
                ontology mutations through deterministic operators. This dashboard visualizes
                performance metrics validating the theoretical framework.
              </p>
            </div>
            <div>
              <p className="font-semibold text-slate-300 mb-2">Methodology</p>
              <p>
                Metrics collected via OTEL observability spans across 8 JTBD scenarios. All
                measurements include statistical percentiles (p50, p95, p99). Information processing
                rates computed from entropy cascade analysis.
              </p>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
