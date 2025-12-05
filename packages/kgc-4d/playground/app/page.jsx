'use client';

import { useState } from 'react';
import {
  UniverseExplorer,
  EventTimeline,
  EntityEditor,
  EntityList,
  ConnectionStatus,
  ShardViewer,
} from '../components/index.mjs';

export default function PlaygroundPage() {
  const [selectedEntity, setSelectedEntity] = useState('http://example.org/project/alpha');
  const [activeTab, setActiveTab] = useState('explorer');

  const tabs = [
    { id: 'explorer', label: '4D Explorer' },
    { id: 'entities', label: 'Entities' },
    { id: 'shard', label: 'Shard Data' },
    { id: 'docs', label: 'Documentation' },
  ];

  return (
    <div className="space-y-8">
      {/* Hero Section */}
      <div className="bg-gradient-to-r from-slate-900 via-slate-800 to-slate-900 border border-slate-800 rounded-xl p-8 space-y-4">
        <div className="flex items-start justify-between">
          <div className="space-y-2">
            <h2 className="text-3xl font-bold bg-gradient-to-r from-universe-400 via-tether-400 to-shard-400 bg-clip-text text-transparent">
              Shard-Based Architecture
            </h2>
            <p className="text-slate-400 max-w-2xl">
              Experience the 80/20 pivot: The Server holds the{' '}
              <span className="text-universe-400">Universe</span>, the Browser receives a{' '}
              <span className="text-shard-400">Shard</span>, and the{' '}
              <span className="text-tether-400">Tether</span> keeps them synchronized.
            </p>
          </div>
          <ConnectionStatus />
        </div>

        {/* Key Metrics */}
        <div className="grid grid-cols-2 md:grid-cols-4 gap-4 mt-6">
          <div className="bg-slate-900/50 border border-slate-700 rounded-lg p-3">
            <div className="text-xs text-slate-400 uppercase">Pattern</div>
            <div className="text-lg font-bold text-universe-400">Check-Out/In</div>
          </div>
          <div className="bg-slate-900/50 border border-slate-700 rounded-lg p-3">
            <div className="text-xs text-slate-400 uppercase">Server</div>
            <div className="text-lg font-bold text-universe-400">Authoritative</div>
          </div>
          <div className="bg-slate-900/50 border border-slate-700 rounded-lg p-3">
            <div className="text-xs text-slate-400 uppercase">Client</div>
            <div className="text-lg font-bold text-shard-400">Thin View</div>
          </div>
          <div className="bg-slate-900/50 border border-slate-700 rounded-lg p-3">
            <div className="text-xs text-slate-400 uppercase">Sync</div>
            <div className="text-lg font-bold text-tether-400">Real-Time SSE</div>
          </div>
        </div>
      </div>

      {/* Tab Navigation */}
      <div className="flex flex-wrap gap-2 bg-slate-900 border border-slate-800 rounded-lg p-1">
        {tabs.map((tab) => (
          <button
            key={tab.id}
            onClick={() => setActiveTab(tab.id)}
            className={`px-4 py-2 rounded-lg text-sm font-medium transition-colors ${
              activeTab === tab.id
                ? 'bg-universe-500 text-white'
                : 'text-slate-400 hover:text-slate-200 hover:bg-slate-800'
            }`}
          >
            {tab.label}
          </button>
        ))}
      </div>

      {/* Tab Content */}
      {activeTab === 'explorer' && (
        <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
          <div className="lg:col-span-2">
            <UniverseExplorer />
          </div>
          <div>
            <EventTimeline />
          </div>
        </div>
      )}

      {activeTab === 'entities' && (
        <div className="grid grid-cols-1 lg:grid-cols-4 gap-6">
          <div className="space-y-4">
            <EntityList
              typeUri="http://kgc.io/ontology/Project"
              selectedUri={selectedEntity}
              onSelect={setSelectedEntity}
            />
            <EntityList
              typeUri="http://kgc.io/ontology/Task"
              selectedUri={selectedEntity}
              onSelect={setSelectedEntity}
            />
            <EntityList
              typeUri="http://kgc.io/ontology/User"
              selectedUri={selectedEntity}
              onSelect={setSelectedEntity}
            />
          </div>
          <div className="lg:col-span-2">
            <EntityEditor entityUri={selectedEntity} />
          </div>
          <div>
            <EventTimeline />
          </div>
        </div>
      )}

      {activeTab === 'shard' && (
        <div className="space-y-6">
          <ShardViewer />
        </div>
      )}

      {activeTab === 'docs' && (
        <div className="bg-slate-900 border border-slate-800 rounded-xl p-8 space-y-6">
          <h2 className="text-2xl font-bold text-white">Shard-Based Architecture</h2>

          <div className="prose prose-invert max-w-none">
            <h3 className="text-lg font-semibold text-universe-400">The Divergence: Ideal vs. Real (80/20)</h3>
            <div className="overflow-x-auto">
              <table className="w-full text-sm">
                <thead>
                  <tr className="border-b border-slate-700">
                    <th className="text-left py-2 px-4 text-slate-400">Feature</th>
                    <th className="text-left py-2 px-4 text-slate-400">Ideal (Hyperdimensional)</th>
                    <th className="text-left py-2 px-4 text-slate-400">Real (80/20 MVP)</th>
                  </tr>
                </thead>
                <tbody className="text-slate-300">
                  <tr className="border-b border-slate-800">
                    <td className="py-2 px-4 font-medium">Topology</td>
                    <td className="py-2 px-4">Holographic: Every node contains the whole</td>
                    <td className="py-2 px-4 text-tether-400">Projective: Server = Universe, Browser = Shard</td>
                  </tr>
                  <tr className="border-b border-slate-800">
                    <td className="py-2 px-4 font-medium">Storage</td>
                    <td className="py-2 px-4">Full Clone: Browser syncs entire Git repo</td>
                    <td className="py-2 px-4 text-tether-400">Cache: Browser stores only active Shard</td>
                  </tr>
                  <tr className="border-b border-slate-800">
                    <td className="py-2 px-4 font-medium">Latency</td>
                    <td className="py-2 px-4">Zero-Latency Local: All logic client-side</td>
                    <td className="py-2 px-4 text-tether-400">Networked: Logic (mu) runs on Server</td>
                  </tr>
                  <tr className="border-b border-slate-800">
                    <td className="py-2 px-4 font-medium">Consistency</td>
                    <td className="py-2 px-4">Eventual/Merge: CRDT/Vector Clocks</td>
                    <td className="py-2 px-4 text-tether-400">Authoritative: Server accepts/rejects Delta</td>
                  </tr>
                </tbody>
              </table>
            </div>

            <h3 className="text-lg font-semibold text-tether-400 mt-8">The Data Flow: Check-Out / Check-In</h3>
            <div className="bg-slate-800/50 rounded-lg p-4 font-mono text-sm space-y-4">
              <div>
                <div className="text-universe-400 font-semibold">Step 1: Check-Out (Projection)</div>
                <div className="text-slate-400 ml-4">
                  User opens Project Alpha → Server executes Temporal Query → Shard sent to Browser
                </div>
              </div>
              <div>
                <div className="text-shard-400 font-semibold">Step 2: Manipulation (Cognition)</div>
                <div className="text-slate-400 ml-4">
                  User changes budget → Browser updates local Shard → Delta sent to Server
                </div>
              </div>
              <div>
                <div className="text-tether-400 font-semibold">Step 3: Reconciliation (The Law)</div>
                <div className="text-slate-400 ml-4">
                  Server runs Knowledge Hooks (mu) → Validate → ACK or REJECT
                </div>
              </div>
              <div>
                <div className="text-green-400 font-semibold">Step 4: Alignment</div>
                <div className="text-slate-400 ml-4">
                  On ACK: Update with official t_ns | On REJECT: Rollback to Server state
                </div>
              </div>
            </div>

            <h3 className="text-lg font-semibold text-shard-400 mt-8">API Endpoints</h3>
            <ul className="space-y-2 text-slate-300">
              <li>
                <code className="bg-slate-800 px-2 py-1 rounded text-universe-400">GET /api/shard</code>
                <span className="ml-2">— Check-Out: Project a filtered Shard from Universe</span>
              </li>
              <li>
                <code className="bg-slate-800 px-2 py-1 rounded text-universe-400">POST /api/delta</code>
                <span className="ml-2">— Check-In: Submit Delta for validation and commit</span>
              </li>
              <li>
                <code className="bg-slate-800 px-2 py-1 rounded text-universe-400">GET /api/tether</code>
                <span className="ml-2">— SSE stream for real-time Shard updates</span>
              </li>
            </ul>
          </div>
        </div>
      )}
    </div>
  );
}
