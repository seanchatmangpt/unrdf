"use client";

import { useState } from "react";
import { Button } from "@/components/ui/button";
import { Badge } from "@/components/ui/badge";

export function FederationDemo() {
  const [stores, setStores] = useState([
    { id: "store-1", name: "Primary Store", status: "healthy", latency: 12, quads: 45000 },
    { id: "store-2", name: "Replica EU", status: "healthy", latency: 45, quads: 45000 },
    { id: "store-3", name: "Replica US", status: "syncing", latency: 28, quads: 44850 }
  ]);
  const [queryResult, setQueryResult] = useState(null);
  const [loading, setLoading] = useState(false);

  const runFederatedQuery = async () => {
    setLoading(true);
    await new Promise(r => setTimeout(r, 1200));
    setQueryResult({
      totalResults: 1247,
      storeResults: [
        { store: "store-1", results: 423, latency: 45 },
        { store: "store-2", results: 412, latency: 89 },
        { store: "store-3", results: 412, latency: 67 }
      ],
      executionTime: 201
    });
    setLoading(false);
  };

  return (
    <div className="space-y-6">
      {/* Store Status */}
      <div className="grid grid-cols-3 gap-4">
        {stores.map(store => (
          <div key={store.id} className="bg-slate-900 rounded-lg p-4">
            <div className="flex items-center justify-between mb-2">
              <span className="font-medium text-slate-300">{store.name}</span>
              <Badge variant={store.status === "healthy" ? "success" : "warning"}>
                {store.status}
              </Badge>
            </div>
            <div className="text-xs text-slate-400 space-y-1">
              <div className="flex justify-between">
                <span>Latency:</span>
                <span>{store.latency}ms</span>
              </div>
              <div className="flex justify-between">
                <span>Quads:</span>
                <span>{store.quads.toLocaleString()}</span>
              </div>
            </div>
          </div>
        ))}
      </div>

      {/* Query Controls */}
      <div className="flex items-center gap-4">
        <Button onClick={runFederatedQuery} disabled={loading}>
          {loading ? "Querying..." : "Run Federated Query"}
        </Button>
        <span className="text-sm text-slate-400">
          Queries all 3 stores simultaneously
        </span>
      </div>

      {/* Query Results */}
      {queryResult && (
        <div className="bg-slate-900 rounded-lg p-4">
          <div className="text-sm font-medium text-slate-300 mb-3">
            Federated Query Results
          </div>
          <div className="grid grid-cols-4 gap-4 mb-4">
            <div className="text-center">
              <div className="text-xl font-bold text-blue-400">{queryResult.totalResults}</div>
              <div className="text-xs text-slate-400">Total Results</div>
            </div>
            <div className="text-center">
              <div className="text-xl font-bold text-green-400">{queryResult.executionTime}ms</div>
              <div className="text-xs text-slate-400">Execution Time</div>
            </div>
            <div className="text-center">
              <div className="text-xl font-bold text-purple-400">3</div>
              <div className="text-xs text-slate-400">Stores Queried</div>
            </div>
            <div className="text-center">
              <div className="text-xl font-bold text-orange-400">Parallel</div>
              <div className="text-xs text-slate-400">Strategy</div>
            </div>
          </div>
          <div className="space-y-2">
            {queryResult.storeResults.map((sr, i) => (
              <div key={i} className="flex items-center justify-between bg-slate-800 p-2 rounded">
                <span className="text-sm text-slate-300">{stores.find(s => s.id === sr.store)?.name}</span>
                <div className="flex items-center gap-4 text-xs">
                  <span className="text-slate-400">{sr.results} results</span>
                  <span className="text-slate-500">{sr.latency}ms</span>
                </div>
              </div>
            ))}
          </div>
        </div>
      )}

      {/* Additional Federation Hooks */}
      <div className="grid grid-cols-2 gap-4">
        {/* useConsensusManager */}
        <div className="bg-slate-900 rounded-lg p-4">
          <div className="text-sm font-medium text-slate-300 mb-2">useConsensusManager</div>
          <div className="text-xs text-slate-400 mb-2">Distributed consensus for data conflicts</div>
          <div className="space-y-1 text-xs">
            <div className="flex justify-between">
              <span className="text-slate-500">Algorithm:</span>
              <span className="text-blue-400">Raft</span>
            </div>
            <div className="flex justify-between">
              <span className="text-slate-500">Leader:</span>
              <span className="text-green-400">store-1</span>
            </div>
            <div className="flex justify-between">
              <span className="text-slate-500">Term:</span>
              <span className="text-slate-300">42</span>
            </div>
          </div>
        </div>

        {/* useDistributedQuery */}
        <div className="bg-slate-900 rounded-lg p-4">
          <div className="text-sm font-medium text-slate-300 mb-2">useDistributedQuery</div>
          <div className="text-xs text-slate-400 mb-2">Query routing and optimization</div>
          <div className="space-y-1 text-xs">
            <div className="flex justify-between">
              <span className="text-slate-500">Strategy:</span>
              <Badge variant="outline">Scatter-Gather</Badge>
            </div>
            <div className="flex justify-between">
              <span className="text-slate-500">Partitions:</span>
              <span className="text-slate-300">3</span>
            </div>
          </div>
        </div>

        {/* useDataReplication */}
        <div className="bg-slate-900 rounded-lg p-4">
          <div className="text-sm font-medium text-slate-300 mb-2">useDataReplication</div>
          <div className="text-xs text-slate-400 mb-2">Async replication between stores</div>
          <div className="flex items-center gap-2">
            <Badge variant="success">Primary</Badge>
            <span className="text-slate-500">→</span>
            <Badge variant="secondary">2 Replicas</Badge>
          </div>
          <div className="text-xs text-slate-500 mt-2">Lag: 45ms</div>
        </div>

        {/* useFederationHealth */}
        <div className="bg-slate-900 rounded-lg p-4">
          <div className="text-sm font-medium text-slate-300 mb-2">useFederationHealth</div>
          <div className="text-xs text-slate-400 mb-2">Cluster health monitoring</div>
          <div className="grid grid-cols-3 gap-2 text-xs text-center">
            <div>
              <div className="w-3 h-3 rounded-full bg-green-500 mx-auto mb-1"></div>
              <span className="text-slate-500">Primary</span>
            </div>
            <div>
              <div className="w-3 h-3 rounded-full bg-green-500 mx-auto mb-1"></div>
              <span className="text-slate-500">EU</span>
            </div>
            <div>
              <div className="w-3 h-3 rounded-full bg-yellow-500 mx-auto mb-1"></div>
              <span className="text-slate-500">US</span>
            </div>
          </div>
        </div>
      </div>

      {/* Code Example */}
      <div className="bg-slate-900 rounded-lg p-4">
        <div className="text-xs text-slate-500 mb-2">Usage Example - All Federation Hooks</div>
        <pre className="text-xs text-slate-300 overflow-x-auto">
{`import {
  useFederatedSystem,
  useConsensusManager,
  useDistributedQuery,
  useDataReplication,
  useFederationHealth
} from 'unrdf/react-hooks';

// Basic federation
const { stores, executeQuery, health } = useFederatedSystem({
  stores: ['primary', 'replica-eu', 'replica-us'],
  strategy: 'parallel'
});

// Raft consensus for conflicts
const { leader, term, proposeChange, onLeaderChange } = useConsensusManager({
  algorithm: 'raft',
  electionTimeout: 5000
});

// Distributed query optimization
const { route, partitions, optimize } = useDistributedQuery({
  strategy: 'scatter-gather',
  timeout: 10000
});

// Async replication management
const { replicate, lag, status, pause, resume } = useDataReplication({
  mode: 'async',
  batchSize: 100
});

// Health monitoring
const { health, alerts, subscribe } = useFederationHealth();
subscribe((status) => console.log('Health changed:', status));`}
        </pre>
      </div>
    </div>
  );
}
