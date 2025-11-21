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

      {/* Code Example */}
      <div className="bg-slate-900 rounded-lg p-4">
        <div className="text-xs text-slate-500 mb-2">Usage Example</div>
        <pre className="text-xs text-slate-300 overflow-x-auto">
{`import { useFederatedSystem } from 'unrdf/react-hooks';

function FederatedSearch() {
  const { stores, executeQuery, health } = useFederatedSystem({
    stores: ['primary', 'replica-eu', 'replica-us'],
    strategy: 'parallel'
  });

  const search = async () => {
    const result = await executeQuery(
      'SELECT * WHERE { ?s foaf:name ?name }',
      { timeout: 5000 }
    );
    console.log(result.totalResults);
  };
}`}
        </pre>
      </div>
    </div>
  );
}
