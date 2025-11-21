"use client";

import { useState } from "react";
import { Button } from "@/components/ui/button";
import { Badge } from "@/components/ui/badge";

export function CompositionDemo() {
  const [preset, setPreset] = useState("basic");
  const [isOnline, setIsOnline] = useState(true);
  const [pendingCount, setPendingCount] = useState(0);
  const [lastSynced, setLastSynced] = useState(null);
  const [data, setData] = useState([]);
  const [isLive, setIsLive] = useState(false);

  const presets = {
    basic: { realtime: false, recovery: false, errorBoundary: true },
    realtime: { realtime: true, recovery: false, errorBoundary: true },
    resilient: { realtime: false, recovery: true, errorBoundary: true },
    full: { realtime: true, recovery: true, errorBoundary: true }
  };

  const simulateOfflineInsert = () => {
    setData(prev => [...prev, { id: Date.now(), name: `Item-${Date.now()}`, synced: isOnline }]);
    if (!isOnline) {
      setPendingCount(prev => prev + 1);
    }
  };

  const toggleOnline = () => {
    setIsOnline(!isOnline);
    if (!isOnline) {
      // Coming back online - sync
      setTimeout(() => {
        setPendingCount(0);
        setLastSynced(new Date());
        setData(prev => prev.map(d => ({ ...d, synced: true })));
      }, 500);
    }
  };

  const toggleLive = () => {
    setIsLive(!isLive);
  };

  return (
    <div className="space-y-6">
      {/* Preset Selector */}
      <div className="space-y-2">
        <label className="text-sm font-medium text-slate-300">Stack Preset</label>
        <div className="flex gap-2">
          {Object.keys(presets).map(p => (
            <Button
              key={p}
              variant={preset === p ? "default" : "outline"}
              onClick={() => setPreset(p)}
              className="capitalize"
            >
              {p}
            </Button>
          ))}
        </div>
        <div className="flex gap-2 mt-2">
          <Badge variant={presets[preset].realtime ? "success" : "secondary"}>
            Realtime: {presets[preset].realtime ? "ON" : "OFF"}
          </Badge>
          <Badge variant={presets[preset].recovery ? "success" : "secondary"}>
            Recovery: {presets[preset].recovery ? "ON" : "OFF"}
          </Badge>
          <Badge variant={presets[preset].errorBoundary ? "success" : "secondary"}>
            ErrorBoundary: {presets[preset].errorBoundary ? "ON" : "OFF"}
          </Badge>
        </div>
      </div>

      {/* Offline Store Demo */}
      <div className="bg-slate-900 rounded-lg p-4">
        <div className="text-sm font-medium text-slate-300 mb-3">
          Offline-First Store Demo
        </div>
        <div className="flex items-center gap-4 mb-4">
          <Button
            variant={isOnline ? "default" : "destructive"}
            onClick={toggleOnline}
          >
            {isOnline ? "Go Offline" : "Go Online"}
          </Button>
          <Badge variant={isOnline ? "success" : "destructive"}>
            {isOnline ? "ONLINE" : "OFFLINE"}
          </Badge>
          {pendingCount > 0 && (
            <Badge variant="warning">
              {pendingCount} pending sync
            </Badge>
          )}
          {lastSynced && (
            <span className="text-xs text-slate-400">
              Last synced: {lastSynced.toLocaleTimeString()}
            </span>
          )}
        </div>

        <div className="flex gap-2 mb-4">
          <Button onClick={simulateOfflineInsert} variant="secondary">
            Insert Item (works offline)
          </Button>
          {preset === "realtime" || preset === "full" ? (
            <Button onClick={toggleLive} variant={isLive ? "destructive" : "outline"}>
              {isLive ? "Stop Live" : "Start Live"}
            </Button>
          ) : null}
        </div>

        <div className="bg-slate-800 rounded p-3 max-h-40 overflow-y-auto">
          {data.length === 0 ? (
            <div className="text-center text-slate-500 py-4">
              No items yet. Click "Insert Item" to add data.
            </div>
          ) : (
            <div className="space-y-2">
              {data.map(item => (
                <div key={item.id} className="flex items-center justify-between text-sm">
                  <span className="text-slate-300">{item.name}</span>
                  <Badge variant={item.synced ? "success" : "warning"} className="text-xs">
                    {item.synced ? "Synced" : "Pending"}
                  </Badge>
                </div>
              ))}
            </div>
          )}
        </div>
      </div>

      {/* Stack Variants */}
      <div className="grid grid-cols-3 gap-4">
        {/* useCRUDStack */}
        <div className="bg-slate-900 rounded-lg p-4">
          <div className="text-sm font-medium text-slate-300 mb-2">useCRUDStack</div>
          <div className="text-xs text-slate-400 mb-2">Optimized for data entry apps</div>
          <div className="space-y-1 text-xs">
            <div className="flex items-center gap-2">
              <Badge variant="success">✓</Badge>
              <span className="text-slate-300">Batch insert/update</span>
            </div>
            <div className="flex items-center gap-2">
              <Badge variant="success">✓</Badge>
              <span className="text-slate-300">Optimistic updates</span>
            </div>
            <div className="flex items-center gap-2">
              <Badge variant="success">✓</Badge>
              <span className="text-slate-300">Undo/Redo</span>
            </div>
          </div>
        </div>

        {/* useDashboardStack */}
        <div className="bg-slate-900 rounded-lg p-4">
          <div className="text-sm font-medium text-slate-300 mb-2">useDashboardStack</div>
          <div className="text-xs text-slate-400 mb-2">Real-time analytics dashboards</div>
          <div className="space-y-1 text-xs">
            <div className="flex items-center gap-2">
              <Badge variant="success">✓</Badge>
              <span className="text-slate-300">Live change feed</span>
            </div>
            <div className="flex items-center gap-2">
              <Badge variant="success">✓</Badge>
              <span className="text-slate-300">Aggregation queries</span>
            </div>
            <div className="flex items-center gap-2">
              <Badge variant="success">✓</Badge>
              <span className="text-slate-300">Auto-refresh</span>
            </div>
          </div>
        </div>

        {/* useProductionStack */}
        <div className="bg-slate-900 rounded-lg p-4">
          <div className="text-sm font-medium text-slate-300 mb-2">useProductionStack</div>
          <div className="text-xs text-slate-400 mb-2">Full production setup</div>
          <div className="space-y-1 text-xs">
            <div className="flex items-center gap-2">
              <Badge variant="success">✓</Badge>
              <span className="text-slate-300">Error boundaries</span>
            </div>
            <div className="flex items-center gap-2">
              <Badge variant="success">✓</Badge>
              <span className="text-slate-300">Auto-recovery</span>
            </div>
            <div className="flex items-center gap-2">
              <Badge variant="success">✓</Badge>
              <span className="text-slate-300">Telemetry</span>
            </div>
          </div>
        </div>
      </div>

      {/* Code Example */}
      <div className="bg-slate-900 rounded-lg p-4">
        <div className="text-xs text-slate-500 mb-2">Usage Example - All Composition Hooks</div>
        <pre className="text-xs text-slate-300 overflow-x-auto">
{`import {
  useKnowledgeStack,
  useCRUDStack,
  useDashboardStack,
  useProductionStack,
  useOfflineStore
} from 'unrdf/react-hooks';

// Generic stack with presets
const { query, data, loading } = useKnowledgeStack({ preset: 'basic' });

// CRUD-optimized (forms, data entry)
const { insert, update, delete: del, undo, redo, history } = useCRUDStack({
  optimisticUpdates: true,
  historySize: 50
});

// Dashboard-optimized (analytics, real-time)
const { data, subscribe, aggregate, refresh } = useDashboardStack({
  refreshInterval: 5000,
  aggregations: ['count', 'sum', 'avg']
});

// Production-ready (error handling, recovery, observability)
const {
  query, data,
  hasError, error, recover,
  retryCount, isRecovering,
  metrics, trace
} = useProductionStack({
  maxRetries: 3,
  enableTelemetry: true
});

// Offline-first with IndexedDB
const { quads, insert, sync, pendingCount, isOnline } = useOfflineStore();
await insert([{ subject, predicate, object }]); // Works offline!`}
        </pre>
      </div>
    </div>
  );
}
