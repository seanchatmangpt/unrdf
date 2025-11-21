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

      {/* Code Example */}
      <div className="bg-slate-900 rounded-lg p-4">
        <div className="text-xs text-slate-500 mb-2">Usage Example</div>
        <pre className="text-xs text-slate-300 overflow-x-auto">
{`// Hook Composition - Choose your preset
import { useKnowledgeStack, useOfflineStore } from 'unrdf/react-hooks';

// Basic CRUD app
const { query, data, loading } = useKnowledgeStack({ preset: 'basic' });

// Real-time dashboard
const { query, data, changes, startLive } = useKnowledgeStack({ preset: 'realtime' });

// Production resilient
const { query, data, retryCount, isRecovering } = useKnowledgeStack({ preset: 'full' });

// Offline-first storage
const {
  quads,
  insert,
  sync,
  pendingCount,
  isOnline
} = useOfflineStore();

// Insert works offline - automatically syncs when online
await insert([{ subject, predicate, object }]);`}
        </pre>
      </div>
    </div>
  );
}
