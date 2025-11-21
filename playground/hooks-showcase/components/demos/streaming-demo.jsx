"use client";

import { useState, useEffect, useRef } from "react";
import { Button } from "@/components/ui/button";
import { Badge } from "@/components/ui/badge";

// Mock useChangeFeed
function useChangeFeedMock(config = {}) {
  const [changes, setChanges] = useState([]);
  const [isActive, setIsActive] = useState(false);
  const intervalRef = useRef(null);

  const start = () => {
    setIsActive(true);
    intervalRef.current = setInterval(() => {
      const operations = ['insert', 'delete', 'update'];
      const operation = operations[Math.floor(Math.random() * operations.length)];
      const newChange = {
        id: Date.now(),
        operation,
        timestamp: new Date().toISOString(),
        subject: `http://example.org/entity-${Math.floor(Math.random() * 1000)}`,
        predicate: 'http://xmlns.com/foaf/0.1/name',
        object: `Value-${Math.floor(Math.random() * 100)}`
      };
      setChanges(prev => [newChange, ...prev].slice(0, 20));
    }, 1500);
  };

  const stop = () => {
    setIsActive(false);
    if (intervalRef.current) {
      clearInterval(intervalRef.current);
    }
  };

  useEffect(() => {
    return () => {
      if (intervalRef.current) clearInterval(intervalRef.current);
    };
  }, []);

  return {
    changes,
    start,
    stop,
    isActive,
    stats: { totalChanges: changes.length }
  };
}

export function StreamingDemo() {
  const { changes, start, stop, isActive, stats } = useChangeFeedMock();

  const getOperationColor = (op) => {
    switch (op) {
      case 'insert': return 'bg-green-500';
      case 'delete': return 'bg-red-500';
      case 'update': return 'bg-yellow-500';
      default: return 'bg-gray-500';
    }
  };

  return (
    <div className="space-y-6">
      {/* Controls */}
      <div className="flex items-center gap-4">
        <Button
          onClick={isActive ? stop : start}
          variant={isActive ? "destructive" : "default"}
        >
          {isActive ? "Stop Stream" : "Start Stream"}
        </Button>
        <Badge variant={isActive ? "success" : "secondary"}>
          {isActive ? "LIVE" : "Stopped"}
        </Badge>
        <span className="text-sm text-slate-400">
          {stats.totalChanges} changes received
        </span>
      </div>

      {/* Live Feed */}
      <div className="bg-slate-900 rounded-lg p-4 h-64 overflow-y-auto">
        <div className="text-xs text-slate-500 mb-2">Real-time Change Feed</div>
        {changes.length === 0 ? (
          <div className="text-center text-slate-500 py-8">
            Click "Start Stream" to see live changes
          </div>
        ) : (
          <div className="space-y-2">
            {changes.map((change) => (
              <div
                key={change.id}
                className="flex items-center gap-3 p-2 bg-slate-800 rounded animate-in slide-in-from-top-2"
              >
                <div className={`w-2 h-2 rounded-full ${getOperationColor(change.operation)}`} />
                <Badge variant="outline" className="text-xs">
                  {change.operation.toUpperCase()}
                </Badge>
                <span className="text-xs text-slate-400 font-mono flex-1 truncate">
                  {change.subject}
                </span>
                <span className="text-xs text-slate-500">
                  {new Date(change.timestamp).toLocaleTimeString()}
                </span>
              </div>
            ))}
          </div>
        )}
      </div>

      {/* Additional Streaming Hooks */}
      <div className="grid grid-cols-2 gap-4">
        {/* useStreamProcessor */}
        <div className="bg-slate-900 rounded-lg p-4">
          <div className="text-sm font-medium text-slate-300 mb-2">useStreamProcessor</div>
          <div className="text-xs text-slate-400 mb-2">Window operations on streams</div>
          <div className="flex gap-2 flex-wrap">
            <Badge variant="outline">Tumbling</Badge>
            <Badge variant="outline">Sliding</Badge>
            <Badge variant="outline">Session</Badge>
          </div>
          <div className="text-xs text-slate-500 mt-2">Window: 5s | Processed: 1,247</div>
        </div>

        {/* useSubscriptionManager */}
        <div className="bg-slate-900 rounded-lg p-4">
          <div className="text-sm font-medium text-slate-300 mb-2">useSubscriptionManager</div>
          <div className="text-xs text-slate-400 mb-2">Manage multiple subscriptions</div>
          <div className="space-y-1 text-xs">
            <div className="flex items-center gap-2">
              <div className="w-2 h-2 rounded-full bg-green-500"></div>
              <span className="text-slate-300">prices (active)</span>
            </div>
            <div className="flex items-center gap-2">
              <div className="w-2 h-2 rounded-full bg-green-500"></div>
              <span className="text-slate-300">inventory (active)</span>
            </div>
            <div className="flex items-center gap-2">
              <div className="w-2 h-2 rounded-full bg-gray-500"></div>
              <span className="text-slate-500">alerts (paused)</span>
            </div>
          </div>
        </div>

        {/* useRealTimeValidator */}
        <div className="bg-slate-900 rounded-lg p-4">
          <div className="text-sm font-medium text-slate-300 mb-2">useRealTimeValidator</div>
          <div className="text-xs text-slate-400 mb-2">SHACL validation on streams</div>
          <div className="flex items-center gap-2">
            <Badge variant="success">Valid: 98.5%</Badge>
            <Badge variant="destructive">Violations: 12</Badge>
          </div>
        </div>

        {/* useStreamingPipeline */}
        <div className="bg-slate-900 rounded-lg p-4">
          <div className="text-sm font-medium text-slate-300 mb-2">useStreamingPipeline</div>
          <div className="text-xs text-slate-400 mb-2">Multi-stage stream processing</div>
          <div className="flex items-center gap-1 text-xs">
            <Badge variant="secondary">Source</Badge>
            <span className="text-slate-500">→</span>
            <Badge variant="secondary">Filter</Badge>
            <span className="text-slate-500">→</span>
            <Badge variant="secondary">Transform</Badge>
            <span className="text-slate-500">→</span>
            <Badge variant="secondary">Sink</Badge>
          </div>
        </div>
      </div>

      {/* Code Example */}
      <div className="bg-slate-900 rounded-lg p-4">
        <div className="text-xs text-slate-500 mb-2">Usage Example - All Streaming Hooks</div>
        <pre className="text-xs text-slate-300 overflow-x-auto">
{`import {
  useChangeFeed,
  useStreamProcessor,
  useSubscriptionManager,
  useRealTimeValidator,
  useStreamingPipeline
} from 'unrdf/react-hooks';

// Basic change feed
const { changes, start, stop } = useChangeFeed({ operations: ['insert'] });

// Window operations
const { window, aggregate } = useStreamProcessor({ windowType: 'tumbling', size: 5000 });
const avgPrice = aggregate(window, 'avg', 'price');

// Multiple subscriptions
const { subscribe, unsubscribe, subscriptions } = useSubscriptionManager();
subscribe('prices', { query: 'SELECT ?price WHERE...' });

// Real-time SHACL validation
const { violations, validationRate } = useRealTimeValidator({ shapes: shapesGraph });

// Multi-stage pipeline
const { addStage, execute } = useStreamingPipeline();
addStage('filter', (quad) => quad.predicate === 'price');
addStage('transform', (quad) => ({ ...quad, value: parseFloat(quad.object) }));`}
        </pre>
      </div>
    </div>
  );
}
