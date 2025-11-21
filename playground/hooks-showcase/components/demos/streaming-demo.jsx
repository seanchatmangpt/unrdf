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

      {/* Code Example */}
      <div className="bg-slate-900 rounded-lg p-4">
        <div className="text-xs text-slate-500 mb-2">Usage Example</div>
        <pre className="text-xs text-slate-300 overflow-x-auto">
{`import { useChangeFeed } from 'unrdf/react-hooks';

function LiveDashboard() {
  const { changes, start, stop, stats } = useChangeFeed({
    operations: ['insert', 'delete'],
    batchSize: 10
  });

  useEffect(() => {
    start();
    return () => stop();
  }, []);

  return (
    <div>
      <p>Live Updates: {stats.totalChanges}</p>
      {changes.map(c => <div key={c.id}>{c.operation}: {c.subject}</div>)}
    </div>
  );
}`}
        </pre>
      </div>
    </div>
  );
}
