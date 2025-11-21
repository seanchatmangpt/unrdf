"use client";

import { useState } from "react";
import { Button } from "@/components/ui/button";
import { Badge } from "@/components/ui/badge";

export function ErrorRecoveryDemo() {
  const [hasError, setHasError] = useState(false);
  const [error, setError] = useState(null);
  const [retryCount, setRetryCount] = useState(0);
  const [isRecovering, setIsRecovering] = useState(false);
  const [errorLog, setErrorLog] = useState([]);

  const simulateError = () => {
    const err = new Error("Simulated network error: Connection timeout");
    setHasError(true);
    setError(err);
    setErrorLog(prev => [...prev, {
      id: Date.now(),
      message: err.message,
      timestamp: new Date().toISOString(),
      stack: "at fetchData (api.js:42)\nat query (hooks.js:156)"
    }]);
  };

  const simulateRecovery = async () => {
    setIsRecovering(true);
    for (let i = 1; i <= 3; i++) {
      setRetryCount(i);
      await new Promise(r => setTimeout(r, 800));
      if (i === 3) {
        // Success on 3rd retry
        setHasError(false);
        setError(null);
        setIsRecovering(false);
        setRetryCount(0);
      }
    }
  };

  const resetError = () => {
    setHasError(false);
    setError(null);
    setRetryCount(0);
    setIsRecovering(false);
  };

  const clearLog = () => {
    setErrorLog([]);
  };

  return (
    <div className="space-y-6">
      {/* Status Display */}
      <div className="flex items-center gap-4">
        <Badge variant={hasError ? "destructive" : "success"}>
          {hasError ? "ERROR" : "HEALTHY"}
        </Badge>
        {isRecovering && (
          <Badge variant="warning">
            Recovering... (Attempt {retryCount}/3)
          </Badge>
        )}
        <span className="text-sm text-slate-400">
          {errorLog.length} errors logged
        </span>
      </div>

      {/* Error State Display */}
      {hasError && (
        <div className="bg-red-900/30 border border-red-700 rounded-lg p-4">
          <div className="flex items-start justify-between">
            <div>
              <div className="text-red-400 font-medium">Error Caught</div>
              <div className="text-sm text-red-300 mt-1">{error?.message}</div>
            </div>
            <div className="flex gap-2">
              <Button
                variant="outline"
                size="sm"
                onClick={simulateRecovery}
                disabled={isRecovering}
              >
                {isRecovering ? `Retrying (${retryCount}/3)...` : "Auto Recover"}
              </Button>
              <Button variant="secondary" size="sm" onClick={resetError}>
                Reset
              </Button>
            </div>
          </div>
        </div>
      )}

      {/* Controls */}
      <div className="flex gap-2">
        <Button variant="destructive" onClick={simulateError} disabled={hasError}>
          Simulate Error
        </Button>
        <Button variant="outline" onClick={clearLog}>
          Clear Log
        </Button>
      </div>

      {/* Error Log */}
      <div className="bg-slate-900 rounded-lg p-4">
        <div className="text-sm font-medium text-slate-300 mb-3">
          Error Log (useErrorReporting)
        </div>
        <div className="space-y-2 max-h-40 overflow-y-auto">
          {errorLog.length === 0 ? (
            <div className="text-center text-slate-500 py-4">
              No errors logged. Click "Simulate Error" to test.
            </div>
          ) : (
            errorLog.map(log => (
              <div key={log.id} className="bg-slate-800 p-2 rounded text-xs">
                <div className="flex justify-between text-slate-400">
                  <span className="text-red-400 font-medium">{log.message}</span>
                  <span>{new Date(log.timestamp).toLocaleTimeString()}</span>
                </div>
                <pre className="text-slate-500 mt-1">{log.stack}</pre>
              </div>
            ))
          )}
        </div>
      </div>

      {/* Code Example */}
      <div className="bg-slate-900 rounded-lg p-4">
        <div className="text-xs text-slate-500 mb-2">Usage Example</div>
        <pre className="text-xs text-slate-300 overflow-x-auto">
{`import { useErrorBoundary, useRecovery } from 'unrdf/react-hooks';

function App() {
  const { hasError, error, resetError } = useErrorBoundary({
    onError: (err) => analytics.track('error', err)
  });

  const { executeWithRecovery, retryCount, isRecovering } = useRecovery({
    maxRetries: 3,
    retryDelay: 1000  // Exponential backoff
  });

  if (hasError) {
    return (
      <div>
        <p>Error: {error.message}</p>
        <button onClick={resetError}>Try Again</button>
      </div>
    );
  }

  // Auto-retry on failure
  const fetchData = () => executeWithRecovery(async () => {
    return await api.query('SELECT * WHERE { ?s ?p ?o }');
  });
}`}
        </pre>
      </div>
    </div>
  );
}
