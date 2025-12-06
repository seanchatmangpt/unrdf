/**
 * @file playground/hooks-showcase/components/demos/error-recovery-demo.jsx
 * @description Error handling and recovery hooks demonstration
 */

"use client";

import { useState } from "react";
import { Button } from "@/components/ui/button";
import { Badge } from "@/components/ui/badge";

export function ErrorRecoveryDemo() {
  const [errors, setErrors] = useState([]);
  const [recoveryAttempts, setRecoveryAttempts] = useState(0);
  const [errorReportingEnabled, setErrorReportingEnabled] = useState(true);

  const errorTypes = [
    { type: "validation", message: "SHACL validation failed for triple", severity: "warning" },
    { type: "network", message: "Failed to sync with remote endpoint", severity: "error" },
    { type: "query", message: "SPARQL syntax error in query", severity: "error" },
    { type: "permission", message: "Unauthorized access to graph", severity: "critical" }
  ];

  const simulateError = () => {
    const randomError = errorTypes[Math.floor(Math.random() * errorTypes.length)];
    const error = {
      ...randomError,
      timestamp: new Date().toISOString(),
      id: Date.now()
    };
    setErrors(prev => [error, ...prev].slice(0, 5));
  };

  const attemptRecovery = (errorId) => {
    setRecoveryAttempts(prev => prev + 1);
    setErrors(prev => prev.filter(e => e.id !== errorId));
  };

  const clearAllErrors = () => {
    setErrors([]);
    setRecoveryAttempts(0);
  };

  const getSeverityColor = (severity) => {
    switch(severity) {
      case "warning": return "text-yellow-400 bg-yellow-900/20 border-yellow-700/50";
      case "error": return "text-orange-400 bg-orange-900/20 border-orange-700/50";
      case "critical": return "text-red-400 bg-red-900/20 border-red-700/50";
      default: return "text-slate-400 bg-slate-900/20 border-slate-700/50";
    }
  };

  return (
    <div className="space-y-6">
      {/* Error Boundary Controls */}
      <div className="bg-slate-900 rounded-lg p-4 border border-slate-700">
        <div className="flex items-center justify-between mb-4">
          <h3 className="text-sm font-medium text-slate-300">Error Handling</h3>
          <div className="flex gap-2">
            <Button variant="outline" size="sm" onClick={simulateError}>
              Simulate Error
            </Button>
            <Button variant="destructive" size="sm" onClick={clearAllErrors} disabled={errors.length === 0}>
              Clear All
            </Button>
          </div>
        </div>

        {/* Error Reporting Toggle */}
        <div className="flex items-center justify-between p-3 bg-slate-800 rounded">
          <div>
            <p className="text-sm text-slate-300">Error Reporting</p>
            <p className="text-xs text-slate-500">Send errors to monitoring service</p>
          </div>
          <Button
            variant={errorReportingEnabled ? "default" : "outline"}
            size="sm"
            onClick={() => setErrorReportingEnabled(!errorReportingEnabled)}
          >
            {errorReportingEnabled ? "Enabled" : "Disabled"}
          </Button>
        </div>
      </div>

      {/* Stats */}
      <div className="grid grid-cols-3 gap-3">
        <div className="bg-slate-800 rounded-lg p-4 text-center border border-slate-700">
          <div className="text-2xl font-bold text-red-400">{errors.length}</div>
          <div className="text-xs text-slate-400 mt-1">Active Errors</div>
        </div>
        <div className="bg-slate-800 rounded-lg p-4 text-center border border-slate-700">
          <div className="text-2xl font-bold text-green-400">{recoveryAttempts}</div>
          <div className="text-xs text-slate-400 mt-1">Recoveries</div>
        </div>
        <div className="bg-slate-800 rounded-lg p-4 text-center border border-slate-700">
          <div className="text-2xl font-bold text-blue-400">
            {errors.length > 0 ? Math.round((recoveryAttempts / (errors.length + recoveryAttempts)) * 100) : 100}%
          </div>
          <div className="text-xs text-slate-400 mt-1">Success Rate</div>
        </div>
      </div>

      {/* Error List */}
      <div className="space-y-2">
        <h3 className="text-sm font-medium text-slate-300">Error Log</h3>
        {errors.length === 0 ? (
          <div className="bg-green-900/20 border border-green-700/50 rounded-lg p-6 text-center">
            <div className="text-3xl mb-2">✓</div>
            <p className="text-sm text-green-400">No errors detected</p>
            <p className="text-xs text-slate-500 mt-1">System is operating normally</p>
          </div>
        ) : (
          errors.map((error) => (
            <div key={error.id} className={`p-3 border rounded-lg ${getSeverityColor(error.severity)}`}>
              <div className="flex items-start justify-between">
                <div className="flex-1">
                  <div className="flex items-center gap-2 mb-1">
                    <Badge variant="outline" className="text-xs">
                      {error.type}
                    </Badge>
                    <Badge variant={error.severity === "critical" ? "destructive" : "secondary"} className="text-xs">
                      {error.severity}
                    </Badge>
                  </div>
                  <p className="text-sm font-medium">{error.message}</p>
                  <p className="text-xs opacity-75 mt-1">
                    {new Date(error.timestamp).toLocaleTimeString()}
                  </p>
                </div>
                <Button
                  size="sm"
                  variant="outline"
                  onClick={() => attemptRecovery(error.id)}
                  className="ml-3"
                >
                  Recover
                </Button>
              </div>
            </div>
          ))
        )}
      </div>
    </div>
  );
}
