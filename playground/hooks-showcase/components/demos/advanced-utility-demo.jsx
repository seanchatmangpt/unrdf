/**
 * @file playground/hooks-showcase/components/demos/advanced-utility-demo.jsx
 * @description Advanced utility hooks - Graph diff, isomorphism, reasoning, observability
 */

"use client";

import { useState } from "react";
import { Button } from "@/components/ui/button";
import { Badge } from "@/components/ui/badge";

export function AdvancedUtilityDemo() {
  const [diffResults, setDiffResults] = useState(null);
  const [mergeStatus, setMergeStatus] = useState(null);
  const [qualityMetrics, setQualityMetrics] = useState(null);
  const [observability, setObservability] = useState({
    traces: [],
    spans: 0,
    duration: 0
  });

  const runGraphDiff = () => {
    // Simulate graph diff operation
    setDiffResults({
      added: [
        { subject: "ex:Alice", predicate: "foaf:age", object: "29" },
        { subject: "ex:Dave", predicate: "foaf:name", object: '"Dave Wilson"' }
      ],
      removed: [
        { subject: "ex:Alice", predicate: "foaf:age", object: "28" }
      ],
      modified: [
        {
          subject: "ex:Bob",
          predicate: "foaf:email",
          old: '"bob@old.com"',
          new: '"bob@new.com"'
        }
      ]
    });
  };

  const checkIsomorphism = () => {
    // Simulate isomorphism check
    const isomorphic = Math.random() > 0.5;
    setMergeStatus({
      isomorphic,
      confidence: isomorphic ? 0.95 : 0.12,
      mappings: isomorphic ? 8 : 0
    });
  };

  const analyzeQuality = () => {
    // Simulate quality metrics calculation
    setQualityMetrics({
      completeness: 87,
      consistency: 92,
      accuracy: 95,
      timeliness: 88,
      uniqueness: 94,
      overall: 91
    });
  };

  const captureObservability = () => {
    // Simulate OTEL trace capture
    const newTrace = {
      id: Date.now(),
      operation: "SPARQL Query",
      duration: Math.floor(Math.random() * 100) + 10,
      spans: Math.floor(Math.random() * 5) + 2
    };

    setObservability(prev => ({
      traces: [newTrace, ...prev.traces].slice(0, 5),
      spans: prev.spans + newTrace.spans,
      duration: prev.duration + newTrace.duration
    }));
  };

  return (
    <div className="space-y-6">
      {/* Graph Diff */}
      <div className="bg-slate-900 rounded-lg p-4 border border-slate-700">
        <div className="flex items-center justify-between mb-3">
          <h3 className="text-sm font-medium text-slate-300">Graph Diff & Merge</h3>
          <div className="flex gap-2">
            <Button size="sm" onClick={runGraphDiff}>Run Diff</Button>
            <Button size="sm" variant="outline" onClick={checkIsomorphism}>
              Check Isomorphism
            </Button>
          </div>
        </div>

        {diffResults && (
          <div className="space-y-3 mt-4">
            <div className="p-3 bg-green-900/20 border border-green-700/50 rounded">
              <div className="flex items-center justify-between mb-2">
                <span className="text-xs font-medium text-green-400">Added ({diffResults.added.length})</span>
                <Badge variant="success" className="text-xs">+</Badge>
              </div>
              {diffResults.added.map((triple, idx) => (
                <div key={idx} className="text-xs font-mono text-slate-300">
                  {triple.subject} {triple.predicate} {triple.object}
                </div>
              ))}
            </div>

            <div className="p-3 bg-red-900/20 border border-red-700/50 rounded">
              <div className="flex items-center justify-between mb-2">
                <span className="text-xs font-medium text-red-400">Removed ({diffResults.removed.length})</span>
                <Badge variant="destructive" className="text-xs">-</Badge>
              </div>
              {diffResults.removed.map((triple, idx) => (
                <div key={idx} className="text-xs font-mono text-slate-300">
                  {triple.subject} {triple.predicate} {triple.object}
                </div>
              ))}
            </div>

            <div className="p-3 bg-blue-900/20 border border-blue-700/50 rounded">
              <div className="flex items-center justify-between mb-2">
                <span className="text-xs font-medium text-blue-400">Modified ({diffResults.modified.length})</span>
                <Badge variant="default" className="text-xs">~</Badge>
              </div>
              {diffResults.modified.map((change, idx) => (
                <div key={idx} className="text-xs font-mono space-y-1">
                  <div className="text-slate-400">{change.subject} {change.predicate}</div>
                  <div className="text-red-400">- {change.old}</div>
                  <div className="text-green-400">+ {change.new}</div>
                </div>
              ))}
            </div>
          </div>
        )}

        {mergeStatus && (
          <div className={`mt-3 p-3 border rounded ${mergeStatus.isomorphic ? 'bg-green-900/20 border-green-700/50' : 'bg-orange-900/20 border-orange-700/50'}`}>
            <div className="flex items-center justify-between">
              <span className="text-sm font-medium">
                {mergeStatus.isomorphic ? '✓ Graphs are isomorphic' : '✗ Graphs are not isomorphic'}
              </span>
              <Badge variant={mergeStatus.isomorphic ? "success" : "secondary"}>
                {(mergeStatus.confidence * 100).toFixed(0)}% confidence
              </Badge>
            </div>
            {mergeStatus.isomorphic && (
              <p className="text-xs text-slate-400 mt-1">
                Found {mergeStatus.mappings} valid node mappings
              </p>
            )}
          </div>
        )}
      </div>

      {/* Quality Metrics */}
      <div className="bg-slate-900 rounded-lg p-4 border border-slate-700">
        <div className="flex items-center justify-between mb-3">
          <h3 className="text-sm font-medium text-slate-300">Data Quality Metrics</h3>
          <Button size="sm" onClick={analyzeQuality}>Analyze Quality</Button>
        </div>

        {qualityMetrics && (
          <div className="space-y-2">
            {Object.entries(qualityMetrics).map(([metric, score]) => (
              <div key={metric} className="space-y-1">
                <div className="flex items-center justify-between text-xs">
                  <span className="text-slate-400 capitalize">{metric}</span>
                  <span className="text-slate-300 font-medium">{score}%</span>
                </div>
                <div className="w-full bg-slate-800 rounded-full h-2">
                  <div
                    className={`h-2 rounded-full ${
                      score >= 90 ? 'bg-green-500' :
                      score >= 75 ? 'bg-blue-500' :
                      'bg-orange-500'
                    }`}
                    style={{ width: `${score}%` }}
                  />
                </div>
              </div>
            ))}
          </div>
        )}
      </div>

      {/* Observability */}
      <div className="bg-slate-900 rounded-lg p-4 border border-slate-700">
        <div className="flex items-center justify-between mb-3">
          <h3 className="text-sm font-medium text-slate-300">OpenTelemetry Observability</h3>
          <Button size="sm" onClick={captureObservability}>Capture Trace</Button>
        </div>

        <div className="grid grid-cols-3 gap-3 mb-4">
          <div className="bg-slate-800 rounded p-3 text-center">
            <div className="text-lg font-bold text-blue-400">{observability.traces.length}</div>
            <div className="text-xs text-slate-400">Traces</div>
          </div>
          <div className="bg-slate-800 rounded p-3 text-center">
            <div className="text-lg font-bold text-purple-400">{observability.spans}</div>
            <div className="text-xs text-slate-400">Spans</div>
          </div>
          <div className="bg-slate-800 rounded p-3 text-center">
            <div className="text-lg font-bold text-green-400">{observability.duration}ms</div>
            <div className="text-xs text-slate-400">Total Time</div>
          </div>
        </div>

        {observability.traces.length > 0 && (
          <div className="space-y-2">
            <h4 className="text-xs font-medium text-slate-400 mb-2">Recent Traces</h4>
            {observability.traces.map((trace) => (
              <div key={trace.id} className="p-2 bg-slate-800 rounded text-xs">
                <div className="flex items-center justify-between mb-1">
                  <span className="text-slate-300 font-mono">{trace.operation}</span>
                  <Badge variant="outline" className="text-xs">{trace.duration}ms</Badge>
                </div>
                <div className="text-slate-500">{trace.spans} spans captured</div>
              </div>
            ))}
          </div>
        )}
      </div>
    </div>
  );
}
