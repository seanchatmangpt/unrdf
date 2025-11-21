"use client";

import { useState } from "react";
import { Button } from "@/components/ui/button";
import { Badge } from "@/components/ui/badge";

export function AdvancedUtilityDemo() {
  const [diffResult, setDiffResult] = useState(null);
  const [qualityScore, setQualityScore] = useState(null);
  const [traces, setTraces] = useState([]);

  const computeDiff = () => {
    setDiffResult({
      added: 12,
      removed: 5,
      modified: 3,
      stats: { totalChanges: 20 }
    });
  };

  const analyzeQuality = () => {
    setQualityScore({
      overall: 87,
      completeness: 92,
      consistency: 85,
      accuracy: 88,
      timeliness: 82
    });
  };

  const startTrace = () => {
    setTraces(prev => [...prev, {
      id: `trace-${Date.now()}`,
      operation: "query",
      duration: Math.floor(Math.random() * 100) + 20,
      status: "success"
    }]);
  };

  return (
    <div className="space-y-6">
      {/* useGraphDiff */}
      <div className="bg-slate-900 rounded-lg p-4">
        <div className="flex items-center justify-between mb-3">
          <div>
            <div className="text-sm font-medium text-slate-300">useGraphDiff</div>
            <div className="text-xs text-slate-400">Compute differences between graph versions</div>
          </div>
          <Button variant="outline" size="sm" onClick={computeDiff}>Compare</Button>
        </div>
        {diffResult && (
          <div className="grid grid-cols-4 gap-4">
            <div className="bg-slate-800 p-3 rounded text-center">
              <div className="text-lg font-bold text-green-400">+{diffResult.added}</div>
              <div className="text-xs text-slate-500">Added</div>
            </div>
            <div className="bg-slate-800 p-3 rounded text-center">
              <div className="text-lg font-bold text-red-400">-{diffResult.removed}</div>
              <div className="text-xs text-slate-500">Removed</div>
            </div>
            <div className="bg-slate-800 p-3 rounded text-center">
              <div className="text-lg font-bold text-yellow-400">~{diffResult.modified}</div>
              <div className="text-xs text-slate-500">Modified</div>
            </div>
            <div className="bg-slate-800 p-3 rounded text-center">
              <div className="text-lg font-bold text-blue-400">{diffResult.stats.totalChanges}</div>
              <div className="text-xs text-slate-500">Total</div>
            </div>
          </div>
        )}
      </div>

      {/* Grid of additional hooks */}
      <div className="grid grid-cols-2 gap-4">
        {/* useIsomorphism */}
        <div className="bg-slate-900 rounded-lg p-4">
          <div className="text-sm font-medium text-slate-300 mb-2">useIsomorphism</div>
          <div className="text-xs text-slate-400 mb-2">Graph structural equivalence</div>
          <div className="flex items-center gap-2">
            <Badge variant="success">Isomorphic: Yes</Badge>
            <span className="text-xs text-slate-500">Canonical form match</span>
          </div>
        </div>

        {/* useReasoningSession */}
        <div className="bg-slate-900 rounded-lg p-4">
          <div className="text-sm font-medium text-slate-300 mb-2">useReasoningSession</div>
          <div className="text-xs text-slate-400 mb-2">OWL/RDFS inference engine</div>
          <div className="space-y-1 text-xs">
            <div className="flex justify-between">
              <span className="text-slate-500">Inferred triples:</span>
              <span className="text-green-400">+156</span>
            </div>
            <div className="flex justify-between">
              <span className="text-slate-500">Rules applied:</span>
              <span className="text-blue-400">rdfs:subClassOf, owl:sameAs</span>
            </div>
          </div>
        </div>

        {/* useGraphMerge */}
        <div className="bg-slate-900 rounded-lg p-4">
          <div className="text-sm font-medium text-slate-300 mb-2">useGraphMerge</div>
          <div className="text-xs text-slate-400 mb-2">Merge graphs with conflict resolution</div>
          <div className="flex items-center gap-2">
            <Badge variant="outline">3 graphs</Badge>
            <Badge variant="warning">2 conflicts</Badge>
            <Badge variant="success">Resolved</Badge>
          </div>
        </div>

        {/* useQualityMetrics */}
        <div className="bg-slate-900 rounded-lg p-4">
          <div className="flex items-center justify-between mb-2">
            <div className="text-sm font-medium text-slate-300">useQualityMetrics</div>
            <Button variant="outline" size="sm" onClick={analyzeQuality}>Analyze</Button>
          </div>
          {qualityScore && (
            <div className="space-y-1">
              {Object.entries(qualityScore).filter(([k]) => k !== 'overall').map(([key, val]) => (
                <div key={key} className="flex items-center gap-2">
                  <span className="text-xs text-slate-500 w-24 capitalize">{key}:</span>
                  <div className="flex-1 h-2 bg-slate-700 rounded">
                    <div className="h-2 bg-blue-500 rounded" style={{ width: `${val}%` }} />
                  </div>
                  <span className="text-xs text-slate-400">{val}%</span>
                </div>
              ))}
            </div>
          )}
        </div>
      </div>

      {/* useObservabilityManager */}
      <div className="bg-slate-900 rounded-lg p-4">
        <div className="flex items-center justify-between mb-3">
          <div>
            <div className="text-sm font-medium text-slate-300">useObservabilityManager</div>
            <div className="text-xs text-slate-400">Distributed tracing (OpenTelemetry pattern)</div>
          </div>
          <Button variant="outline" size="sm" onClick={startTrace}>Start Trace</Button>
        </div>
        <div className="space-y-2 max-h-32 overflow-y-auto">
          {traces.length === 0 ? (
            <div className="text-center text-slate-500 py-4 text-xs">Click "Start Trace" to add spans</div>
          ) : (
            traces.map((t) => (
              <div key={t.id} className="flex items-center justify-between bg-slate-800 p-2 rounded text-xs">
                <span className="text-slate-400 font-mono">{t.id.slice(0, 20)}...</span>
                <span className="text-slate-300">{t.operation}</span>
                <span className="text-blue-400">{t.duration}ms</span>
                <Badge variant="success">{t.status}</Badge>
              </div>
            ))
          )}
        </div>
      </div>

      {/* Code Example */}
      <div className="bg-slate-900 rounded-lg p-4">
        <div className="text-xs text-slate-500 mb-2">Usage Example - All Advanced Utility Hooks</div>
        <pre className="text-xs text-slate-300 overflow-x-auto">
{`import {
  useGraphDiff, useIsomorphism, useReasoningSession,
  useGraphMerge, useQualityMetrics, useObservabilityManager
} from 'unrdf/react-hooks';

// Version control for graphs
const { computeDiff, diff, applyPatch } = useGraphDiff();
const changes = await computeDiff('graph:v1', 'graph:v2');

// Check structural equivalence
const { checkIsomorphism, signature } = useIsomorphism();
const isEqual = await checkIsomorphism(graph1, graph2);

// OWL reasoning
const { infer, materialize, rules } = useReasoningSession({ rules: ['rdfs', 'owl'] });
const inferred = await infer(graph);

// Merge multiple graphs
const { merge, conflicts, resolve } = useGraphMerge({ strategy: 'latest-wins' });
const merged = await merge([graph1, graph2, graph3]);

// Data quality scoring
const { compute, score, metrics } = useQualityMetrics();
const quality = await compute(graph); // { completeness: 92, consistency: 85, ... }

// Distributed tracing
const { startTrace, endTrace, recordMetric } = useObservabilityManager();
const traceId = startTrace('query-execution');
// ... operation ...
endTrace(traceId);`}
        </pre>
      </div>
    </div>
  );
}
