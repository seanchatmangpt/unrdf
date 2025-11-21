"use client";

import { useState } from "react";
import { Button } from "@/components/ui/button";
import { Badge } from "@/components/ui/badge";

// Mock useDarkMatterCore
function useDarkMatterCoreMock() {
  const [analysis, setAnalysis] = useState(null);
  const [loading, setLoading] = useState(false);

  const analyze = async () => {
    setLoading(true);
    await new Promise(r => setTimeout(r, 1000));
    setAnalysis({
      paretoScore: 78,
      criticalPaths: [
        { path: "query → filter → sort", impact: 0.4, latency: 120 },
        { path: "insert → validate → commit", impact: 0.3, latency: 85 },
        { path: "fetch → transform → render", impact: 0.2, latency: 65 }
      ],
      suggestions: [
        { type: "INDEX", reason: "Add index on foaf:name for 40% faster lookups", gain: "40%" },
        { type: "CACHE", reason: "Enable query caching for repeated SELECT queries", gain: "60%" },
        { type: "BATCH", reason: "Batch insert operations for 30% throughput gain", gain: "30%" }
      ],
      metrics: {
        totalQueries: 1247,
        avgLatency: 45,
        cacheHitRate: 0.68,
        errorRate: 0.02
      }
    });
    setLoading(false);
  };

  return { analysis, analyze, loading };
}

export function DarkMatterDemo() {
  const { analysis, analyze, loading } = useDarkMatterCoreMock();

  return (
    <div className="space-y-6">
      {/* Controls */}
      <div className="flex items-center gap-4">
        <Button onClick={analyze} disabled={loading}>
          {loading ? "Analyzing..." : "Run 80/20 Analysis"}
        </Button>
        {analysis && (
          <Badge variant={analysis.paretoScore >= 80 ? "success" : "warning"}>
            Pareto Score: {analysis.paretoScore}/100
          </Badge>
        )}
      </div>

      {analysis && (
        <>
          {/* Metrics Grid */}
          <div className="grid grid-cols-4 gap-4">
            <div className="bg-slate-900 rounded-lg p-4 text-center">
              <div className="text-2xl font-bold text-blue-400">
                {analysis.metrics.totalQueries}
              </div>
              <div className="text-xs text-slate-400">Total Queries</div>
            </div>
            <div className="bg-slate-900 rounded-lg p-4 text-center">
              <div className="text-2xl font-bold text-green-400">
                {analysis.metrics.avgLatency}ms
              </div>
              <div className="text-xs text-slate-400">Avg Latency</div>
            </div>
            <div className="bg-slate-900 rounded-lg p-4 text-center">
              <div className="text-2xl font-bold text-purple-400">
                {(analysis.metrics.cacheHitRate * 100).toFixed(0)}%
              </div>
              <div className="text-xs text-slate-400">Cache Hit Rate</div>
            </div>
            <div className="bg-slate-900 rounded-lg p-4 text-center">
              <div className="text-2xl font-bold text-red-400">
                {(analysis.metrics.errorRate * 100).toFixed(1)}%
              </div>
              <div className="text-xs text-slate-400">Error Rate</div>
            </div>
          </div>

          {/* Critical Paths */}
          <div className="bg-slate-900 rounded-lg p-4">
            <div className="text-sm font-medium text-slate-300 mb-3">
              Critical Paths (20% causing 80% of load)
            </div>
            <div className="space-y-3">
              {analysis.criticalPaths.map((path, i) => (
                <div key={i} className="flex items-center gap-4">
                  <div className="flex-1">
                    <div className="text-sm text-slate-300 font-mono">{path.path}</div>
                    <div className="h-2 bg-slate-700 rounded-full mt-1">
                      <div
                        className="h-2 bg-blue-500 rounded-full"
                        style={{ width: `${path.impact * 100}%` }}
                      />
                    </div>
                  </div>
                  <div className="text-right">
                    <div className="text-sm text-slate-300">{path.latency}ms</div>
                    <div className="text-xs text-slate-500">{(path.impact * 100).toFixed(0)}% impact</div>
                  </div>
                </div>
              ))}
            </div>
          </div>

          {/* Optimization Suggestions */}
          <div className="bg-slate-900 rounded-lg p-4">
            <div className="text-sm font-medium text-slate-300 mb-3">
              Top Optimization Suggestions
            </div>
            <div className="space-y-3">
              {analysis.suggestions.map((sug, i) => (
                <div key={i} className="flex items-start gap-3 p-3 bg-slate-800 rounded">
                  <Badge variant={sug.type === "INDEX" ? "default" : sug.type === "CACHE" ? "success" : "secondary"}>
                    {sug.type}
                  </Badge>
                  <div className="flex-1">
                    <div className="text-sm text-slate-300">{sug.reason}</div>
                  </div>
                  <div className="text-green-400 font-bold">+{sug.gain}</div>
                </div>
              ))}
            </div>
          </div>
        </>
      )}

      {/* Additional Dark Matter Hooks */}
      <div className="grid grid-cols-3 gap-4">
        {/* useQueryAnalyzer */}
        <div className="bg-slate-900 rounded-lg p-4">
          <div className="text-sm font-medium text-slate-300 mb-2">useQueryAnalyzer</div>
          <div className="text-xs text-slate-400 mb-2">Optimize slow SPARQL queries</div>
          <div className="space-y-1 text-xs">
            <div className="flex justify-between">
              <span className="text-slate-500">Slow queries:</span>
              <span className="text-yellow-400">3</span>
            </div>
            <div className="flex justify-between">
              <span className="text-slate-500">Optimizable:</span>
              <span className="text-green-400">2</span>
            </div>
          </div>
        </div>

        {/* useOptimizer */}
        <div className="bg-slate-900 rounded-lg p-4">
          <div className="text-sm font-medium text-slate-300 mb-2">useOptimizer</div>
          <div className="text-xs text-slate-400 mb-2">Auto-optimization engine</div>
          <Badge variant="success">Auto-tuning ON</Badge>
          <div className="text-xs text-slate-500 mt-1">Combines DarkMatter + QueryAnalyzer</div>
        </div>

        {/* useCriticalPath */}
        <div className="bg-slate-900 rounded-lg p-4">
          <div className="text-sm font-medium text-slate-300 mb-2">useCriticalPath</div>
          <div className="text-xs text-slate-400 mb-2">Identify bottleneck operations</div>
          <div className="text-xs">
            <div className="text-red-400">Critical: query→filter (45ms)</div>
            <div className="text-yellow-400">Warning: sort→limit (23ms)</div>
          </div>
        </div>
      </div>

      {/* Code Example */}
      <div className="bg-slate-900 rounded-lg p-4">
        <div className="text-xs text-slate-500 mb-2">Usage Example - All Dark Matter Hooks</div>
        <pre className="text-xs text-slate-300 overflow-x-auto">
{`import {
  useDarkMatterCore,
  useQueryAnalyzer,
  useOptimizer,
  useCriticalPath
} from 'unrdf/react-hooks';

// 80/20 Performance Analysis
const { analysis, analyze, paretoScore } = useDarkMatterCore();

// Query-specific optimization
const { optimizeQuery, slowQueries, suggestions } = useQueryAnalyzer({ slowThreshold: 100 });
const optimized = await optimizeQuery('SELECT * WHERE { ?s ?p ?o }');
console.log('Expected gain:', optimized.estimatedGain);

// Auto-optimization (combines both)
const { enable, disable, isOptimizing } = useOptimizer({ autoApply: true });

// Find bottlenecks
const { paths, analyze: analyzePath } = useCriticalPath();
paths.forEach(p => console.log(p.operation, p.latency, p.impact));`}
        </pre>
      </div>
    </div>
  );
}
