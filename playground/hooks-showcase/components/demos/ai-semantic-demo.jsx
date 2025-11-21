"use client";

import { useState } from "react";
import { Button } from "@/components/ui/button";
import { Badge } from "@/components/ui/badge";

export function AISemanticDemo() {
  const [nlQuery, setNlQuery] = useState("Show all people who know Bob");
  const [sparqlResult, setSparqlResult] = useState("");
  const [embeddings, setEmbeddings] = useState([]);
  const [anomalies, setAnomalies] = useState([]);

  const convertToSPARQL = () => {
    setSparqlResult(`SELECT ?person ?name WHERE {
  ?person foaf:knows ex:bob ;
          foaf:name ?name .
}`);
  };

  const generateEmbeddings = () => {
    setEmbeddings([
      { uri: "ex:alice", similarity: 0.95, label: "Alice" },
      { uri: "ex:charlie", similarity: 0.87, label: "Charlie" },
      { uri: "ex:diana", similarity: 0.72, label: "Diana" },
    ]);
  };

  const detectAnomalies = () => {
    setAnomalies([
      { subject: "ex:product-99", value: 9999, zScore: 4.2, severity: "high" },
      { subject: "ex:product-45", value: -50, zScore: 3.1, severity: "medium" },
    ]);
  };

  return (
    <div className="space-y-6">
      {/* useSemanticAnalyzer */}
      <div className="bg-slate-900 rounded-lg p-4">
        <div className="text-sm font-medium text-slate-300 mb-3">useSemanticAnalyzer</div>
        <div className="text-xs text-slate-400 mb-3">Analyze semantic relationships in RDF graphs</div>
        <div className="grid grid-cols-3 gap-4">
          <div className="bg-slate-800 p-3 rounded">
            <div className="text-xs text-slate-500 mb-1">Relationships Found</div>
            <div className="text-lg font-bold text-blue-400">247</div>
          </div>
          <div className="bg-slate-800 p-3 rounded">
            <div className="text-xs text-slate-500 mb-1">Entity Types</div>
            <div className="text-lg font-bold text-green-400">12</div>
          </div>
          <div className="bg-slate-800 p-3 rounded">
            <div className="text-xs text-slate-500 mb-1">Clusters</div>
            <div className="text-lg font-bold text-purple-400">5</div>
          </div>
        </div>
      </div>

      {/* useNLPQueryBuilder */}
      <div className="bg-slate-900 rounded-lg p-4">
        <div className="text-sm font-medium text-slate-300 mb-3">useNLPQueryBuilder</div>
        <div className="text-xs text-slate-400 mb-3">Convert natural language to SPARQL</div>
        <div className="flex gap-2 mb-3">
          <input
            type="text"
            value={nlQuery}
            onChange={(e) => setNlQuery(e.target.value)}
            className="flex-1 bg-slate-800 border border-slate-600 rounded px-3 py-2 text-sm text-slate-300"
            placeholder="Enter natural language query..."
          />
          <Button onClick={convertToSPARQL}>Convert</Button>
        </div>
        {sparqlResult && (
          <pre className="bg-slate-800 p-3 rounded text-xs text-green-400 overflow-x-auto">
            {sparqlResult}
          </pre>
        )}
      </div>

      {/* useEmbeddingsManager */}
      <div className="bg-slate-900 rounded-lg p-4">
        <div className="flex items-center justify-between mb-3">
          <div>
            <div className="text-sm font-medium text-slate-300">useEmbeddingsManager</div>
            <div className="text-xs text-slate-400">Vector embeddings for semantic search</div>
          </div>
          <Button variant="outline" size="sm" onClick={generateEmbeddings}>
            Find Similar
          </Button>
        </div>
        {embeddings.length > 0 && (
          <div className="space-y-2">
            {embeddings.map((e, i) => (
              <div key={i} className="flex items-center justify-between bg-slate-800 p-2 rounded">
                <span className="text-sm text-slate-300">{e.label}</span>
                <div className="flex items-center gap-2">
                  <div className="w-24 h-2 bg-slate-700 rounded-full">
                    <div
                      className="h-2 bg-blue-500 rounded-full"
                      style={{ width: `${e.similarity * 100}%` }}
                    />
                  </div>
                  <span className="text-xs text-slate-400">{(e.similarity * 100).toFixed(0)}%</span>
                </div>
              </div>
            ))}
          </div>
        )}
      </div>

      {/* useAnomalyDetector */}
      <div className="bg-slate-900 rounded-lg p-4">
        <div className="flex items-center justify-between mb-3">
          <div>
            <div className="text-sm font-medium text-slate-300">useAnomalyDetector</div>
            <div className="text-xs text-slate-400">Statistical anomaly detection (z-scores)</div>
          </div>
          <Button variant="outline" size="sm" onClick={detectAnomalies}>
            Detect Anomalies
          </Button>
        </div>
        {anomalies.length > 0 && (
          <div className="space-y-2">
            {anomalies.map((a, i) => (
              <div key={i} className="flex items-center justify-between bg-slate-800 p-2 rounded">
                <span className="text-sm text-slate-300 font-mono">{a.subject}</span>
                <div className="flex items-center gap-2">
                  <span className="text-xs text-slate-400">Value: {a.value}</span>
                  <span className="text-xs text-slate-400">Z: {a.zScore}</span>
                  <Badge variant={a.severity === "high" ? "destructive" : "warning"}>
                    {a.severity}
                  </Badge>
                </div>
              </div>
            ))}
          </div>
        )}
      </div>

      {/* Code Example */}
      <div className="bg-slate-900 rounded-lg p-4">
        <div className="text-xs text-slate-500 mb-2">Usage Example - All AI/Semantic Hooks</div>
        <pre className="text-xs text-slate-300 overflow-x-auto">
{`import {
  useSemanticAnalyzer,
  useNLPQueryBuilder,
  useEmbeddingsManager,
  useAnomalyDetector
} from 'unrdf/react-hooks';

// Analyze relationships
const { analyze, relationships, findSimilar } = useSemanticAnalyzer();
const rels = await analyze('http://example.org/alice');

// Natural language to SPARQL
const { buildQuery, history } = useNLPQueryBuilder();
const sparql = buildQuery('Show all people over 30');

// Vector embeddings for similarity
const { index, search, embeddings } = useEmbeddingsManager({ dimensions: 128 });
await index('http://example.org/alice', ['software', 'engineer']);
const similar = await search('developer', 10);

// Anomaly detection
const { detect, anomalies, threshold } = useAnomalyDetector({ threshold: 2.5 });
const outliers = await detect('http://example.org/price');`}
        </pre>
      </div>
    </div>
  );
}
