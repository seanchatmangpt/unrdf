"use client";

import { useState } from "react";
import { Button } from "@/components/ui/button";
import { Badge } from "@/components/ui/badge";

export function FormUIDemo() {
  const [query, setQuery] = useState(`SELECT ?person ?name ?age
WHERE {
  ?person a foaf:Person ;
          foaf:name ?name ;
          foaf:age ?age .
  FILTER (?age > 25)
}
ORDER BY ?name
LIMIT 10`);
  const [errors, setErrors] = useState([]);
  const [graphData, setGraphData] = useState(null);
  const [currentPage, setCurrentPage] = useState(1);
  const pageSize = 5;

  const mockResults = [
    { person: "ex:alice", name: "Alice", age: 30 },
    { person: "ex:bob", name: "Bob", age: 28 },
    { person: "ex:charlie", name: "Charlie", age: 35 },
    { person: "ex:diana", name: "Diana", age: 27 },
    { person: "ex:eve", name: "Eve", age: 32 },
    { person: "ex:frank", name: "Frank", age: 29 },
    { person: "ex:grace", name: "Grace", age: 31 },
    { person: "ex:henry", name: "Henry", age: 26 },
  ];

  const validateQuery = () => {
    const newErrors = [];
    if (!query.toUpperCase().includes('SELECT') && !query.toUpperCase().includes('CONSTRUCT')) {
      newErrors.push({ line: 1, message: "Missing query type (SELECT/CONSTRUCT)" });
    }
    if (!query.toUpperCase().includes('WHERE')) {
      newErrors.push({ line: 1, message: "Missing WHERE clause" });
    }
    setErrors(newErrors);
    return newErrors.length === 0;
  };

  const formatQuery = () => {
    const formatted = query
      .replace(/\s+/g, ' ')
      .replace(/SELECT/gi, 'SELECT')
      .replace(/WHERE/gi, '\nWHERE')
      .replace(/FILTER/gi, '\n  FILTER')
      .replace(/ORDER BY/gi, '\nORDER BY')
      .replace(/LIMIT/gi, '\nLIMIT')
      .replace(/\{/g, ' {\n  ')
      .replace(/\}/g, '\n}')
      .replace(/\.\s+/g, ' .\n  ')
      .trim();
    setQuery(formatted);
  };

  const visualizeGraph = () => {
    setGraphData({
      nodes: [
        { id: "foaf:Person", label: "Person", type: "class" },
        { id: "ex:alice", label: "Alice", type: "instance" },
        { id: "ex:bob", label: "Bob", type: "instance" },
        { id: "ex:charlie", label: "Charlie", type: "instance" },
      ],
      edges: [
        { source: "ex:alice", target: "foaf:Person", label: "a" },
        { source: "ex:bob", target: "foaf:Person", label: "a" },
        { source: "ex:charlie", target: "foaf:Person", label: "a" },
        { source: "ex:alice", target: "ex:bob", label: "knows" },
        { source: "ex:bob", target: "ex:charlie", label: "knows" },
      ]
    });
  };

  const totalPages = Math.ceil(mockResults.length / pageSize);
  const paginatedResults = mockResults.slice((currentPage - 1) * pageSize, currentPage * pageSize);

  return (
    <div className="space-y-6">
      {/* SPARQL Editor */}
      <div className="space-y-2">
        <div className="flex items-center justify-between">
          <label className="text-sm font-medium text-slate-300">SPARQL Editor</label>
          <div className="flex gap-2">
            <Button variant="outline" size="sm" onClick={formatQuery}>
              Format
            </Button>
            <Button variant="outline" size="sm" onClick={validateQuery}>
              Validate
            </Button>
          </div>
        </div>
        <textarea
          value={query}
          onChange={(e) => setQuery(e.target.value)}
          className="w-full h-48 bg-slate-900 border border-slate-600 rounded-lg p-3 text-sm font-mono text-slate-300 focus:ring-2 focus:ring-blue-500"
        />
        {errors.length > 0 && (
          <div className="space-y-1">
            {errors.map((err, i) => (
              <div key={i} className="text-sm text-red-400">
                Line {err.line}: {err.message}
              </div>
            ))}
          </div>
        )}
        {errors.length === 0 && query.length > 0 && (
          <Badge variant="success">Query valid</Badge>
        )}
      </div>

      {/* Graph Visualizer */}
      <div className="bg-slate-900 rounded-lg p-4">
        <div className="flex items-center justify-between mb-3">
          <span className="text-sm font-medium text-slate-300">Graph Visualizer</span>
          <Button variant="outline" size="sm" onClick={visualizeGraph}>
            Generate Graph
          </Button>
        </div>
        {graphData ? (
          <div className="bg-slate-800 rounded p-4">
            <div className="flex flex-wrap gap-4 mb-4">
              {graphData.nodes.map(node => (
                <div
                  key={node.id}
                  className={`px-3 py-2 rounded-lg text-sm ${
                    node.type === "class"
                      ? "bg-purple-500/20 border border-purple-500 text-purple-300"
                      : "bg-blue-500/20 border border-blue-500 text-blue-300"
                  }`}
                >
                  {node.label}
                </div>
              ))}
            </div>
            <div className="text-xs text-slate-400">
              {graphData.nodes.length} nodes, {graphData.edges.length} edges
            </div>
            <div className="mt-2 space-y-1">
              {graphData.edges.map((edge, i) => (
                <div key={i} className="text-xs text-slate-500">
                  {edge.source} --[{edge.label}]--&gt; {edge.target}
                </div>
              ))}
            </div>
          </div>
        ) : (
          <div className="text-center text-slate-500 py-8">
            Click "Generate Graph" to visualize query results
          </div>
        )}
      </div>

      {/* Results Paginator */}
      <div className="bg-slate-900 rounded-lg p-4">
        <div className="text-sm font-medium text-slate-300 mb-3">
          Results Paginator
        </div>
        <table className="w-full text-sm mb-4">
          <thead>
            <tr className="border-b border-slate-700">
              <th className="text-left p-2 text-slate-400">Person</th>
              <th className="text-left p-2 text-slate-400">Name</th>
              <th className="text-left p-2 text-slate-400">Age</th>
            </tr>
          </thead>
          <tbody>
            {paginatedResults.map((row, i) => (
              <tr key={i} className="border-b border-slate-800">
                <td className="p-2 text-blue-400 font-mono text-xs">{row.person}</td>
                <td className="p-2 text-slate-300">{row.name}</td>
                <td className="p-2 text-slate-300">{row.age}</td>
              </tr>
            ))}
          </tbody>
        </table>
        <div className="flex items-center justify-between">
          <span className="text-xs text-slate-400">
            Showing {(currentPage - 1) * pageSize + 1}-{Math.min(currentPage * pageSize, mockResults.length)} of {mockResults.length}
          </span>
          <div className="flex gap-2">
            <Button
              variant="outline"
              size="sm"
              onClick={() => setCurrentPage(p => Math.max(1, p - 1))}
              disabled={currentPage === 1}
            >
              Previous
            </Button>
            <span className="px-3 py-1 text-sm text-slate-400">
              {currentPage} / {totalPages}
            </span>
            <Button
              variant="outline"
              size="sm"
              onClick={() => setCurrentPage(p => Math.min(totalPages, p + 1))}
              disabled={currentPage === totalPages}
            >
              Next
            </Button>
          </div>
        </div>
      </div>

      {/* Additional Form Hooks */}
      <div className="grid grid-cols-2 gap-4">
        {/* useQueryBuilder */}
        <div className="bg-slate-900 rounded-lg p-4">
          <div className="text-sm font-medium text-slate-300 mb-2">useQueryBuilder</div>
          <div className="text-xs text-slate-400 mb-3">Visual SPARQL query construction</div>
          <div className="space-y-2">
            <div className="flex items-center gap-2">
              <Badge variant="outline">Subject</Badge>
              <span className="text-xs text-slate-300">?person</span>
            </div>
            <div className="flex items-center gap-2">
              <Badge variant="outline">Predicate</Badge>
              <span className="text-xs text-slate-300">foaf:name</span>
            </div>
            <div className="flex items-center gap-2">
              <Badge variant="outline">Object</Badge>
              <span className="text-xs text-slate-300">?name</span>
            </div>
          </div>
          <div className="mt-2 text-xs text-green-400">→ SELECT ?person ?name WHERE ...</div>
        </div>

        {/* useFormValidation */}
        <div className="bg-slate-900 rounded-lg p-4">
          <div className="text-sm font-medium text-slate-300 mb-2">useFormValidation</div>
          <div className="text-xs text-slate-400 mb-3">SHACL-based form validation</div>
          <div className="space-y-2">
            <div className="flex justify-between items-center">
              <span className="text-xs text-slate-300">Name (required)</span>
              <Badge variant="success">✓</Badge>
            </div>
            <div className="flex justify-between items-center">
              <span className="text-xs text-slate-300">Email (pattern)</span>
              <Badge variant="success">✓</Badge>
            </div>
            <div className="flex justify-between items-center">
              <span className="text-xs text-slate-300">Age (minValue: 18)</span>
              <Badge variant="destructive">✗ Must be ≥ 18</Badge>
            </div>
          </div>
        </div>
      </div>

      {/* Code Example */}
      <div className="bg-slate-900 rounded-lg p-4">
        <div className="text-xs text-slate-500 mb-2">Usage Example - All Form & UI Hooks</div>
        <pre className="text-xs text-slate-300 overflow-x-auto">
{`import {
  useSPARQLEditor,
  useGraphVisualizer,
  useResultsPaginator,
  useQueryBuilder,
  useFormValidation
} from 'unrdf/react-hooks';

// SPARQL Editor with syntax highlighting
const { query, setQuery, validate, format, errors } = useSPARQLEditor();

// Graph Visualization
const { visualize, nodes, edges, layout } = useGraphVisualizer();

// Paginated results
const { pageData, nextPage, prevPage, currentPage } = useResultsPaginator(results, {
  pageSize: 10
});

// Visual query builder (no SPARQL knowledge needed)
const { addTriple, removeTriple, build, triples } = useQueryBuilder();
addTriple({ subject: '?person', predicate: 'foaf:name', object: '?name' });
const sparql = build(); // Returns valid SPARQL

// SHACL-based form validation
const { validate: validateForm, errors, isValid } = useFormValidation({
  shapes: personShape // SHACL shapes graph
});
const result = await validateForm({ name: 'Alice', age: 17 });
// result.errors = [{ path: 'age', message: 'Must be >= 18' }]`}
        </pre>
      </div>
    </div>
  );
}
