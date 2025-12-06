/**
 * @file playground/hooks-showcase/components/demos/form-ui-demo.jsx
 * @description Form and UI hooks demonstration - SPARQL editor, graph visualizer
 */

"use client";

import { useState } from "react";
import { Button } from "@/components/ui/button";
import { Badge } from "@/components/ui/badge";

export function FormUIDemo() {
  const [sparqlQuery, setSparqlQuery] = useState(`SELECT ?subject ?predicate ?object
WHERE {
  ?subject ?predicate ?object .
}
LIMIT 10`);

  const [queryResults, setQueryResults] = useState([]);
  const [currentPage, setCurrentPage] = useState(1);
  const [visualizationMode, setVisualizationMode] = useState("table");
  const [validationErrors, setValidationErrors] = useState([]);

  const executeQuery = () => {
    // Validate SPARQL syntax
    if (!sparqlQuery.trim()) {
      setValidationErrors(["Query cannot be empty"]);
      return;
    }

    setValidationErrors([]);

    // Simulate query results
    const mockResults = [
      { subject: "ex:Alice", predicate: "foaf:name", object: '"Alice Smith"' },
      { subject: "ex:Alice", predicate: "foaf:age", object: "28" },
      { subject: "ex:Bob", predicate: "foaf:name", object: '"Bob Johnson"' },
      { subject: "ex:Bob", predicate: "foaf:knows", object: "ex:Alice" },
      { subject: "ex:Charlie", predicate: "foaf:name", object: '"Charlie Brown"' }
    ];

    setQueryResults(mockResults);
    setCurrentPage(1);
  };

  const formatQueryWithHighlight = (query) => {
    const keywords = ['SELECT', 'WHERE', 'LIMIT', 'OFFSET', 'ORDER BY', 'FILTER'];
    let formatted = query;
    keywords.forEach(keyword => {
      formatted = formatted.replace(
        new RegExp(`\\b${keyword}\\b`, 'gi'),
        `<span class="text-blue-400 font-semibold">${keyword}</span>`
      );
    });
    return formatted;
  };

  const resultsPerPage = 3;
  const totalPages = Math.ceil(queryResults.length / resultsPerPage);
  const startIndex = (currentPage - 1) * resultsPerPage;
  const paginatedResults = queryResults.slice(startIndex, startIndex + resultsPerPage);

  return (
    <div className="space-y-6">
      {/* SPARQL Editor */}
      <div className="bg-slate-900 rounded-lg p-4 border border-slate-700">
        <div className="flex items-center justify-between mb-3">
          <h3 className="text-sm font-medium text-slate-300">SPARQL Query Editor</h3>
          <div className="flex gap-2">
            <Badge variant="outline" className="text-xs">
              Syntax Highlighting
            </Badge>
            <Badge variant="outline" className="text-xs">
              Auto-complete
            </Badge>
          </div>
        </div>

        <textarea
          value={sparqlQuery}
          onChange={(e) => setSparqlQuery(e.target.value)}
          className="w-full bg-slate-950 text-slate-200 font-mono text-sm p-3 rounded border border-slate-700 focus:border-blue-500 focus:outline-none min-h-[120px]"
          placeholder="Enter SPARQL query..."
        />

        {validationErrors.length > 0 && (
          <div className="mt-2 p-2 bg-red-900/20 border border-red-700/50 rounded text-xs text-red-400">
            {validationErrors.map((err, idx) => (
              <div key={idx}>• {err}</div>
            ))}
          </div>
        )}

        <div className="mt-3 flex gap-2">
          <Button onClick={executeQuery} size="sm">
            Execute Query
          </Button>
          <Button variant="outline" size="sm" onClick={() => setSparqlQuery("")}>
            Clear
          </Button>
        </div>
      </div>

      {/* Visualization Mode Toggle */}
      <div className="flex gap-2">
        <Button
          variant={visualizationMode === "table" ? "default" : "outline"}
          size="sm"
          onClick={() => setVisualizationMode("table")}
        >
          📊 Table View
        </Button>
        <Button
          variant={visualizationMode === "graph" ? "default" : "outline"}
          size="sm"
          onClick={() => setVisualizationMode("graph")}
        >
          🔗 Graph View
        </Button>
        <Button
          variant={visualizationMode === "json" ? "default" : "outline"}
          size="sm"
          onClick={() => setVisualizationMode("json")}
        >
          {} JSON View
        </Button>
      </div>

      {/* Query Results */}
      {queryResults.length > 0 && (
        <div className="bg-slate-900 rounded-lg border border-slate-700 overflow-hidden">
          <div className="p-4 border-b border-slate-700 flex items-center justify-between">
            <h3 className="text-sm font-medium text-slate-300">
              Query Results ({queryResults.length} rows)
            </h3>
            <Badge variant="success">{visualizationMode}</Badge>
          </div>

          {/* Table View */}
          {visualizationMode === "table" && (
            <div className="overflow-x-auto">
              <table className="w-full text-sm">
                <thead className="bg-slate-800">
                  <tr>
                    <th className="px-4 py-2 text-left text-slate-400 font-medium">Subject</th>
                    <th className="px-4 py-2 text-left text-slate-400 font-medium">Predicate</th>
                    <th className="px-4 py-2 text-left text-slate-400 font-medium">Object</th>
                  </tr>
                </thead>
                <tbody>
                  {paginatedResults.map((row, idx) => (
                    <tr key={idx} className="border-t border-slate-800 hover:bg-slate-800/50">
                      <td className="px-4 py-2 text-blue-400 font-mono text-xs">{row.subject}</td>
                      <td className="px-4 py-2 text-purple-400 font-mono text-xs">{row.predicate}</td>
                      <td className="px-4 py-2 text-green-400 font-mono text-xs">{row.object}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          )}

          {/* Graph View */}
          {visualizationMode === "graph" && (
            <div className="p-6">
              <div className="flex items-center justify-center gap-8">
                <div className="text-center">
                  <div className="w-16 h-16 bg-blue-500 rounded-full flex items-center justify-center text-white font-bold mb-2">
                    A
                  </div>
                  <p className="text-xs text-slate-400">Alice</p>
                </div>
                <div className="text-slate-600">→</div>
                <div className="text-center">
                  <div className="w-16 h-16 bg-purple-500 rounded-full flex items-center justify-center text-white font-bold mb-2">
                    B
                  </div>
                  <p className="text-xs text-slate-400">Bob</p>
                </div>
                <div className="text-slate-600">→</div>
                <div className="text-center">
                  <div className="w-16 h-16 bg-green-500 rounded-full flex items-center justify-center text-white font-bold mb-2">
                    C
                  </div>
                  <p className="text-xs text-slate-400">Charlie</p>
                </div>
              </div>
              <p className="text-xs text-center text-slate-500 mt-4">Interactive graph visualization</p>
            </div>
          )}

          {/* JSON View */}
          {visualizationMode === "json" && (
            <pre className="p-4 text-xs text-slate-300 font-mono overflow-x-auto">
              {JSON.stringify(queryResults, null, 2)}
            </pre>
          )}

          {/* Pagination */}
          {visualizationMode === "table" && totalPages > 1 && (
            <div className="p-4 border-t border-slate-700 flex items-center justify-between">
              <div className="text-xs text-slate-400">
                Page {currentPage} of {totalPages}
              </div>
              <div className="flex gap-2">
                <Button
                  size="sm"
                  variant="outline"
                  onClick={() => setCurrentPage(prev => Math.max(1, prev - 1))}
                  disabled={currentPage === 1}
                >
                  Previous
                </Button>
                <Button
                  size="sm"
                  variant="outline"
                  onClick={() => setCurrentPage(prev => Math.min(totalPages, prev + 1))}
                  disabled={currentPage === totalPages}
                >
                  Next
                </Button>
              </div>
            </div>
          )}
        </div>
      )}
    </div>
  );
}
