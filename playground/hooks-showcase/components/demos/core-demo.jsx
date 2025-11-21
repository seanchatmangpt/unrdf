"use client";

import { useState, useEffect } from "react";
import { Button } from "@/components/ui/button";
import { Badge } from "@/components/ui/badge";

// Mock the hooks for demo (in production, import from unrdf)
function useKnowledgeEngineMock() {
  const [data, setData] = useState([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);

  const query = async (sparql) => {
    setLoading(true);
    await new Promise(r => setTimeout(r, 500));
    setData([
      { subject: "http://example.org/alice", name: "Alice", age: 30 },
      { subject: "http://example.org/bob", name: "Bob", age: 25 },
      { subject: "http://example.org/charlie", name: "Charlie", age: 35 }
    ]);
    setLoading(false);
  };

  const insert = async (quads) => {
    setLoading(true);
    await new Promise(r => setTimeout(r, 300));
    const newItem = {
      subject: `http://example.org/person-${Date.now()}`,
      name: quads[0]?.name || "New Person",
      age: quads[0]?.age || 20
    };
    setData(prev => [...prev, newItem]);
    setLoading(false);
    return { inserted: 1 };
  };

  const del = async (pattern) => {
    setLoading(true);
    await new Promise(r => setTimeout(r, 200));
    setData(prev => prev.filter(item => item.subject !== pattern.subject));
    setLoading(false);
    return { deleted: 1 };
  };

  return { query, insert, delete: del, data, loading, error };
}

export function CoreDemo() {
  const { query, insert, delete: del, data, loading, error } = useKnowledgeEngineMock();
  const [sparqlQuery, setSparqlQuery] = useState(`SELECT ?person ?name ?age
WHERE {
  ?person a foaf:Person ;
          foaf:name ?name ;
          foaf:age ?age .
}`);

  const handleQuery = () => {
    query(sparqlQuery);
  };

  const handleInsert = () => {
    insert([{ name: `Person-${Math.floor(Math.random() * 1000)}`, age: Math.floor(Math.random() * 50) + 18 }]);
  };

  const handleDelete = (subject) => {
    del({ subject });
  };

  return (
    <div className="space-y-6">
      {/* SPARQL Query Input */}
      <div className="space-y-2">
        <label className="text-sm font-medium text-slate-300">SPARQL Query</label>
        <textarea
          value={sparqlQuery}
          onChange={(e) => setSparqlQuery(e.target.value)}
          className="w-full h-32 bg-slate-900 border border-slate-600 rounded-lg p-3 text-sm font-mono text-slate-300 focus:ring-2 focus:ring-blue-500 focus:border-transparent"
        />
        <div className="flex gap-2">
          <Button onClick={handleQuery} disabled={loading}>
            {loading ? "Loading..." : "Execute Query"}
          </Button>
          <Button variant="secondary" onClick={handleInsert} disabled={loading}>
            Insert Random Person
          </Button>
        </div>
      </div>

      {/* Status */}
      <div className="flex items-center gap-4">
        <Badge variant={loading ? "warning" : "success"}>
          {loading ? "Loading" : "Ready"}
        </Badge>
        <span className="text-sm text-slate-400">
          {data.length} results
        </span>
      </div>

      {/* Results Table */}
      {data.length > 0 && (
        <div className="overflow-x-auto">
          <table className="w-full text-sm">
            <thead>
              <tr className="border-b border-slate-600">
                <th className="text-left p-2 text-slate-400">Subject</th>
                <th className="text-left p-2 text-slate-400">Name</th>
                <th className="text-left p-2 text-slate-400">Age</th>
                <th className="text-left p-2 text-slate-400">Actions</th>
              </tr>
            </thead>
            <tbody>
              {data.map((row, i) => (
                <tr key={i} className="border-b border-slate-700 hover:bg-slate-700/50">
                  <td className="p-2 font-mono text-xs text-blue-400">{row.subject}</td>
                  <td className="p-2 text-slate-300">{row.name}</td>
                  <td className="p-2 text-slate-300">{row.age}</td>
                  <td className="p-2">
                    <Button
                      variant="destructive"
                      size="sm"
                      onClick={() => handleDelete(row.subject)}
                    >
                      Delete
                    </Button>
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      )}

      {/* Additional Hooks Demo */}
      <div className="grid grid-cols-3 gap-4">
        {/* useKnowledgeEngineContext */}
        <div className="bg-slate-900 rounded-lg p-4">
          <div className="text-sm font-medium text-slate-300 mb-2">useKnowledgeEngineContext</div>
          <div className="text-xs text-slate-400 mb-2">Access engine from any component</div>
          <Badge variant="success">Provider Active</Badge>
        </div>

        {/* useTransaction */}
        <div className="bg-slate-900 rounded-lg p-4">
          <div className="text-sm font-medium text-slate-300 mb-2">useTransaction</div>
          <div className="text-xs text-slate-400 mb-2">ACID transactions with rollback</div>
          <div className="flex gap-2">
            <Badge variant="outline">Begin</Badge>
            <Badge variant="outline">Commit</Badge>
            <Badge variant="destructive">Rollback</Badge>
          </div>
        </div>

        {/* useKnowledgeHook */}
        <div className="bg-slate-900 rounded-lg p-4">
          <div className="text-sm font-medium text-slate-300 mb-2">useKnowledgeHook</div>
          <div className="text-xs text-slate-400 mb-2">Policy-driven triggers</div>
          <div className="text-xs text-green-400">3 hooks registered</div>
        </div>
      </div>

      {/* Code Example */}
      <div className="bg-slate-900 rounded-lg p-4">
        <div className="text-xs text-slate-500 mb-2">Usage Example - All Core Hooks</div>
        <pre className="text-xs text-slate-300 overflow-x-auto">
{`import {
  useKnowledgeEngine,
  useKnowledgeEngineContext,
  useTransaction,
  useKnowledgeHook
} from 'unrdf/react-hooks';

// Basic CRUD
const { query, insert, data, loading } = useKnowledgeEngine();

// Access from nested components (no prop drilling)
const { engine } = useKnowledgeEngineContext();

// ACID transactions
const { begin, commit, rollback, isActive } = useTransaction();
await begin();
await insert([...]);
await commit(); // or rollback() on error

// Policy hooks (auto-triggered)
const { registerHook, hooks } = useKnowledgeHook();
registerHook({
  name: 'validate-age',
  when: { kind: 'sparql-ask', query: 'ASK { ?p :age ?a FILTER(?a < 18) }' },
  run: (event) => { if (event.result) throw new Error('Must be 18+'); }
});`}
        </pre>
      </div>
    </div>
  );
}
