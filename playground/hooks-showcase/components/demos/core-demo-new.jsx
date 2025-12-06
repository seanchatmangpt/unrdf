/**
 * @file playground/hooks-showcase/components/demos/core-demo-new.jsx
 * @description Core hooks demonstration with REAL UNRDF hooks + KGC-4D backend
 *
 * Now uses:
 * - useKnowledgeEngine() - Real RDF operations
 * - useTriples() - Real triple queries
 * - Real data persisted in KGC-4D Universe
 */

"use client";

import { useState, useEffect } from "react";
import { Button } from "@/components/ui/button";
import { Badge } from "@/components/ui/badge";
import { useKnowledgeEngine, useTriples } from "../../lib/unrdf-kgc-bridge.mjs";
import { useConnection } from "../../../../packages/kgc-4d/playground/lib/client/hooks.mjs";

export function CoreDemoNew() {
  const { ready, addTriples, deleteTriples, quadCount, timestamp } = useKnowledgeEngine();
  const { state: connectionState, isConnected } = useConnection();

  const [newPersonName, setNewPersonName] = useState('');
  const [newPersonAge, setNewPersonAge] = useState(25);

  // Query all people from the RDF store
  const { data: people, isLoading, count } = useTriples({
    predicate: 'http://xmlns.com/foaf/0.1/name'
  });

  // Initialize with sample data on first load
  useEffect(() => {
    if (ready && quadCount === 0) {
      // Add some initial people
      addTriples([
        {
          subject: 'http://example.org/people/alice',
          predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          object: 'http://xmlns.com/foaf/0.1/Person'
        },
        {
          subject: 'http://example.org/people/alice',
          predicate: 'http://xmlns.com/foaf/0.1/name',
          object: 'Alice Johnson'
        },
        {
          subject: 'http://example.org/people/alice',
          predicate: 'http://xmlns.com/foaf/0.1/age',
          object: '30'
        },
        {
          subject: 'http://example.org/people/bob',
          predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          object: 'http://xmlns.com/foaf/0.1/Person'
        },
        {
          subject: 'http://example.org/people/bob',
          predicate: 'http://xmlns.com/foaf/0.1/name',
          object: 'Bob Smith'
        },
        {
          subject: 'http://example.org/people/bob',
          predicate: 'http://xmlns.com/foaf/0.1/age',
          object: '25'
        }
      ]);
    }
  }, [ready, quadCount, addTriples]);

  const handleAddPerson = async () => {
    if (!ready || !newPersonName) return;

    const personId = `http://example.org/people/${Date.now()}`;

    await addTriples([
      {
        subject: personId,
        predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        object: 'http://xmlns.com/foaf/0.1/Person'
      },
      {
        subject: personId,
        predicate: 'http://xmlns.com/foaf/0.1/name',
        object: newPersonName
      },
      {
        subject: personId,
        predicate: 'http://xmlns.com/foaf/0.1/age',
        object: String(newPersonAge)
      }
    ]);

    setNewPersonName('');
    setNewPersonAge(25);
  };

  const handleDeletePerson = async (subject) => {
    if (!ready) return;

    // Find all triples for this person
    const allTriples = useTriples({ subject }).data;

    await deleteTriples(allTriples.map(t => ({
      subject: t.subject,
      predicate: t.predicate,
      object: t.object
    })));
  };

  return (
    <div className="space-y-6">
      {/* Connection Status */}
      <div className="bg-slate-900 rounded-lg p-4 border border-slate-700">
        <div className="flex items-center justify-between">
          <div>
            <h3 className="text-sm font-medium text-slate-300 mb-1">Knowledge Engine Status</h3>
            <div className="flex items-center gap-3">
              <Badge variant={isConnected ? "success" : "secondary"}>
                {isConnected ? "● Connected to Universe" : "○ Disconnected"}
              </Badge>
              <span className="text-xs text-slate-400">
                State: {connectionState}
              </span>
              <span className="text-xs text-slate-400">
                {quadCount} quads in shard
              </span>
            </div>
          </div>
          <div className="text-right">
            <div className="text-xs text-slate-500">Last Update</div>
            <div className="text-xs text-slate-400 font-mono">
              {timestamp ? new Date(timestamp).toLocaleTimeString() : 'N/A'}
            </div>
          </div>
        </div>
      </div>

      {/* Add Person Form */}
      <div className="bg-slate-900 rounded-lg p-4 border border-slate-700 space-y-3">
        <h3 className="text-sm font-medium text-slate-300 mb-3">
          Add Person to RDF Store
        </h3>
        <div className="flex gap-3">
          <input
            type="text"
            placeholder="Name"
            value={newPersonName}
            onChange={(e) => setNewPersonName(e.target.value)}
            className="flex-1 bg-slate-800 border border-slate-600 rounded px-3 py-2 text-sm text-slate-300 focus:ring-2 focus:ring-blue-500 focus:border-transparent"
            disabled={!ready}
          />
          <input
            type="number"
            placeholder="Age"
            value={newPersonAge}
            onChange={(e) => setNewPersonAge(Number(e.target.value))}
            className="w-24 bg-slate-800 border border-slate-600 rounded px-3 py-2 text-sm text-slate-300 focus:ring-2 focus:ring-blue-500 focus:border-transparent"
            disabled={!ready}
          />
          <Button
            onClick={handleAddPerson}
            disabled={!ready || !newPersonName}
            className="bg-blue-600 hover:bg-blue-700"
          >
            Add Person
          </Button>
        </div>
        <p className="text-xs text-slate-500">
          💡 This creates real RDF triples in the KGC-4D Universe
        </p>
      </div>

      {/* People List */}
      <div className="bg-slate-900 rounded-lg p-4 border border-slate-700">
        <div className="flex items-center justify-between mb-4">
          <h3 className="text-sm font-medium text-slate-300">
            People in Knowledge Graph
          </h3>
          <Badge variant="outline">
            {count} {count === 1 ? 'person' : 'people'}
          </Badge>
        </div>

        {isLoading ? (
          <div className="text-center text-slate-400 py-8">Loading from Universe...</div>
        ) : people && people.length > 0 ? (
          <div className="overflow-x-auto">
            <table className="w-full text-sm">
              <thead>
                <tr className="border-b border-slate-700">
                  <th className="text-left p-2 text-slate-400 font-medium">Subject (URI)</th>
                  <th className="text-left p-2 text-slate-400 font-medium">Name</th>
                  <th className="text-left p-2 text-slate-400 font-medium">Actions</th>
                </tr>
              </thead>
              <tbody>
                {people.map((person, i) => (
                  <tr key={i} className="border-b border-slate-800 hover:bg-slate-800/50">
                    <td className="p-2 font-mono text-xs text-blue-400 truncate max-w-xs">
                      {person.subject}
                    </td>
                    <td className="p-2 text-slate-300">
                      {person.object}
                    </td>
                    <td className="p-2">
                      <Button
                        variant="destructive"
                        size="sm"
                        onClick={() => handleDeletePerson(person.subject)}
                        disabled={!ready}
                      >
                        Delete
                      </Button>
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        ) : (
          <div className="text-center text-slate-500 py-8">
            No people yet. Add one above!
          </div>
        )}
      </div>

      {/* Hook Information */}
      <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
        <div className="bg-slate-900 rounded-lg p-4 border border-slate-700">
          <h4 className="text-sm font-medium text-slate-300 mb-2">
            useKnowledgeEngine()
          </h4>
          <p className="text-xs text-slate-400 mb-3">
            Core hook for RDF CRUD operations
          </p>
          <div className="space-y-1 text-xs">
            <div className="flex justify-between">
              <span className="text-slate-500">Status:</span>
              <span className="text-green-400">{ready ? 'Ready' : 'Loading'}</span>
            </div>
            <div className="flex justify-between">
              <span className="text-slate-500">Quads:</span>
              <span className="text-blue-400">{quadCount}</span>
            </div>
          </div>
        </div>

        <div className="bg-slate-900 rounded-lg p-4 border border-slate-700">
          <h4 className="text-sm font-medium text-slate-300 mb-2">
            useTriples()
          </h4>
          <p className="text-xs text-slate-400 mb-3">
            Query triples by pattern
          </p>
          <div className="space-y-1 text-xs">
            <div className="flex justify-between">
              <span className="text-slate-500">Pattern:</span>
              <span className="text-purple-400">foaf:name</span>
            </div>
            <div className="flex justify-between">
              <span className="text-slate-500">Results:</span>
              <span className="text-blue-400">{count}</span>
            </div>
          </div>
        </div>

        <div className="bg-slate-900 rounded-lg p-4 border border-slate-700">
          <h4 className="text-sm font-medium text-slate-300 mb-2">
            KGC-4D Backend
          </h4>
          <p className="text-xs text-slate-400 mb-3">
            Shard/Universe/Tether architecture
          </p>
          <div className="space-y-1 text-xs">
            <div className="flex justify-between">
              <span className="text-slate-500">Connection:</span>
              <span className="text-green-400">SSE (Tether)</span>
            </div>
            <div className="flex justify-between">
              <span className="text-slate-500">Sync:</span>
              <span className="text-blue-400">Real-time</span>
            </div>
          </div>
        </div>
      </div>

      {/* Code Example */}
      <div className="bg-slate-900 rounded-lg p-4 border border-slate-700">
        <h4 className="text-xs text-slate-500 mb-2">Live Code Example</h4>
        <pre className="text-xs text-slate-300 overflow-x-auto">
{`import { useKnowledgeEngine, useTriples } from 'unrdf-react';

function PeopleList() {
  // Connect to KGC-4D Universe
  const { ready, addTriples, deleteTriples } = useKnowledgeEngine();

  // Query people (auto-updates via SSE)
  const { data: people } = useTriples({
    predicate: 'http://xmlns.com/foaf/0.1/name'
  });

  // Add person (persisted to Universe)
  const addPerson = async (name, age) => {
    await addTriples([
      { subject: \`http://example.org/people/\${Date.now()}\`,
        predicate: 'foaf:name', object: name },
      { subject: \`http://example.org/people/\${Date.now()}\`,
        predicate: 'foaf:age', object: String(age) }
    ]);
  };

  return ready ? (
    <ul>{people?.map(p => <li>{p.object}</li>)}</ul>
  ) : <div>Connecting to Universe...</div>;
}`}
        </pre>
      </div>

      {/* Reality Check */}
      <div className="bg-green-900/20 border border-green-700 rounded-lg p-4">
        <h4 className="text-sm font-medium text-green-400 mb-2">
          ✅ This is REAL - Not Mocked
        </h4>
        <ul className="text-xs text-green-300 space-y-1">
          <li>• Data persists in KGC-4D Universe (server-side RDF store)</li>
          <li>• Changes sync real-time via SSE (Server-Sent Events)</li>
          <li>• Open this page in multiple tabs to see live collaboration</li>
          <li>• useKnowledgeEngine() = real UNRDF hooks, not mocks</li>
          <li>• Backend = @unrdf/kgc-4d with Shard/Universe/Tether pattern</li>
        </ul>
      </div>
    </div>
  );
}
