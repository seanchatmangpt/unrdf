/**
 * @file playground/hooks-showcase/components/demos/streaming-demo-new.jsx
 * @description Streaming hooks demonstration with REAL real-time updates
 *
 * Uses:
 * - useChangeFeed() - Real-time change notifications via SSE
 * - useKnowledgeEngine() - RDF operations
 * - Live collaboration across tabs
 */

"use client";

import { useState } from "react";
import { Button } from "@/components/ui/button";
import { Badge } from "@/components/ui/badge";
import { useKnowledgeEngine, useChangeFeed } from "../../lib/unrdf-kgc-bridge.mjs";
import { useConnection } from "../../../../packages/kgc-4d/playground/lib/client/hooks.mjs";

export function StreamingDemoNew() {
  const { ready, addTriples } = useKnowledgeEngine();
  const { isConnected } = useConnection();

  // Subscribe to ALL changes in real-time
  const allChanges = useChangeFeed();

  // Subscribe to only ADD operations
  const addChanges = useChangeFeed({
    filter: (change) => change.type === 'add'
  });

  // Subscribe to only foaf:name changes
  const nameChanges = useChangeFeed({
    filter: (change) => change.triple.predicate === 'http://xmlns.com/foaf/0.1/name'
  });

  const [message, setMessage] = useState('');

  const sendMessage = async () => {
    if (!ready || !message) return;

    const messageId = `http://example.org/messages/${Date.now()}`;

    await addTriples([
      {
        subject: messageId,
        predicate: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        object: 'http://example.org/Message'
      },
      {
        subject: messageId,
        predicate: 'http://example.org/text',
        object: message
      },
      {
        subject: messageId,
        predicate: 'http://example.org/timestamp',
        object: new Date().toISOString()
      }
    ]);

    setMessage('');
  };

  const addRandomPerson = async () => {
    const names = ['Alice', 'Bob', 'Carol', 'Dave', 'Eve', 'Frank'];
    const randomName = names[Math.floor(Math.random() * names.length)];
    const personId = `http://example.org/people/${Date.now()}`;

    await addTriples([
      {
        subject: personId,
        predicate: 'http://xmlns.com/foaf/0.1/name',
        object: randomName
      }
    ]);
  };

  return (
    <div className="space-y-6">
      {/* Connection Status */}
      <div className="bg-slate-900 rounded-lg p-4 border border-slate-700">
        <div className="flex items-center gap-3">
          <div className={`w-3 h-3 rounded-full ${isConnected ? 'bg-green-500 animate-pulse' : 'bg-red-500'}`} />
          <div>
            <h3 className="text-sm font-medium text-slate-300">
              Real-Time Change Feed
            </h3>
            <p className="text-xs text-slate-400">
              {isConnected ? 'Connected via SSE (Server-Sent Events)' : 'Disconnected'}
            </p>
          </div>
          <Badge variant={isConnected ? "success" : "secondary"} className="ml-auto">
            {allChanges.length} changes received
          </Badge>
        </div>
      </div>

      {/* Test Actions */}
      <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
        <div className="bg-slate-900 rounded-lg p-4 border border-slate-700 space-y-3">
          <h4 className="text-sm font-medium text-slate-300">Send Message</h4>
          <p className="text-xs text-slate-400">
            Add a message and watch it appear in the change feed
          </p>
          <div className="flex gap-2">
            <input
              type="text"
              placeholder="Type a message..."
              value={message}
              onChange={(e) => setMessage(e.target.value)}
              onKeyPress={(e) => e.key === 'Enter' && sendMessage()}
              className="flex-1 bg-slate-800 border border-slate-600 rounded px-3 py-2 text-sm text-slate-300"
              disabled={!ready}
            />
            <Button onClick={sendMessage} disabled={!ready || !message}>
              Send
            </Button>
          </div>
        </div>

        <div className="bg-slate-900 rounded-lg p-4 border border-slate-700 space-y-3">
          <h4 className="text-sm font-medium text-slate-300">Add Random Person</h4>
          <p className="text-xs text-slate-400">
            Watch the change feed update in real-time
          </p>
          <Button onClick={addRandomPerson} disabled={!ready} className="w-full">
            Add Random Person
          </Button>
        </div>
      </div>

      {/* Change Feed Monitors */}
      <div className="grid grid-cols-1 lg:grid-cols-3 gap-4">
        {/* All Changes */}
        <div className="bg-slate-900 rounded-lg p-4 border border-slate-700">
          <div className="flex items-center justify-between mb-3">
            <h4 className="text-sm font-medium text-slate-300">All Changes</h4>
            <Badge variant="outline">{allChanges.length}</Badge>
          </div>
          <div className="space-y-2 max-h-64 overflow-y-auto">
            {allChanges.slice(-10).reverse().map((change, i) => (
              <div key={i} className="text-xs p-2 bg-slate-800 rounded">
                <div className="flex items-center gap-2 mb-1">
                  <Badge variant={change.type === 'add' ? "success" : "destructive"} className="text-xs">
                    {change.type}
                  </Badge>
                  <span className="text-slate-500 font-mono">
                    {new Date(change.timestamp).toLocaleTimeString()}
                  </span>
                </div>
                <div className="text-slate-400 font-mono truncate">
                  {change.triple.predicate.split('/').pop()}: {change.triple.object}
                </div>
              </div>
            ))}
            {allChanges.length === 0 && (
              <p className="text-center text-slate-500 py-4">No changes yet</p>
            )}
          </div>
        </div>

        {/* Add-Only Changes */}
        <div className="bg-slate-900 rounded-lg p-4 border border-green-900/30 border-green-500/30">
          <div className="flex items-center justify-between mb-3">
            <h4 className="text-sm font-medium text-green-400">Adds Only</h4>
            <Badge variant="success">{addChanges.length}</Badge>
          </div>
          <div className="space-y-2 max-h-64 overflow-y-auto">
            {addChanges.slice(-10).reverse().map((change, i) => (
              <div key={i} className="text-xs p-2 bg-green-900/20 rounded border border-green-800">
                <div className="flex items-center gap-2 mb-1">
                  <span className="text-green-400">+</span>
                  <span className="text-slate-500 font-mono">
                    {new Date(change.timestamp).toLocaleTimeString()}
                  </span>
                </div>
                <div className="text-green-300 font-mono truncate">
                  {change.triple.predicate.split('/').pop()}: {change.triple.object}
                </div>
              </div>
            ))}
            {addChanges.length === 0 && (
              <p className="text-center text-slate-500 py-4">No adds yet</p>
            )}
          </div>
        </div>

        {/* Name Changes Only */}
        <div className="bg-slate-900 rounded-lg p-4 border border-purple-900/30 border-purple-500/30">
          <div className="flex items-center justify-between mb-3">
            <h4 className="text-sm font-medium text-purple-400">Names Only</h4>
            <Badge variant="outline" className="border-purple-500 text-purple-400">
              {nameChanges.length}
            </Badge>
          </div>
          <div className="space-y-2 max-h-64 overflow-y-auto">
            {nameChanges.slice(-10).reverse().map((change, i) => (
              <div key={i} className="text-xs p-2 bg-purple-900/20 rounded border border-purple-800">
                <div className="flex items-center gap-2 mb-1">
                  <span className="text-purple-400">👤</span>
                  <span className="text-slate-500 font-mono">
                    {new Date(change.timestamp).toLocaleTimeString()}
                  </span>
                </div>
                <div className="text-purple-300 font-mono">
                  foaf:name = "{change.triple.object}"
                </div>
              </div>
            ))}
            {nameChanges.length === 0 && (
              <p className="text-center text-slate-500 py-4">No names yet</p>
            )}
          </div>
        </div>
      </div>

      {/* Code Examples */}
      <div className="bg-slate-900 rounded-lg p-4 border border-slate-700">
        <h4 className="text-xs text-slate-500 mb-3">useChangeFeed() Examples</h4>
        <div className="space-y-4">
          <div>
            <p className="text-xs text-slate-400 mb-2">All changes:</p>
            <pre className="text-xs text-slate-300 bg-slate-800 p-3 rounded overflow-x-auto">
{`const changes = useChangeFeed();
// Returns: [{ type: 'add'|'delete', triple: {...}, timestamp, source }]`}
            </pre>
          </div>

          <div>
            <p className="text-xs text-slate-400 mb-2">Filter by operation type:</p>
            <pre className="text-xs text-slate-300 bg-slate-800 p-3 rounded overflow-x-auto">
{`const addChanges = useChangeFeed({
  filter: (change) => change.type === 'add'
});`}
            </pre>
          </div>

          <div>
            <p className="text-xs text-slate-400 mb-2">Filter by predicate:</p>
            <pre className="text-xs text-slate-300 bg-slate-800 p-3 rounded overflow-x-auto">
{`const nameChanges = useChangeFeed({
  filter: (change) =>
    change.triple.predicate === 'http://xmlns.com/foaf/0.1/name'
});`}
            </pre>
          </div>
        </div>
      </div>

      {/* Multi-Tab Test */}
      <div className="bg-blue-900/20 border border-blue-700 rounded-lg p-4">
        <h4 className="text-sm font-medium text-blue-400 mb-2">
          🔄 Multi-Tab Collaboration Test
        </h4>
        <ol className="text-xs text-blue-300 space-y-1 list-decimal list-inside">
          <li>Open this page in a second browser tab</li>
          <li>Click "Add Random Person" in one tab</li>
          <li>Watch the change feed update in BOTH tabs instantly</li>
          <li>This is real-time synchronization via SSE (Tether protocol)</li>
        </ol>
      </div>

      {/* Reality Check */}
      <div className="bg-green-900/20 border border-green-700 rounded-lg p-4">
        <h4 className="text-sm font-medium text-green-400 mb-2">
          ✅ This is REAL - Not Mocked
        </h4>
        <ul className="text-xs text-green-300 space-y-1">
          <li>• useChangeFeed() subscribes to KGC-4D's SSE stream (Tether)</li>
          <li>• Changes propagate via Server-Sent Events in real-time</li>
          <li>• Filter functions run client-side for performance</li>
          <li>• Open in multiple tabs to see live collaboration</li>
          <li>• Backend = @unrdf/kgc-4d with Delta Sync Reducer pattern</li>
        </ul>
      </div>
    </div>
  );
}
