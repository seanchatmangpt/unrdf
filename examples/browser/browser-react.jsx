/**
 * UNRDF v3.1.0 React Example
 *
 * This example demonstrates using UNRDF in a React application with:
 * - React hooks for state management
 * - IndexedDB persistent storage
 * - Web Worker query execution
 * - Performance profiling
 *
 * To use:
 * 1. npm install unrdf react react-dom
 * 2. Add to your bundler (Webpack, Vite, etc.)
 * 3. Configure browser entry point (see BROWSER-COMPATIBILITY.md)
 */

import React, { useState, useEffect, useCallback } from 'react';
import { createBrowserKnowledgeEngine, parseTurtle } from 'unrdf/browser';

// Custom hook for UNRDF knowledge engine
function useKnowledgeEngine(options = {}) {
  const [engine, setEngine] = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);

  useEffect(() => {
    let mounted = true;

    async function initialize() {
      try {
        const eng = await createBrowserKnowledgeEngine({
          storage: {
            type: 'indexeddb',
            name: options.dbName || 'react-knowledge-graph',
            quota: options.quota || 100 * 1024 * 1024, // 100MB
          },
          workers: {
            enabled: true,
            maxWorkers: navigator.hardwareConcurrency || 4,
          },
          profiling: {
            enabled: true,
            slowQueryThreshold: 100,
          },
          ...options,
        });

        if (mounted) {
          setEngine(eng);
          setLoading(false);
        }
      } catch (err) {
        if (mounted) {
          setError(err);
          setLoading(false);
        }
      }
    }

    initialize();

    return () => {
      mounted = false;
      if (engine) {
        engine.close().catch(console.error);
      }
    };
  }, [options.dbName, options.quota]);

  return { engine, loading, error };
}

// Custom hook for SPARQL queries
function useQuery(engine, queryString) {
  const [data, setData] = useState(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);

  const execute = useCallback(async () => {
    if (!engine || !queryString) return;

    setLoading(true);
    setError(null);

    try {
      const results = await engine.query({
        query: queryString,
        type: 'sparql-select',
      });
      setData(results);
    } catch (err) {
      setError(err);
    } finally {
      setLoading(false);
    }
  }, [engine, queryString]);

  return { data, loading, error, execute };
}

// Main App Component
export default function App() {
  const {
    engine,
    loading: engineLoading,
    error: engineError,
  } = useKnowledgeEngine({
    dbName: 'react-demo-graph',
  });

  const [status, setStatus] = useState('Initializing...');
  const [tripleCount, setTripleCount] = useState(0);
  const [queryResults, setQueryResults] = useState(null);
  const [profile, setProfile] = useState(null);

  // Update stats
  const updateStats = useCallback(async () => {
    if (!engine) return;

    try {
      const countResult = await engine.query({
        query: 'SELECT (COUNT(*) as ?count) WHERE { ?s ?p ?o }',
        type: 'sparql-select',
      });
      setTripleCount(countResult[0]?.count || 0);
    } catch (err) {
      console.error('Failed to update stats:', err);
    }
  }, [engine]);

  useEffect(() => {
    if (engine) {
      setStatus('Ready ✅');
      updateStats();
    }
  }, [engine, updateStats]);

  // Add sample data
  const handleAddData = async () => {
    if (!engine) return;

    setStatus('Adding sample data...');

    try {
      const ttl = `
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:alice a foaf:Person ;
         foaf:name "Alice" ;
         foaf:age "30"^^xsd:integer ;
         foaf:knows ex:bob .

ex:bob a foaf:Person ;
       foaf:name "Bob" ;
       foaf:age "25"^^xsd:integer ;
       foaf:knows ex:alice .

ex:charlie a foaf:Person ;
           foaf:name "Charlie" ;
           foaf:age "35"^^xsd:integer .
      `;

      const store = await parseTurtle(ttl);

      await engine.executeTransaction({
        additions: [...store],
        removals: [],
        actor: 'react-app',
      });

      setStatus(`Added ${[...store].length} triples ✅`);
      await updateStats();
    } catch (err) {
      setStatus(`Error: ${err.message}`);
      console.error(err);
    }
  };

  // Run query
  const handleQuery = async () => {
    if (!engine) return;

    setStatus('Running query...');

    try {
      const results = await engine.query({
        query: `
          PREFIX foaf: <http://xmlns.com/foaf/0.1/>
          SELECT ?person ?name ?age
          WHERE {
            ?person a foaf:Person ;
                    foaf:name ?name ;
                    foaf:age ?age .
          }
          ORDER BY DESC(?age)
        `,
        type: 'sparql-select',
      });

      setQueryResults(results);
      setStatus(`Query returned ${results.length} results ✅`);
    } catch (err) {
      setStatus(`Query error: ${err.message}`);
      console.error(err);
    }
  };

  // Show performance profile
  const handleProfile = async () => {
    if (!engine) return;

    setStatus('Generating performance profile...');

    try {
      const perf = await engine.getPerformanceProfile();
      setProfile(perf);
      setStatus('Performance profile ready ✅');
    } catch (err) {
      setStatus(`Profiling error: ${err.message}`);
      console.error(err);
    }
  };

  // Clear all data
  const handleClear = async () => {
    if (!engine) return;

    if (!window.confirm('Clear all data? This cannot be undone.')) {
      return;
    }

    setStatus('Clearing all data...');

    try {
      await engine.clearAll();
      setQueryResults(null);
      setProfile(null);
      await updateStats();
      setStatus('Data cleared ✅');
    } catch (err) {
      setStatus(`Clear error: ${err.message}`);
      console.error(err);
    }
  };

  // Loading state
  if (engineLoading) {
    return (
      <div style={styles.container}>
        <h1>UNRDF React Example</h1>
        <div style={styles.card}>
          <p>Initializing knowledge engine...</p>
        </div>
      </div>
    );
  }

  // Error state
  if (engineError) {
    return (
      <div style={styles.container}>
        <h1>UNRDF React Example</h1>
        <div style={{ ...styles.card, background: '#f8d7da', color: '#721c24' }}>
          <h2>Initialization Error</h2>
          <p>{engineError.message}</p>
          <pre>{engineError.stack}</pre>
        </div>
      </div>
    );
  }

  return (
    <div style={styles.container}>
      <h1>UNRDF v3.1.0 React Example</h1>
      <p style={styles.subtitle}>React + IndexedDB + Web Workers</p>

      {/* Status Card */}
      <div style={styles.card}>
        <h2>System Status</h2>
        <div style={styles.status}>{status}</div>
        <div style={styles.stats}>
          <div style={styles.statCard}>
            <div style={styles.statLabel}>Engine Status</div>
            <div style={styles.statValue}>Ready ✅</div>
          </div>
          <div style={styles.statCard}>
            <div style={styles.statLabel}>Total Triples</div>
            <div style={styles.statValue}>{tripleCount}</div>
          </div>
        </div>
      </div>

      {/* Actions Card */}
      <div style={styles.card}>
        <h2>Actions</h2>
        <div style={styles.buttonGroup}>
          <button onClick={handleAddData} style={styles.button}>
            Add Sample Data
          </button>
          <button onClick={handleQuery} style={styles.button}>
            Run Query
          </button>
          <button onClick={handleProfile} style={styles.button}>
            Show Performance
          </button>
          <button onClick={handleClear} style={{ ...styles.button, background: '#dc3545' }}>
            Clear All Data
          </button>
        </div>
      </div>

      {/* Query Results */}
      {queryResults && (
        <div style={styles.card}>
          <h2>Query Results ({queryResults.length} rows)</h2>
          <table style={styles.table}>
            <thead>
              <tr>
                {Object.keys(queryResults[0] || {}).map(key => (
                  <th key={key} style={styles.th}>
                    {key}
                  </th>
                ))}
              </tr>
            </thead>
            <tbody>
              {queryResults.map((row, i) => (
                <tr key={i} style={styles.tr}>
                  {Object.values(row).map((val, j) => (
                    <td key={j} style={styles.td}>
                      {val}
                    </td>
                  ))}
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      )}

      {/* Performance Profile */}
      {profile && (
        <div style={styles.card}>
          <h2>Performance Profile</h2>
          <div style={styles.stats}>
            <div style={styles.statCard}>
              <div style={styles.statLabel}>p50 Latency</div>
              <div style={styles.statValue}>{profile.latency.p50.toFixed(2)}ms</div>
            </div>
            <div style={styles.statCard}>
              <div style={styles.statLabel}>p95 Latency</div>
              <div style={styles.statValue}>{profile.latency.p95.toFixed(2)}ms</div>
            </div>
            <div style={styles.statCard}>
              <div style={styles.statLabel}>Heap Used</div>
              <div style={styles.statValue}>
                {(profile.memory.heapUsed / 1024 / 1024).toFixed(2)}MB
              </div>
            </div>
            <div style={styles.statCard}>
              <div style={styles.statLabel}>Cache Hit Rate</div>
              <div style={styles.statValue}>{(profile.cache.hitRate * 100).toFixed(1)}%</div>
            </div>
          </div>
        </div>
      )}
    </div>
  );
}

// Styles
const styles = {
  container: {
    maxWidth: '1200px',
    margin: '0 auto',
    padding: '20px',
    fontFamily: '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif',
  },
  subtitle: {
    color: '#666',
    marginBottom: '30px',
  },
  card: {
    background: 'white',
    borderRadius: '8px',
    padding: '20px',
    marginBottom: '20px',
    boxShadow: '0 2px 4px rgba(0,0,0,0.1)',
  },
  status: {
    padding: '10px',
    background: '#d1ecf1',
    color: '#0c5460',
    borderRadius: '4px',
    marginBottom: '15px',
  },
  stats: {
    display: 'grid',
    gridTemplateColumns: 'repeat(auto-fit, minmax(200px, 1fr))',
    gap: '15px',
  },
  statCard: {
    background: '#f8f9fa',
    padding: '15px',
    borderRadius: '4px',
    borderLeft: '4px solid #007bff',
  },
  statLabel: {
    fontSize: '12px',
    color: '#666',
    textTransform: 'uppercase',
    marginBottom: '5px',
  },
  statValue: {
    fontSize: '24px',
    fontWeight: 'bold',
    color: '#333',
  },
  buttonGroup: {
    display: 'flex',
    gap: '10px',
    flexWrap: 'wrap',
  },
  button: {
    padding: '10px 20px',
    border: 'none',
    borderRadius: '4px',
    background: '#007bff',
    color: 'white',
    cursor: 'pointer',
    fontSize: '14px',
    fontWeight: '500',
  },
  table: {
    width: '100%',
    borderCollapse: 'collapse',
    marginTop: '10px',
  },
  th: {
    padding: '10px',
    textAlign: 'left',
    borderBottom: '2px solid #dee2e6',
    background: '#f8f9fa',
    fontWeight: '600',
  },
  tr: {
    borderBottom: '1px solid #dee2e6',
  },
  td: {
    padding: '10px',
    textAlign: 'left',
  },
};
