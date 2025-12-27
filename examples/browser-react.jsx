/**
 * UNRDF Browser Demo - React Integration
 *
 * React component demonstrating UNRDF browser capabilities:
 * - Parse and store RDF data
 * - Execute SPARQL queries
 * - Manage lockchain audit trail
 *
 * Usage:
 *   import { UnrdfDemo } from './examples/browser-react.jsx';
 *   <UnrdfDemo />
 */

import React, { useState, useEffect, useCallback } from 'react';
import { IndexedDBQuadStore } from '../src/browser/indexeddb-store.mjs';
import { BrowserQueryExecutor } from '../src/browser/comunica-browser-adapter.mjs';
import { BrowserLockchainWriter } from '../src/browser/browser-lockchain-writer.mjs';
import { Parser } from 'n3';

const SAMPLE_TURTLE = `@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix ex: <http://example.org/> .

ex:alice a foaf:Person ;
  foaf:name "Alice Wonderland" ;
  foaf:age 30 ;
  foaf:knows ex:bob .

ex:bob a foaf:Person ;
  foaf:name "Bob Builder" ;
  foaf:age 35 .`;

const SAMPLE_QUERY = `SELECT ?person ?name ?age WHERE {
  ?person a <http://xmlns.com/foaf/0.1/Person> ;
          <http://xmlns.com/foaf/0.1/name> ?name .
  OPTIONAL { ?person <http://xmlns.com/foaf/0.1/age> ?age }
}`;

/**
 * Custom hook for UNRDF browser integration
 */
export function useUnrdf() {
  const [store] = useState(() => new IndexedDBQuadStore());
  const [queryExecutor] = useState(() => new BrowserQueryExecutor(store));
  const [lockchain] = useState(() => new BrowserLockchainWriter());
  const [isInitialized, setIsInitialized] = useState(false);
  const [stats, setStats] = useState({ quads: 0, graphs: 0, commits: 0 });

  useEffect(() => {
    async function init() {
      try {
        await store.init();
        await queryExecutor.init();
        await lockchain.init();
        setIsInitialized(true);
        await updateStats();
      } catch (error) {
        console.error('Failed to initialize UNRDF:', error);
      }
    }

    init();

    return () => {
      store.close();
      queryExecutor.close();
      lockchain.close();
    };
  }, [store, queryExecutor, lockchain]);

  const updateStats = useCallback(async () => {
    if (!isInitialized) return;

    try {
      const quadCount = await store.size();
      const graphs = await store.getGraphs();
      const history = await lockchain.getHistory(Infinity);

      setStats({
        quads: quadCount,
        graphs: graphs.length,
        commits: history.length,
      });
    } catch (error) {
      console.error('Failed to update stats:', error);
    }
  }, [isInitialized, store, lockchain]);

  const parseAndStore = useCallback(async (turtleData) => {
    if (!isInitialized) throw new Error('Not initialized');

    const parser = new Parser();
    const quads = parser.parse(turtleData);
    await store.addQuads(quads);

    await lockchain.recordChange({
      type: 'add',
      data: turtleData,
      author: 'react-user',
      message: `Added ${quads.length} quads`,
    });

    await updateStats();
    return quads.length;
  }, [isInitialized, store, lockchain, updateStats]);

  const executeQuery = useCallback(async (queryString) => {
    if (!isInitialized) throw new Error('Not initialized');

    return await queryExecutor.query(queryString);
  }, [isInitialized, queryExecutor]);

  const getHistory = useCallback(async (limit = 10) => {
    if (!isInitialized) throw new Error('Not initialized');

    return await lockchain.getHistory(limit);
  }, [isInitialized, lockchain]);

  const verifyChain = useCallback(async () => {
    if (!isInitialized) throw new Error('Not initialized');

    return await lockchain.verifyChain();
  }, [isInitialized, lockchain]);

  const clearStore = useCallback(async () => {
    if (!isInitialized) throw new Error('Not initialized');

    await store.clear();
    await updateStats();
  }, [isInitialized, store, updateStats]);

  return {
    isInitialized,
    stats,
    parseAndStore,
    executeQuery,
    getHistory,
    verifyChain,
    clearStore,
    updateStats,
  };
}

/**
 * UNRDF Demo Component
 */
export function UnrdfDemo() {
  const {
    isInitialized,
    stats,
    parseAndStore,
    executeQuery,
    getHistory,
    verifyChain,
    clearStore,
  } = useUnrdf();

  const [turtleInput, setTurtleInput] = useState(SAMPLE_TURTLE);
  const [sparqlInput, setSparqlInput] = useState(SAMPLE_QUERY);
  const [queryResults, setQueryResults] = useState(null);
  const [lockchainOutput, setLockchainOutput] = useState(null);
  const [error, setError] = useState(null);

  const handleParse = async () => {
    try {
      setError(null);
      const count = await parseAndStore(turtleInput);
      alert(`✅ Parsed and stored ${count} quads`);
    } catch (err) {
      setError('Parse error: ' + err.message);
    }
  };

  const handleQuery = async () => {
    try {
      setError(null);
      setQueryResults({ loading: true });

      const result = await executeQuery(sparqlInput);
      setQueryResults(result);
    } catch (err) {
      setError('Query error: ' + err.message);
      setQueryResults(null);
    }
  };

  const handleHistory = async () => {
    try {
      setError(null);
      const history = await getHistory(10);
      setLockchainOutput({ type: 'history', data: history });
    } catch (err) {
      setError('History error: ' + err.message);
    }
  };

  const handleVerify = async () => {
    try {
      setError(null);
      const verification = await verifyChain();
      setLockchainOutput({ type: 'verify', data: verification });
    } catch (err) {
      setError('Verification error: ' + err.message);
    }
  };

  const handleClear = async () => {
    if (confirm('Clear all data?')) {
      try {
        setError(null);
        await clearStore();
        setQueryResults(null);
        setLockchainOutput(null);
        alert('✅ Store cleared');
      } catch (err) {
        setError('Clear error: ' + err.message);
      }
    }
  };

  if (!isInitialized) {
    return (
      <div style={styles.loading}>
        <h2>⏳ Initializing UNRDF...</h2>
      </div>
    );
  }

  return (
    <div style={styles.container}>
      <header style={styles.header}>
        <h1>🌐 UNRDF Browser Demo</h1>
        <p style={styles.subtitle}>RDF Knowledge Graphs in Your Browser - React</p>
      </header>

      {error && (
        <div style={styles.error}>
          <strong>❌ Error:</strong> {error}
        </div>
      )}

      {/* Statistics */}
      <div style={styles.stats}>
        <div style={styles.statCard}>
          <div style={styles.statValue}>{stats.quads}</div>
          <div style={styles.statLabel}>Quads Stored</div>
        </div>
        <div style={styles.statCard}>
          <div style={styles.statValue}>{stats.graphs}</div>
          <div style={styles.statLabel}>Graphs</div>
        </div>
        <div style={styles.statCard}>
          <div style={styles.statValue}>{stats.commits}</div>
          <div style={styles.statLabel}>Commits</div>
        </div>
      </div>

      {/* Parse Section */}
      <section style={styles.section}>
        <h2>📝 Parse Turtle Data</h2>
        <textarea
          style={styles.textarea}
          value={turtleInput}
          onChange={(e) => setTurtleInput(e.target.value)}
          rows={8}
        />
        <div style={styles.controls}>
          <button style={styles.button} onClick={handleParse}>
            Parse and Store
          </button>
          <button style={styles.button} onClick={handleClear}>
            Clear Store
          </button>
        </div>
      </section>

      {/* Query Section */}
      <section style={styles.section}>
        <h2>🔍 SPARQL Query</h2>
        <textarea
          style={styles.textarea}
          value={sparqlInput}
          onChange={(e) => setSparqlInput(e.target.value)}
          rows={6}
        />
        <button style={styles.button} onClick={handleQuery}>
          Execute Query
        </button>

        {queryResults && (
          <div style={styles.output}>
            {queryResults.loading ? (
              <p>⏳ Running query...</p>
            ) : queryResults.type === 'bindings' ? (
              <>
                <p style={styles.success}>
                  ✅ Found {queryResults.bindings.length} results:
                </p>
                {queryResults.bindings.map((binding, i) => (
                  <div key={i} style={styles.resultRow}>
                    <strong>Result {i + 1}:</strong>{' '}
                    {Object.entries(binding)
                      .map(([k, v]) => `${k}: ${v}`)
                      .join(', ')}
                  </div>
                ))}
              </>
            ) : queryResults.type === 'boolean' ? (
              <p style={styles.success}>✅ Result: {String(queryResults.value)}</p>
            ) : (
              <p style={styles.success}>✅ Found {queryResults.quads.length} quads</p>
            )}
          </div>
        )}
      </section>

      {/* Lockchain Section */}
      <section style={styles.section}>
        <h2>🔗 Lockchain Audit Trail</h2>
        <div style={styles.controls}>
          <button style={styles.button} onClick={handleHistory}>
            View History
          </button>
          <button style={styles.button} onClick={handleVerify}>
            Verify Chain
          </button>
        </div>

        {lockchainOutput && (
          <div style={styles.output}>
            {lockchainOutput.type === 'history' ? (
              lockchainOutput.data.length === 0 ? (
                <p>No commits yet</p>
              ) : (
                <>
                  <p style={styles.success}>
                    ✅ {lockchainOutput.data.length} recent commits:
                  </p>
                  {lockchainOutput.data.map((commit, i) => (
                    <div key={i} style={styles.resultRow}>
                      <strong>#{i + 1}</strong> {commit.hash.substring(0, 8)}... by{' '}
                      <strong>{commit.author}</strong> - {commit.message}
                    </div>
                  ))}
                </>
              )
            ) : (
              <p style={lockchainOutput.data.valid ? styles.success : styles.error}>
                {lockchainOutput.data.valid ? '✅' : '❌'}{' '}
                {lockchainOutput.data.message} ({lockchainOutput.data.commits} commits)
              </p>
            )}
          </div>
        )}
      </section>
    </div>
  );
}

// Styles
const styles = {
  container: {
    maxWidth: '1200px',
    margin: '0 auto',
    padding: '2rem',
    fontFamily: '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif',
  },
  header: {
    textAlign: 'center',
    marginBottom: '2rem',
    padding: '2rem',
    background: 'linear-gradient(135deg, #667eea 0%, #764ba2 100%)',
    color: 'white',
    borderRadius: '12px',
  },
  subtitle: {
    opacity: 0.9,
    fontSize: '1.1rem',
    marginTop: '0.5rem',
  },
  stats: {
    display: 'grid',
    gridTemplateColumns: 'repeat(auto-fit, minmax(200px, 1fr))',
    gap: '1rem',
    marginBottom: '2rem',
  },
  statCard: {
    background: '#f8f9fa',
    padding: '1.5rem',
    borderRadius: '8px',
    textAlign: 'center',
    border: '2px solid #e0e0e0',
  },
  statValue: {
    fontSize: '2rem',
    fontWeight: 'bold',
    color: '#667eea',
    marginBottom: '0.5rem',
  },
  statLabel: {
    color: '#666',
    fontSize: '0.9rem',
  },
  section: {
    marginBottom: '2rem',
    padding: '1.5rem',
    background: '#f8f9fa',
    borderRadius: '8px',
  },
  textarea: {
    width: '100%',
    padding: '1rem',
    border: '2px solid #e0e0e0',
    borderRadius: '6px',
    fontFamily: '"Courier New", monospace',
    fontSize: '0.9rem',
    marginBottom: '1rem',
    resize: 'vertical',
  },
  controls: {
    display: 'flex',
    gap: '1rem',
    flexWrap: 'wrap',
  },
  button: {
    background: 'linear-gradient(135deg, #667eea 0%, #764ba2 100%)',
    color: 'white',
    border: 'none',
    padding: '0.75rem 1.5rem',
    borderRadius: '6px',
    fontSize: '1rem',
    cursor: 'pointer',
    transition: 'transform 0.2s',
  },
  output: {
    background: 'white',
    padding: '1rem',
    borderRadius: '6px',
    border: '2px solid #e0e0e0',
    marginTop: '1rem',
    maxHeight: '400px',
    overflowY: 'auto',
  },
  resultRow: {
    padding: '0.5rem',
    marginBottom: '0.5rem',
    background: '#f8f9fa',
    borderRadius: '4px',
    borderLeft: '3px solid #667eea',
  },
  success: {
    color: '#28a745',
    fontWeight: 'bold',
  },
  error: {
    color: '#dc3545',
    padding: '1rem',
    background: '#ffe0e0',
    borderRadius: '6px',
    marginBottom: '1rem',
  },
  loading: {
    textAlign: 'center',
    padding: '4rem',
    color: '#667eea',
  },
};

export default UnrdfDemo;
