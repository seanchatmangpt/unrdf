/**
 * @file Graph Explorer Component
 * @description Complete React component demonstrating core hooks usage
 * Displays RDF graphs with SPARQL queries and SHACL validation
 * @example
 * import GraphExplorer from 'unrdf/examples/react-hooks/graph-explorer.jsx';
 * export default function App() {
 *   return <GraphExplorer />;
 * }
 */

import React, { useState, useCallback, useMemo } from 'react';
import {
  KnowledgeEngineProvider,
  useKnowledgeEngineContext,
  useStore,
  useTerms,
  useSPARQLQuery,
  useShapeValidation,
  useTriples,
  useNamespaces,
  usePerformanceTracking,
  useQueryAsync,
} from 'unrdf/react-hooks';

/**
 * Graph Explorer App Component
 * Provides the context and main layout
 */
export function GraphExplorerApp() {
  return (
    <KnowledgeEngineProvider
      config={{
        baseIRI: 'https://example.com/',
        enableKnowledgeHooks: true,
        enableObservability: true,
        cacheConfig: {
          maxSize: 1000,
          ttl: 60000,
        },
      }}
    >
      <GraphExplorer />
    </KnowledgeEngineProvider>
  );
}

/**
 * Main Graph Explorer Component
 * Uses all the core hooks to provide interactive RDF browsing
 */
function GraphExplorer() {
  const { store, isReady } = useKnowledgeEngineContext();
  const [selectedGraph, setSelectedGraph] = useState(null);
  const [sparqlQuery, setSPARQLQuery] = useState('SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10');
  const [shapes, setShapes] = useState('');

  if (!isReady) {
    return <LoadingSpinner />;
  }

  return (
    <div style={styles.container}>
      <header style={styles.header}>
        <h1>UNRDF React Hooks - Graph Explorer</h1>
        <p>Interactive RDF Knowledge Graph Browser</p>
      </header>

      <div style={styles.mainContent}>
        <div style={styles.sidebar}>
          <GraphStatistics />
          <GraphPanel selectedGraph={selectedGraph} onSelectGraph={setSelectedGraph} />
          <DataImportPanel />
        </div>

        <div style={styles.mainPanel}>
          <QueryPanel query={sparqlQuery} onQueryChange={setSPARQLQuery} />
          <ResultsPanel query={sparqlQuery} />
          <ValidationPanel shapes={shapes} query={sparqlQuery} />
        </div>
      </div>
    </div>
  );
}

/**
 * Graph Statistics Component
 * Shows store metrics and performance data
 */
function GraphStatistics() {
  const { store } = useKnowledgeEngineContext();
  const { metrics } = usePerformanceTracking();

  const stats = useMemo(() => {
    if (!store) return null;
    return {
      totalQuads: store.size || 0,
      graphs: new Set([...store.quads()].map(q => q.graph.value)),
    };
  }, [store]);

  if (!stats) return null;

  return (
    <div style={styles.panel}>
      <h3>Store Statistics</h3>
      <dl style={styles.statsList}>
        <dt>Total Quads:</dt>
        <dd>{stats.totalQuads.toLocaleString()}</dd>
        <dt>Graphs:</dt>
        <dd>{stats.graphs.size}</dd>
        {metrics && (
          <>
            <dt>Last Query Time:</dt>
            <dd>{metrics.lastQueryTime?.toFixed(2) || 'N/A'}ms</dd>
            <dt>Cache Hit Rate:</dt>
            <dd>{(metrics.cacheHitRate * 100)?.toFixed(1) || 'N/A'}%</dd>
          </>
        )}
      </dl>
    </div>
  );
}

/**
 * Graph Panel Component
 * Lists available graphs and allows selection
 */
function GraphPanel({ selectedGraph, onSelectGraph }) {
  const { store } = useKnowledgeEngineContext();
  const { data: graphs } = useSPARQLQuery(`
    SELECT DISTINCT ?g WHERE {
      GRAPH ?g { ?s ?p ?o }
    }
  `);

  return (
    <div style={styles.panel}>
      <h3>Available Graphs</h3>
      <ul style={styles.list}>
        {graphs?.rows?.map(row => (
          <li
            key={row.g.value}
            onClick={() => onSelectGraph(row.g)}
            style={{
              ...styles.listItem,
              backgroundColor: selectedGraph?.value === row.g.value ? '#e0f2fe' : 'transparent',
            }}
          >
            {row.g.value.split('/').pop()}
          </li>
        ))}
      </ul>
    </div>
  );
}

/**
 * Data Import Panel Component
 * Allows importing RDF data from various formats
 */
function DataImportPanel() {
  const { store } = useKnowledgeEngineContext();
  const { addQuad } = useStore();
  const { namedNode, literal, quad, blankNode } = useTerms();
  const [importText, setImportText] = useState('');
  const [format, setFormat] = useState('turtle');

  const handleImport = useCallback(async () => {
    try {
      // Example: Create sample quads programmatically
      if (importText.trim()) {
        const subject = namedNode('https://example.com/sample');
        const predicate = namedNode('https://example.com/label');
        const object = literal(importText);
        const graph = namedNode('https://example.com/default');

        const q = quad(subject, predicate, object, graph);
        addQuad(q);
        setImportText('');
      }
    } catch (error) {
      console.error('Import failed:', error);
    }
  }, [importText, addQuad, namedNode, literal, quad]);

  return (
    <div style={styles.panel}>
      <h3>Import Data</h3>
      <select value={format} onChange={e => setFormat(e.target.value)} style={styles.input}>
        <option value="turtle">Turtle</option>
        <option value="jsonld">JSON-LD</option>
        <option value="nquads">N-Quads</option>
      </select>
      <textarea
        placeholder="Paste RDF data..."
        value={importText}
        onChange={e => setImportText(e.target.value)}
        style={{ ...styles.input, minHeight: '100px' }}
      />
      <button onClick={handleImport} style={styles.button}>
        Import
      </button>
    </div>
  );
}

/**
 * Query Panel Component
 * SPARQL query editor
 */
function QueryPanel({ query, onQueryChange }) {
  return (
    <div style={styles.panel}>
      <h3>SPARQL Query</h3>
      <textarea
        value={query}
        onChange={e => onQueryChange(e.target.value)}
        style={{ ...styles.input, minHeight: '150px', fontFamily: 'monospace' }}
      />
      <div style={styles.helperText}>
        <p>
          <strong>Tips:</strong> Use SELECT, ASK, CONSTRUCT, or DESCRIBE. Bind variables with ?name.
        </p>
      </div>
    </div>
  );
}

/**
 * Results Panel Component
 * Displays SPARQL query results
 */
function ResultsPanel({ query }) {
  const { data, loading, error } = useQueryAsync(query);

  if (loading) {
    return (
      <div style={styles.panel}>
        <h3>Results</h3>
        <LoadingSpinner />
      </div>
    );
  }

  if (error) {
    return (
      <div style={styles.panel}>
        <h3>Results</h3>
        <div style={styles.error}>Error: {error.message}</div>
      </div>
    );
  }

  const rows = data?.rows || [];
  if (rows.length === 0) {
    return (
      <div style={styles.panel}>
        <h3>Results</h3>
        <p>No results found</p>
      </div>
    );
  }

  return (
    <div style={styles.panel}>
      <h3>
        Results ({rows.length} row{rows.length !== 1 ? 's' : ''})
      </h3>
      <div style={styles.tableContainer}>
        <table style={styles.table}>
          <thead>
            <tr>
              {Object.keys(rows[0] || {}).map(key => (
                <th key={key}>{key}</th>
              ))}
            </tr>
          </thead>
          <tbody>
            {rows.map((row, idx) => (
              <tr key={idx}>
                {Object.values(row).map((value, idx) => (
                  <td key={idx}>{value?.value || String(value)}</td>
                ))}
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </div>
  );
}

/**
 * Validation Panel Component
 * SHACL shape validation
 */
function ValidationPanel({ shapes, query }) {
  const { results, severity } = useShapeValidation(shapes);

  return (
    <div style={styles.panel}>
      <h3>Shape Validation</h3>
      {!shapes && <p>No shapes configured</p>}
      {shapes && (
        <>
          {results?.length === 0 && <div style={styles.success}>✓ All shapes valid</div>}
          {results?.map((result, idx) => (
            <div key={idx} style={styles.validationResult}>
              <strong>Violation</strong> (severity: {result.severity})<p>{result.message}</p>
            </div>
          ))}
        </>
      )}
    </div>
  );
}

/**
 * Loading Spinner Component
 */
function LoadingSpinner() {
  return (
    <div style={styles.spinner}>
      <div style={styles.spinnerInner}></div>
      <p>Loading...</p>
    </div>
  );
}

/**
 * Component Styles
 */
const styles = {
  container: {
    fontFamily: 'system-ui, -apple-system, sans-serif',
    backgroundColor: '#f5f5f5',
    minHeight: '100vh',
    display: 'flex',
    flexDirection: 'column',
  },
  header: {
    backgroundColor: '#1e293b',
    color: 'white',
    padding: '2rem',
    textAlign: 'center',
  },
  mainContent: {
    display: 'flex',
    flex: 1,
    gap: '1rem',
    padding: '1rem',
  },
  sidebar: {
    width: '300px',
    display: 'flex',
    flexDirection: 'column',
    gap: '1rem',
  },
  mainPanel: {
    flex: 1,
    display: 'flex',
    flexDirection: 'column',
    gap: '1rem',
    overflow: 'auto',
  },
  panel: {
    backgroundColor: 'white',
    borderRadius: '8px',
    padding: '1rem',
    boxShadow: '0 2px 4px rgba(0,0,0,0.1)',
  },
  statsList: {
    display: 'grid',
    gridTemplateColumns: '1fr 1fr',
    gap: '0.5rem',
  },
  list: {
    listStyle: 'none',
    padding: 0,
    margin: 0,
  },
  listItem: {
    padding: '0.5rem',
    borderRadius: '4px',
    cursor: 'pointer',
    transition: 'background-color 0.2s',
  },
  input: {
    width: '100%',
    padding: '0.5rem',
    marginBottom: '0.5rem',
    border: '1px solid #e0e0e0',
    borderRadius: '4px',
    fontFamily: 'inherit',
  },
  button: {
    backgroundColor: '#3b82f6',
    color: 'white',
    padding: '0.5rem 1rem',
    border: 'none',
    borderRadius: '4px',
    cursor: 'pointer',
  },
  tableContainer: {
    overflowX: 'auto',
  },
  table: {
    width: '100%',
    borderCollapse: 'collapse',
    fontSize: '0.875rem',
  },
  error: {
    backgroundColor: '#fee2e2',
    color: '#991b1b',
    padding: '0.75rem',
    borderRadius: '4px',
  },
  success: {
    backgroundColor: '#dcfce7',
    color: '#166534',
    padding: '0.75rem',
    borderRadius: '4px',
  },
  validationResult: {
    backgroundColor: '#fef3c7',
    borderLeft: '4px solid #f59e0b',
    padding: '0.75rem',
    marginBottom: '0.5rem',
    borderRadius: '4px',
  },
  helperText: {
    fontSize: '0.875rem',
    color: '#666',
    marginTop: '0.5rem',
  },
  spinner: {
    display: 'flex',
    flexDirection: 'column',
    alignItems: 'center',
    justifyContent: 'center',
    padding: '2rem',
  },
  spinnerInner: {
    width: '40px',
    height: '40px',
    border: '4px solid #e0e0e0',
    borderTop: '4px solid #3b82f6',
    borderRadius: '50%',
    animation: 'spin 1s linear infinite',
  },
};

export default GraphExplorerApp;
