/**
 * @fileoverview Basic usage example for UNRDF React Hooks
 * @example
 *
 * This example demonstrates how to use UNRDF React hooks in a React application.
 */

import React, { useEffect, useState } from 'react';
import {
  KnowledgeEngineProvider,
  useKnowledgeEngine,
  useStore,
  useTerms,
  useSPARQLQuery
} from '../../src/react-hooks/index.mjs';

/**
 * Example component using UNRDF hooks
 */
function RDFDataViewer() {
  const { store, addQuad } = useStore();
  const { namedNode, literal, quad } = useTerms();

  // Add some sample data
  useEffect(() => {
    const triples = [
      quad(
        namedNode('http://example.org/alice'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Alice', 'en')
      ),
      quad(
        namedNode('http://example.org/alice'),
        namedNode('http://xmlns.com/foaf/0.1/age'),
        literal('30', namedNode('http://www.w3.org/2001/XMLSchema#integer'))
      ),
      quad(
        namedNode('http://example.org/bob'),
        namedNode('http://xmlns.com/foaf/0.1/name'),
        literal('Bob', 'en')
      )
    ];

    triples.forEach(t => addQuad(t));
  }, []);

  return (
    <div>
      <h2>RDF Store</h2>
      <p>Total triples: {store.size}</p>
    </div>
  );
}

/**
 * Example component using SPARQL queries
 */
function SPARQLQueryExample() {
  const { data, loading, error, refetch } = useSPARQLQuery(`
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    SELECT ?person ?name WHERE {
      ?person foaf:name ?name .
    }
  `);

  if (loading) return <div>Loading...</div>;
  if (error) return <div>Error: {error.message}</div>;

  return (
    <div>
      <h2>Query Results</h2>
      <button onClick={refetch}>Refresh</button>
      {data?.rows?.map((row, i) => (
        <div key={i}>
          {row.person.value}: {row.name.value}
        </div>
      ))}
    </div>
  );
}

/**
 * Main app with provider
 */
function App() {
  return (
    <KnowledgeEngineProvider
      config={{
        enableKnowledgeHooks: true,
        enableObservability: true,
        strictMode: false
      }}
    >
      <div className="App">
        <h1>UNRDF React Hooks Demo</h1>
        <RDFDataViewer />
        <SPARQLQueryExample />
      </div>
    </KnowledgeEngineProvider>
  );
}

export default App;
