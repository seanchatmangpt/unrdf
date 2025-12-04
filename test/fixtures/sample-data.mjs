/**
 * @file Sample RDF Test Data
 * @description Reusable test data constants for UNRDF examples
 */

// ============================================================================
// FOAF Sample Data
// ============================================================================

export const FOAF_DATA = {
  alice: {
    uri: 'http://example.org/alice',
    name: 'Alice',
    email: 'alice@example.org',
    mbox_sha1sum: '0e927e5d3b3f8c93c5a59a21e36f83c01f7e5e6d',
    age: 30,
    interests: ['RDF', 'Semantic Web', 'Knowledge Graphs']
  },
  bob: {
    uri: 'http://example.org/bob',
    name: 'Bob',
    email: 'bob@example.org',
    mbox_sha1sum: '1f938f6e4c4f9d84d6b6ac32e47g94d12g8f6f7e',
    age: 28,
    interests: ['Distributed Systems', 'P2P Networks']
  },
  charlie: {
    uri: 'http://example.org/charlie',
    name: 'Charlie',
    email: 'charlie@example.org',
    mbox_sha1sum: '2ga49g7f5d5gae95e7c7bd43f58ha5e23ha9g8g8f',
    age: 35,
    interests: ['Ontologies', 'SPARQL']
  }
}

// ============================================================================
// Standard Vocabularies
// ============================================================================

export const VOCABULARIES = {
  RDF: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  RDFS: 'http://www.w3.org/2000/01/rdf-schema#',
  OWL: 'http://www.w3.org/2002/07/owl#',
  XSD: 'http://www.w3.org/2001/XMLSchema#',
  FOAF: 'http://xmlns.com/foaf/0.1/',
  DC: 'http://purl.org/dc/elements/1.1/',
  DCTERMS: 'http://purl.org/dc/terms/',
  SKOS: 'http://www.w3.org/2004/02/skos/core#',
  SCHEMA: 'http://schema.org/'
}

// ============================================================================
// Sample Quads
// ============================================================================

export const SAMPLE_QUADS = {
  simple: {
    subject: 'http://example.org/subject',
    predicate: 'http://example.org/predicate',
    object: 'Object value',
    graph: null
  },

  typed: {
    subject: 'http://example.org/entity',
    predicate: `${VOCABULARIES.RDF}type`,
    object: 'http://example.org/Thing',
    graph: null
  },

  literal: {
    subject: 'http://example.org/alice',
    predicate: `${VOCABULARIES.FOAF}name`,
    object: 'Alice',
    graph: null
  },

  withGraph: {
    subject: 'http://example.org/bob',
    predicate: `${VOCABULARIES.FOAF}email`,
    object: 'bob@example.org',
    graph: 'http://example.org/graph1'
  }
}

// ============================================================================
// Invalid Test Cases
// ============================================================================

export const INVALID_QUADS = {
  missingSubject: {
    subject: null,
    predicate: 'http://example.org/predicate',
    object: 'value',
    graph: null
  },

  missingPredicate: {
    subject: 'http://example.org/subject',
    predicate: null,
    object: 'value',
    graph: null
  },

  missingObject: {
    subject: 'http://example.org/subject',
    predicate: 'http://example.org/predicate',
    object: null,
    graph: null
  },

  emptyString: {
    subject: '',
    predicate: '',
    object: '',
    graph: null
  }
}

// ============================================================================
// Sample Turtle/N-Triples
// ============================================================================

export const SAMPLE_TURTLE = `
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix ex: <http://example.org/> .

ex:alice a foaf:Person ;
  foaf:name "Alice" ;
  foaf:email "alice@example.org" ;
  foaf:age 30 ;
  foaf:knows ex:bob .

ex:bob a foaf:Person ;
  foaf:name "Bob" ;
  foaf:email "bob@example.org" ;
  foaf:age 28 .
`

export const SAMPLE_NTRIPLES = `
<http://example.org/alice> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://xmlns.com/foaf/0.1/Person> .
<http://example.org/alice> <http://xmlns.com/foaf/0.1/name> "Alice" .
<http://example.org/alice> <http://xmlns.com/foaf/0.1/email> "alice@example.org" .
<http://example.org/bob> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://xmlns.com/foaf/0.1/Person> .
<http://example.org/bob> <http://xmlns.com/foaf/0.1/name> "Bob" .
<http://example.org/bob> <http://xmlns.com/foaf/0.1/email> "bob@example.org" .
`

// ============================================================================
// Sample SPARQL Queries
// ============================================================================

export const SAMPLE_QUERIES = {
  selectAll: `
    SELECT ?s ?p ?o
    WHERE {
      ?s ?p ?o .
    }
  `,

  selectNames: `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    SELECT ?name
    WHERE {
      ?person foaf:name ?name .
    }
  `,

  construct: `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    CONSTRUCT {
      ?person a foaf:Person .
      ?person foaf:name ?name .
    }
    WHERE {
      ?person foaf:name ?name .
    }
  `,

  filter: `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    SELECT ?person ?name
    WHERE {
      ?person foaf:name ?name .
      FILTER(STRLEN(?name) > 3)
    }
  `
}

// ============================================================================
// Sample Configuration Objects
// ============================================================================

export const SAMPLE_CONFIGS = {
  basic: {
    storage: 'memory',
    syncEnabled: false,
    logLevel: 'error'
  },

  persistent: {
    storage: 'indexeddb',
    dbName: 'test-db',
    storeName: 'test-store',
    version: 1
  },

  federated: {
    peerDiscovery: true,
    maxPeers: 5,
    syncInterval: 5000,
    conflictResolution: 'last-write-wins'
  },

  streaming: {
    enableStreaming: true,
    bufferSize: 100,
    flushInterval: 1000
  }
}

// ============================================================================
// Sample Batch Operations
// ============================================================================

export const SAMPLE_BATCHES = {
  smallBatch: 10,
  mediumBatch: 100,
  largeBatch: 1000,

  generateBatch(size) {
    const batch = []
    for (let i = 0; i < size; i++) {
      batch.push({
        subject: `http://example.org/entity${i}`,
        predicate: `${VOCABULARIES.RDF}type`,
        object: 'http://example.org/Thing',
        graph: null
      })
    }
    return batch
  }
}

// ============================================================================
// Sample Error Cases
// ============================================================================

export const ERROR_CASES = {
  networkError: {
    code: 'NETWORK_ERROR',
    message: 'Network request failed',
    statusCode: 0
  },

  parseError: {
    code: 'PARSE_ERROR',
    message: 'Failed to parse RDF data',
    line: 5,
    column: 10
  },

  validationError: {
    code: 'VALIDATION_ERROR',
    message: 'Invalid quad structure',
    details: 'Missing subject'
  },

  timeoutError: {
    code: 'TIMEOUT',
    message: 'Operation timed out',
    timeout: 5000
  }
}

// ============================================================================
// Performance Benchmarks
// ============================================================================

export const BENCHMARK_SIZES = {
  tiny: 10,
  small: 100,
  medium: 1000,
  large: 10000,
  xlarge: 100000
}

export const PERFORMANCE_THRESHOLDS = {
  query: {
    fast: 10,      // ms
    acceptable: 50,
    slow: 200
  },

  insert: {
    fast: 5,
    acceptable: 20,
    slow: 100
  },

  sync: {
    fast: 100,
    acceptable: 500,
    slow: 2000
  }
}
