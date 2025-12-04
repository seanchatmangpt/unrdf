/**
 * @file Test Type Definitions
 * @description JSDoc type definitions for UNRDF tests
 */

/**
 * @typedef {Object} TestQuad
 * @property {Object} subject - Quad subject (NamedNode or BlankNode)
 * @property {Object} predicate - Quad predicate (NamedNode)
 * @property {Object} object - Quad object (NamedNode, BlankNode, or Literal)
 * @property {Object} graph - Quad graph (NamedNode or DefaultGraph)
 */

/**
 * @typedef {Object} TestStore
 * @property {number} size - Number of quads in store
 * @property {Function} addQuad - Add quad to store
 * @property {Function} removeQuad - Remove quad from store
 * @property {Function} getQuads - Query quads from store
 * @property {Function} has - Check if quad exists
 */

/**
 * @typedef {Object} TestResult
 * @property {number} passed - Number of passed tests
 * @property {number} failed - Number of failed tests
 * @property {number} skipped - Number of skipped tests
 * @property {number} duration - Test duration in ms
 * @property {string} [error] - Error message if failed
 */

/**
 * @typedef {Object} CoverageMetrics
 * @property {number} lines - Line coverage percentage
 * @property {number} statements - Statement coverage percentage
 * @property {number} functions - Function coverage percentage
 * @property {number} branches - Branch coverage percentage
 */

/**
 * @typedef {Object} TestContext
 * @property {TestStore} [store] - RDF store instance
 * @property {Array<Object>} [subscriptions] - Active subscriptions
 * @property {Array<number>} [timers] - Active timers
 * @property {Function} cleanup - Cleanup function
 */

/**
 * @typedef {Object} MockPeer
 * @property {string} id - Peer identifier
 * @property {string} status - Peer status (healthy, unhealthy, offline)
 * @property {number} lastSeen - Last seen timestamp
 * @property {Object} syncState - Synchronization state
 * @property {Object} metadata - Peer metadata
 */

/**
 * @typedef {Object} MockSubscription
 * @property {Function} subscribe - Subscribe handler
 * @property {Function} next - Emit next value
 * @property {Function} error - Emit error
 * @property {Function} complete - Complete subscription
 * @property {Function} unsubscribe - Unsubscribe
 * @property {Function} isActive - Check if active
 */

/**
 * @typedef {Object} MockServer
 * @property {Map} handlers - Request handlers
 * @property {Array<Object>} requests - Request history
 * @property {Function} on - Register handler
 * @property {Function} get - Register GET handler
 * @property {Function} post - Register POST handler
 * @property {Function} request - Make request
 * @property {Function} reset - Reset server state
 * @property {Function} getRequests - Get request history
 */

/**
 * @typedef {Object} MockWebSocket
 * @property {number} readyState - WebSocket ready state
 * @property {Function} addEventListener - Add event listener
 * @property {Function} removeEventListener - Remove event listener
 * @property {Function} send - Send message
 * @property {Function} close - Close connection
 */

/**
 * @typedef {Object} SampleData
 * @property {string} uri - Entity URI
 * @property {string} name - Entity name
 * @property {string} email - Entity email
 * @property {number} age - Entity age
 * @property {Array<string>} interests - Entity interests
 */

/**
 * @typedef {Object} TestConfig
 * @property {string} storage - Storage type (memory, indexeddb)
 * @property {boolean} syncEnabled - Sync enabled flag
 * @property {string} logLevel - Log level
 * @property {string} [dbName] - Database name
 * @property {string} [storeName] - Store name
 * @property {number} [version] - Database version
 */

/**
 * @typedef {Object} TestVocabularies
 * @property {string} RDF - RDF namespace
 * @property {string} RDFS - RDFS namespace
 * @property {string} OWL - OWL namespace
 * @property {string} XSD - XSD namespace
 * @property {string} FOAF - FOAF namespace
 * @property {string} DC - Dublin Core namespace
 * @property {string} SCHEMA - Schema.org namespace
 */

/**
 * @typedef {Object} PerformanceThresholds
 * @property {Object} query - Query performance thresholds
 * @property {number} query.fast - Fast query time in ms
 * @property {number} query.acceptable - Acceptable query time in ms
 * @property {number} query.slow - Slow query time in ms
 */

export {}
