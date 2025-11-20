/**
 * @file Mock Factory for Test Services
 * @module test-utils/mock-factory
 * 
 * @description
 * Factory for creating mock implementations of services
 * to avoid ESM mocking issues and provide clean test doubles.
 */

/**
 * Mock query engine implementation
 */
export class MockQueryEngine {
  /**
   *
   */
  constructor() {
    this.askResults = new Map();
    this.selectResults = new Map();
    this.constructResults = new Map();
    this.describeResults = new Map();
    this.callCounts = {
      ask: 0,
      select: 0,
      construct: 0,
      describe: 0
    };
  }

  /**
   * Mock ask method
   * @param {Object} store - RDF store
   * @param {string} query - SPARQL query
   * @returns {Promise<boolean>} Query result
   */
  async ask(store, query) {
    this.callCounts.ask++;
    
    // Check for predefined results
    if (this.askResults.has(query)) {
      return this.askResults.get(query);
    }
    
    // Default behavior - check for dangerous patterns
    if (this.isDangerousQuery(query)) {
      throw new Error('Dangerous query detected');
    }
    
    return true;
  }

  /**
   * Mock select method
   * @param {Object} store - RDF store
   * @param {string} query - SPARQL query
   * @returns {Promise<Array>} Query results
   */
  async select(store, query) {
    this.callCounts.select++;
    
    if (this.selectResults.has(query)) {
      return this.selectResults.get(query);
    }
    
    if (this.isDangerousQuery(query)) {
      throw new Error('Dangerous query detected');
    }
    
    return [];
  }

  /**
   * Mock construct method
   * @param {Object} store - RDF store
   * @param {string} query - SPARQL query
   * @returns {Promise<Object>} Constructed graph
   */
  async construct(store, query) {
    this.callCounts.construct++;
    
    if (this.constructResults.has(query)) {
      return this.constructResults.get(query);
    }
    
    if (this.isDangerousQuery(query)) {
      throw new Error('Dangerous query detected');
    }
    
    return new (await import('n3')).Store();
  }

  /**
   * Mock describe method
   * @param {Object} store - RDF store
   * @param {string} query - SPARQL query
   * @returns {Promise<Object>} Description graph
   */
  async describe(store, query) {
    this.callCounts.describe++;
    
    if (this.describeResults.has(query)) {
      return this.describeResults.get(query);
    }
    
    if (this.isDangerousQuery(query)) {
      throw new Error('Dangerous query detected');
    }
    
    return new (await import('n3')).Store();
  }

  /**
   * Check if query contains dangerous patterns
   * @param {string} query - SPARQL query
   * @returns {boolean} True if dangerous
   */
  isDangerousQuery(query) {
    const dangerousPatterns = [
      'DROP TABLE',
      'DELETE',
      'INSERT',
      'CLEAR',
      'LOAD',
      'UNION',
      'FILTER',
      'BIND'
    ];
    
    return dangerousPatterns.some(pattern => 
      query.toUpperCase().includes(pattern.toUpperCase())
    );
  }

  /**
   * Set predefined result for ask query
   * @param {string} query - SPARQL query
   * @param {boolean} result - Result to return
   */
  setAskResult(query, result) {
    this.askResults.set(query, result);
  }

  /**
   * Set predefined result for select query
   * @param {string} query - SPARQL query
   * @param {Array} result - Result to return
   */
  setSelectResult(query, result) {
    this.selectResults.set(query, result);
  }

  /**
   * Reset all call counts
   */
  resetCallCounts() {
    this.callCounts = {
      ask: 0,
      select: 0,
      construct: 0,
      describe: 0
    };
  }

  /**
   * Get call count for method
   * @param {string} method - Method name
   * @returns {number} Call count
   */
  getCallCount(method) {
    return this.callCounts[method] || 0;
  }
}

/**
 * Mock file resolver implementation
 */
export class MockFileResolver {
  /**
   *
   */
  constructor() {
    this.files = new Map();
    this.accessResults = new Map();
    this.readResults = new Map();
    this.callCounts = {
      resolve: 0,
      load: 0,
      calculateHash: 0
    };
  }

  /**
   * Mock file resolution
   * @param {string} uri - File URI
   * @returns {Promise<string>} Resolved path
   */
  async resolve(uri) {
    this.callCounts.resolve++;
    
    if (this.files.has(uri)) {
      return this.files.get(uri);
    }
    
    // Default behavior
    return uri.replace('file://', '');
  }

  /**
   * Mock file loading
   * @param {string} uri - File URI
   * @param {string} expectedHash - Expected file hash
   * @returns {Promise<string>} File content
   */
  async load(uri, expectedHash) {
    this.callCounts.load++;
    
    if (this.readResults.has(uri)) {
      return this.readResults.get(uri);
    }
    
    // Default behavior
    return 'SELECT * WHERE { ?s ?p ?o }';
  }

  /**
   * Mock hash calculation
   * @param {string} content - File content
   * @returns {Promise<string>} Calculated hash
   */
  async calculateHash(content) {
    this.callCounts.calculateHash++;
    return 'a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3';
  }

  /**
   * Set predefined file content
   * @param {string} uri - File URI
   * @param {string} content - File content
   */
  setFileContent(uri, content) {
    this.files.set(uri, content);
  }

  /**
   * Set predefined read result
   * @param {string} uri - File URI
   * @param {string} content - Content to return
   */
  setReadResult(uri, content) {
    this.readResults.set(uri, content);
  }

  /**
   * Reset all call counts
   */
  resetCallCounts() {
    this.callCounts = {
      resolve: 0,
      load: 0,
      calculateHash: 0
    };
  }
}

/**
 * Mock condition evaluator implementation
 */
export class MockConditionEvaluator {
  /**
   *
   */
  constructor() {
    this.evaluationResults = new Map();
    this.callCounts = {
      evaluate: 0,
      isSatisfied: 0
    };
  }

  /**
   * Mock condition evaluation
   * @param {Object} condition - Condition to evaluate
   * @param {Object} context - Evaluation context
   * @returns {Promise<boolean>} Evaluation result
   */
  async evaluate(condition, context) {
    this.callCounts.evaluate++;
    
    const key = JSON.stringify(condition);
    if (this.evaluationResults.has(key)) {
      return this.evaluationResults.get(key);
    }
    
    // Default behavior
    return true;
  }

  /**
   * Mock condition satisfaction check
   * @param {Object} condition - Condition to check
   * @param {Object} context - Evaluation context
   * @returns {Promise<boolean>} Satisfaction result
   */
  async isSatisfied(condition, context) {
    this.callCounts.isSatisfied++;
    
    const key = JSON.stringify(condition);
    if (this.evaluationResults.has(key)) {
      return this.evaluationResults.get(key);
    }
    
    // Default behavior
    return true;
  }

  /**
   * Set predefined evaluation result
   * @param {Object} condition - Condition
   * @param {boolean} result - Result to return
   */
  setEvaluationResult(condition, result) {
    const key = JSON.stringify(condition);
    this.evaluationResults.set(key, result);
  }

  /**
   * Reset all call counts
   */
  resetCallCounts() {
    this.callCounts = {
      evaluate: 0,
      isSatisfied: 0
    };
  }
}

/**
 * Create a mock query engine
 * @returns {MockQueryEngine} Mock query engine instance
 */
export function createMockQueryEngine() {
  return new MockQueryEngine();
}

/**
 * Create a mock file resolver
 * @returns {MockFileResolver} Mock file resolver instance
 */
export function createMockFileResolver() {
  return new MockFileResolver();
}

/**
 * Create a mock condition evaluator
 * @returns {MockConditionEvaluator} Mock condition evaluator instance
 */
export function createMockConditionEvaluator() {
  return new MockConditionEvaluator();
}
