/**
 * Poka-Yoke Pattern: Connection Lifecycle Guard
 * 
 * Enforces strict connection state machine:
 * Disconnected → Connecting → Connected → Closing → Closed
 * 
 * Prevents:
 * - Use-after-close
 * - Query-while-connecting
 * - Double-close
 * - Double-connect
 * 
 * @module @unrdf/core/poka-yoke/connection-lifecycle
 */

const CONN_STATE = {
  DISCONNECTED: 'disconnected',
  CONNECTING: 'connecting',
  CONNECTED: 'connected',
  CLOSING: 'closing',
  CLOSED: 'closed',
};

/**
 * Error thrown when invalid operation attempted in current state
 */
export class ConnectionStateError extends Error {
  /**
   *
   */
  constructor(operation, currentState, validStates = []) {
    const validStatesStr = validStates.join(', ');
    const msg = `Cannot ${operation}: connection is ${currentState} (valid states: ${validStatesStr})`;
    super(msg);
    this.name = 'ConnectionStateError';
    this.operation = operation;
    this.currentState = currentState;
    this.validStates = validStates;
  }
}

/**
 * Mock connection for demonstration
 * In real implementation, this would be a real DB/store connection
 */
class MockConnection {
  constructor(config) {
    this.config = config;
    this.queryCount = 0;
  }
  
  async query(sparql) {
    // Simulate async query
    await new Promise(resolve => setTimeout(resolve, 10));
    this.queryCount++;
    return {
      results: [],
      query: sparql,
    };
  }
  
  async close() {
    // Simulate async close
    await new Promise(resolve => setTimeout(resolve, 10));
  }
}

/**
 * Guarded Connection with enforced lifecycle
 * 
 * State machine: Disconnected → Connecting → Connected → Closing → Closed
 * 
 * POKA-YOKE GUARANTEES:
 * - query() only works in CONNECTED state
 * - connect() cannot be called twice
 * - close() cannot be called twice
 * - All invalid operations throw ConnectionStateError
 * 
 * @example
 * const conn = new GuardedConnection();
 * await conn.connect({ url: 'http://localhost:7878' });  // OK
 * await conn.query('SELECT * WHERE { ?s ?p ?o }');       // OK
 * await conn.close();                                     // OK
 * await conn.query('SELECT * ...');                       // THROWS
 */
export class GuardedConnection {
  #state = CONN_STATE.DISCONNECTED;
  #conn = null;
  
  /**
   * Assert connection is in CONNECTED state
   * @private
   * @param {string} operation - Operation being attempted
   * @throws {ConnectionStateError} If not connected
   */
  #assertConnected(operation) {
    if (this.#state === CONN_STATE.DISCONNECTED) {
      throw new ConnectionStateError(operation, 'disconnected', [CONN_STATE.CONNECTED]);
    }
    if (this.#state === CONN_STATE.CONNECTING) {
      throw new ConnectionStateError(operation, 'connecting', [CONN_STATE.CONNECTED]);
    }
    if (this.#state === CONN_STATE.CLOSING) {
      throw new ConnectionStateError(operation, 'closing', [CONN_STATE.CONNECTED]);
    }
    if (this.#state === CONN_STATE.CLOSED) {
      throw new ConnectionStateError(operation, 'closed', [CONN_STATE.CONNECTED]);
    }
  }
  
  /**
   * Get current connection state (for testing/observability)
   * @returns {string} Current state
   */
  getState() {
    return this.#state;
  }
  
  /**
   * Get connection statistics
   * @returns {Object} Connection stats
   * @throws {ConnectionStateError} If not connected
   */
  getStats() {
    this.#assertConnected('get stats');
    return {
      state: this.#state,
      queryCount: this.#conn?.queryCount || 0,
    };
  }
  
  /**
   * Connect to database/store
   * 
   * State transitions:
   * - DISCONNECTED → CONNECTING → CONNECTED (success)
   * - DISCONNECTED → CONNECTING → DISCONNECTED (failure)
   * 
   * @param {Object} config - Connection configuration
   * @param {string} config.url - Database URL
   * @returns {Promise<void>}
   * @throws {ConnectionStateError} If already connecting or connected
   */
  async connect(config) {
    if (this.#state === CONN_STATE.CONNECTING) {
      throw new ConnectionStateError(
        'connect',
        'already connecting',
        [CONN_STATE.DISCONNECTED]
      );
    }
    if (this.#state === CONN_STATE.CONNECTED) {
      throw new ConnectionStateError(
        'connect',
        'already connected',
        [CONN_STATE.DISCONNECTED]
      );
    }
    if (this.#state === CONN_STATE.CLOSING) {
      throw new ConnectionStateError(
        'connect',
        'closing',
        [CONN_STATE.DISCONNECTED]
      );
    }
    if (this.#state === CONN_STATE.CLOSED) {
      throw new ConnectionStateError(
        'connect',
        'closed (create new instance)',
        [CONN_STATE.DISCONNECTED]
      );
    }
    
    // Transition to CONNECTING
    this.#state = CONN_STATE.CONNECTING;
    
    try {
      // Attempt connection (async)
      this.#conn = new MockConnection(config);
      
      // Simulate async connection setup
      await new Promise(resolve => setTimeout(resolve, 20));
      
      // Success: transition to CONNECTED
      this.#state = CONN_STATE.CONNECTED;
    } catch (err) {
      // Failure: revert to DISCONNECTED
      this.#state = CONN_STATE.DISCONNECTED;
      this.#conn = null;
      throw err;
    }
  }
  
  /**
   * Execute SPARQL query
   * 
   * @param {string} sparql - SPARQL query string
   * @returns {Promise<Object>} Query results
   * @throws {ConnectionStateError} If not connected
   */
  async query(sparql) {
    this.#assertConnected('execute query');
    
    if (!sparql || typeof sparql !== 'string') {
      throw new TypeError('query: sparql must be a non-empty string');
    }
    
    return this.#conn.query(sparql);
  }
  
  /**
   * Close connection
   * 
   * State transitions:
   * - CONNECTED → CLOSING → CLOSED
   * 
   * @returns {Promise<void>}
   * @throws {ConnectionStateError} If not connected
   */
  async close() {
    // Can only close from CONNECTED state
    if (this.#state === CONN_STATE.DISCONNECTED) {
      throw new ConnectionStateError(
        'close',
        'not connected',
        [CONN_STATE.CONNECTED]
      );
    }
    if (this.#state === CONN_STATE.CONNECTING) {
      throw new ConnectionStateError(
        'close',
        'still connecting',
        [CONN_STATE.CONNECTED]
      );
    }
    if (this.#state === CONN_STATE.CLOSING) {
      throw new ConnectionStateError(
        'close',
        'already closing',
        [CONN_STATE.CONNECTED]
      );
    }
    if (this.#state === CONN_STATE.CLOSED) {
      throw new ConnectionStateError(
        'close',
        'already closed',
        [CONN_STATE.CONNECTED]
      );
    }
    
    // Transition to CLOSING
    this.#state = CONN_STATE.CLOSING;
    
    try {
      // Close connection
      await this.#conn.close();
    } finally {
      // Always transition to CLOSED, even if close fails
      this.#state = CONN_STATE.CLOSED;
      this.#conn = null;
    }
  }
}
