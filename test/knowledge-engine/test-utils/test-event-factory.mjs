/**
 * @file Test Event Factory
 * @module test-utils/test-event-factory
 * 
 * @description
 * Factory for creating test events with sensible defaults
 * and easy customization for different test scenarios.
 */

import { Store } from 'n3';

/**
 * Test event factory class
 */
export class TestEventFactory {
  constructor() {
    this.defaultEvent = {
      name: 'test-event',
      payload: {},
      context: {
        graph: new Store(),
        user: { role: 'user', id: 'test-user' },
        timestamp: new Date().toISOString()
      }
    };
  }

  /**
   * Set event name
   * @param {string} name - Event name
   * @returns {TestEventFactory} This factory instance
   */
  withName(name) {
    this.defaultEvent.name = name;
    return this;
  }

  /**
   * Set event payload
   * @param {Object} payload - Event payload
   * @returns {TestEventFactory} This factory instance
   */
  withPayload(payload) {
    this.defaultEvent.payload = { ...payload };
    return this;
  }

  /**
   * Set event context
   * @param {Object} context - Event context
   * @returns {TestEventFactory} This factory instance
   */
  withContext(context) {
    this.defaultEvent.context = { ...this.defaultEvent.context, ...context };
    return this;
  }

  /**
   * Set RDF graph in context
   * @param {Object} graph - RDF graph (Store instance)
   * @returns {TestEventFactory} This factory instance
   */
  withGraph(graph) {
    this.defaultEvent.context.graph = graph;
    return this;
  }

  /**
   * Set user context
   * @param {Object} user - User information
   * @returns {TestEventFactory} This factory instance
   */
  withUser(user) {
    this.defaultEvent.context.user = { ...user };
    return this;
  }

  /**
   * Set timestamp
   * @param {string|Date} timestamp - Timestamp
   * @returns {TestEventFactory} This factory instance
   */
  withTimestamp(timestamp) {
    this.defaultEvent.context.timestamp = timestamp instanceof Date 
      ? timestamp.toISOString() 
      : timestamp;
    return this;
  }

  /**
   * Add RDF data to the graph
   * @param {string} data - RDF data in Turtle format
   * @returns {TestEventFactory} This factory instance
   */
  withRdfData(data) {
    const { Parser } = require('n3');
    const parser = new Parser();
    const quads = parser.parse(data);
    
    quads.forEach(quad => {
      this.defaultEvent.context.graph.add(quad);
    });
    
    return this;
  }

  /**
   * Set large payload for performance testing
   * @param {number} sizeKB - Payload size in KB
   * @returns {TestEventFactory} This factory instance
   */
  withLargePayload(sizeKB = 100) {
    const largeData = 'x'.repeat(sizeKB * 1024);
    this.defaultEvent.payload = { data: largeData, size: sizeKB };
    return this;
  }

  /**
   * Set malicious payload for security testing
   * @param {string} type - Type of malicious payload
   * @returns {TestEventFactory} This factory instance
   */
  withMaliciousPayload(type = 'xss') {
    switch (type) {
      case 'xss':
        this.defaultEvent.payload = { 
          script: '<script>alert("xss")</script>',
          img: '<img src=x onerror=alert(1)>'
        };
        break;
      case 'sql':
        this.defaultEvent.payload = { 
          query: "'; DROP TABLE users; --",
          union: "1' UNION SELECT * FROM users --"
        };
        break;
      case 'path':
        this.defaultEvent.payload = { 
          path: '../../../etc/passwd',
          uri: 'file:///etc/passwd'
        };
        break;
      default:
        this.defaultEvent.payload = { malicious: type };
    }
    return this;
  }

  /**
   * Set error conditions
   * @param {string} errorType - Type of error to simulate
   * @returns {TestEventFactory} This factory instance
   */
  withErrorCondition(errorType = 'timeout') {
    switch (errorType) {
      case 'timeout':
        this.defaultEvent.context.timeout = 1; // 1ms timeout
        break;
      case 'memory':
        this.defaultEvent.context.memoryLimit = 1; // 1MB limit
        break;
      case 'permission':
        this.defaultEvent.context.user.role = 'guest';
        break;
      default:
        this.defaultEvent.context.errorType = errorType;
    }
    return this;
  }

  /**
   * Build the final event object
   * @returns {Object} Complete event definition
   */
  build() {
    return { ...this.defaultEvent };
  }
}

/**
 * Create a new test event factory
 * @returns {TestEventFactory} New factory instance
 */
export function createTestEventFactory() {
  return new TestEventFactory();
}

/**
 * Create a simple test event
 * @param {string} name - Event name
 * @returns {Object} Event definition
 */
export function createTestEvent(name = 'test-event') {
  return new TestEventFactory()
    .withName(name)
    .build();
}

/**
 * Create a test event with RDF data
 * @param {string} name - Event name
 * @param {string} rdfData - RDF data in Turtle format
 * @returns {Object} Event definition
 */
export function createTestEventWithRdf(name, rdfData) {
  return new TestEventFactory()
    .withName(name)
    .withRdfData(rdfData)
    .build();
}

/**
 * Create a test event for security testing
 * @param {string} name - Event name
 * @param {string} maliciousType - Type of malicious payload
 * @returns {Object} Event definition
 */
export function createMaliciousTestEvent(name, maliciousType = 'xss') {
  return new TestEventFactory()
    .withName(name)
    .withMaliciousPayload(maliciousType)
    .build();
}

/**
 * Create a test event for performance testing
 * @param {string} name - Event name
 * @param {number} payloadSizeKB - Payload size in KB
 * @returns {Object} Event definition
 */
export function createPerformanceTestEvent(name, payloadSizeKB = 100) {
  return new TestEventFactory()
    .withName(name)
    .withLargePayload(payloadSizeKB)
    .build();
}

/**
 * Create a test event for error testing
 * @param {string} name - Event name
 * @param {string} errorType - Type of error condition
 * @returns {Object} Event definition
 */
export function createErrorTestEvent(name, errorType = 'timeout') {
  return new TestEventFactory()
    .withName(name)
    .withErrorCondition(errorType)
    .build();
}
