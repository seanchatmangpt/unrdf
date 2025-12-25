/**
 * @file Test Data Fixtures
 * @module test-utils/fixtures
 * @description
 * Reusable test data fixtures for integration and unit tests.
 * Includes sample RDF data, workflows, and common test scenarios.
 */

import { createQuad } from './helpers.mjs';

/**
 * Sample RDF data for testing
 */
export const sampleRDF = {
  /**
   * Simple person data
   */
  person: {
    uri: 'http://example.org/person/john',
    quads: [
      createQuad(
        'http://example.org/person/john',
        'http://xmlns.com/foaf/0.1/name',
        'John Doe',
        { literal: true }
      ),
      createQuad(
        'http://example.org/person/john',
        'http://xmlns.com/foaf/0.1/mbox',
        'mailto:john@example.org'
      ),
      createQuad(
        'http://example.org/person/john',
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        'http://xmlns.com/foaf/0.1/Person'
      ),
    ],
  },

  /**
   * Organization data
   */
  organization: {
    uri: 'http://example.org/org/acme',
    quads: [
      createQuad(
        'http://example.org/org/acme',
        'http://xmlns.com/foaf/0.1/name',
        'ACME Corporation',
        { literal: true }
      ),
      createQuad(
        'http://example.org/org/acme',
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        'http://xmlns.com/foaf/0.1/Organization'
      ),
    ],
  },

  /**
   * Document metadata
   */
  document: {
    uri: 'http://example.org/doc/report-2025',
    quads: [
      createQuad(
        'http://example.org/doc/report-2025',
        'http://purl.org/dc/terms/title',
        '2025 Annual Report',
        { literal: true }
      ),
      createQuad(
        'http://example.org/doc/report-2025',
        'http://purl.org/dc/terms/created',
        '2025-01-01',
        { literal: true }
      ),
      createQuad(
        'http://example.org/doc/report-2025',
        'http://purl.org/dc/terms/creator',
        'http://example.org/person/john'
      ),
    ],
  },
};

/**
 * Sample workflow specifications
 */
export const sampleWorkflows = {
  /**
   * Simple linear workflow
   */
  linear: {
    id: 'linear-workflow',
    name: 'Linear Workflow',
    description: 'Simple sequential workflow',
    tasks: [
      { id: 'step1', type: 'atomic', name: 'Step 1' },
      { id: 'step2', type: 'atomic', name: 'Step 2' },
      { id: 'step3', type: 'atomic', name: 'Step 3' },
    ],
    flows: [
      { from: 'step1', to: 'step2' },
      { from: 'step2', to: 'step3' },
    ],
  },

  /**
   * Parallel split/join workflow
   */
  parallel: {
    id: 'parallel-workflow',
    name: 'Parallel Workflow',
    description: 'Workflow with parallel branches',
    tasks: [
      { id: 'start', type: 'atomic', name: 'Start' },
      { id: 'parallel-a', type: 'atomic', name: 'Parallel A' },
      { id: 'parallel-b', type: 'atomic', name: 'Parallel B' },
      { id: 'join', type: 'atomic', name: 'Join' },
    ],
    flows: [
      { from: 'start', to: 'parallel-a' },
      { from: 'start', to: 'parallel-b' },
      { from: 'parallel-a', to: 'join' },
      { from: 'parallel-b', to: 'join' },
    ],
  },

  /**
   * Conditional workflow with choice
   */
  conditional: {
    id: 'conditional-workflow',
    name: 'Conditional Workflow',
    description: 'Workflow with conditional branching',
    tasks: [
      { id: 'check', type: 'atomic', name: 'Check Condition' },
      { id: 'path-a', type: 'atomic', name: 'Path A' },
      { id: 'path-b', type: 'atomic', name: 'Path B' },
      { id: 'merge', type: 'atomic', name: 'Merge' },
    ],
    flows: [
      { from: 'check', to: 'path-a', condition: 'valueA' },
      { from: 'check', to: 'path-b', condition: 'valueB' },
      { from: 'path-a', to: 'merge' },
      { from: 'path-b', to: 'merge' },
    ],
  },

  /**
   * Document approval workflow
   */
  approval: {
    id: 'document-approval',
    name: 'Document Approval',
    description: 'Multi-stage document approval workflow',
    tasks: [
      { id: 'submit', type: 'atomic', name: 'Submit Document' },
      { id: 'review', type: 'atomic', name: 'Automated Review' },
      { id: 'approve', type: 'atomic', name: 'Manager Approval' },
      { id: 'publish', type: 'atomic', name: 'Publish Document' },
    ],
    flows: [
      { from: 'submit', to: 'review' },
      { from: 'review', to: 'approve' },
      { from: 'approve', to: 'publish' },
    ],
  },
};

/**
 * Sample workflow case data
 */
export const sampleCaseData = {
  /**
   * Document submission data
   */
  documentSubmission: {
    submitter: 'user1',
    document: 'contract-2025.pdf',
    amount: 1000,
    priority: 'high',
    timestamp: '2025-01-01T00:00:00Z',
  },

  /**
   * Approval request data
   */
  approvalRequest: {
    requester: 'user2',
    type: 'expense',
    amount: 5000,
    category: 'travel',
    description: 'Conference travel',
  },

  /**
   * Review task data
   */
  reviewTask: {
    reviewer: 'automated-system',
    decision: 'approved',
    confidence: 0.95,
    checks: ['format', 'content', 'metadata'],
  },
};

/**
 * Sample hook definitions
 */
export const sampleHooks = {
  /**
   * Validation hook
   */
  validation: {
    meta: {
      name: 'validation-hook',
      description: 'Validates data before processing',
      version: '1.0.0',
    },
    when: {
      kind: 'sparql-ask',
      ref: { uri: 'file://test.rq', sha256: 'test-hash' },
    },
    run: async (delta, context) => {
      // Simple validation logic
      if (!delta || !delta.additions) {
        return { veto: true, reason: 'Missing delta data' };
      }
      return { veto: false };
    },
  },

  /**
   * Logging hook
   */
  logging: {
    meta: {
      name: 'logging-hook',
      description: 'Logs all transactions',
      version: '1.0.0',
    },
    when: {
      kind: 'always',
    },
    run: async (delta, context) => {
      console.log(`Transaction: ${delta.id}`, {
        additions: delta.additions?.length || 0,
        removals: delta.removals?.length || 0,
      });
      return { veto: false };
    },
  },
};

/**
 * Sample SPARQL queries
 */
export const sampleQueries = {
  /**
   * Get all persons
   */
  allPersons: `
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    SELECT ?person ?name WHERE {
      ?person a foaf:Person ;
              foaf:name ?name .
    }
  `,

  /**
   * Get documents by creator
   */
  documentsByCreator: `
    PREFIX dc: <http://purl.org/dc/terms/>
    SELECT ?doc ?title WHERE {
      ?doc dc:creator <http://example.org/person/john> ;
           dc:title ?title .
    }
  `,

  /**
   * Count triples
   */
  countTriples: `
    SELECT (COUNT(*) as ?count) WHERE {
      ?s ?p ?o .
    }
  `,
};

/**
 * Performance test data
 */
export const performanceFixtures = {
  /**
   * Generate N quads for stress testing
   * @param {number} count - Number of quads to generate
   * @returns {Array} Array of quads
   */
  generateQuads(count) {
    const quads = [];
    for (let i = 0; i < count; i++) {
      quads.push(
        createQuad(
          `http://example.org/subject/${i}`,
          'http://example.org/predicate',
          `http://example.org/object/${i}`
        )
      );
    }
    return quads;
  },

  /**
   * Generate large workflow with N tasks
   * @param {number} taskCount - Number of tasks
   * @returns {Object} Workflow specification
   */
  generateLargeWorkflow(taskCount) {
    const tasks = [];
    const flows = [];

    for (let i = 0; i < taskCount; i++) {
      tasks.push({
        id: `task-${i}`,
        type: 'atomic',
        name: `Task ${i}`,
      });

      if (i > 0) {
        flows.push({
          from: `task-${i - 1}`,
          to: `task-${i}`,
        });
      }
    }

    return {
      id: `large-workflow-${taskCount}`,
      name: `Large Workflow (${taskCount} tasks)`,
      description: `Generated workflow with ${taskCount} tasks`,
      tasks,
      flows,
    };
  },
};

/**
 * Error scenarios for testing error handling
 */
export const errorScenarios = {
  /**
   * Invalid RDF data
   */
  invalidQuad: {
    subject: null, // Invalid - should be named node
    predicate: { value: 'http://example.org/pred', termType: 'NamedNode' },
    object: { value: 'value', termType: 'Literal' },
  },

  /**
   * Malformed workflow
   */
  malformedWorkflow: {
    id: 'malformed',
    // Missing required fields: name, tasks, flows
  },

  /**
   * Circular workflow
   */
  circularWorkflow: {
    id: 'circular',
    name: 'Circular Workflow',
    tasks: [
      { id: 'a', type: 'atomic', name: 'A' },
      { id: 'b', type: 'atomic', name: 'B' },
    ],
    flows: [
      { from: 'a', to: 'b' },
      { from: 'b', to: 'a' }, // Creates cycle
    ],
  },
};
