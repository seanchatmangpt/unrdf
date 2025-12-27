/**
 * @file Comprehensive validation tests for KGC Runtime schemas
 * @description Tests all schemas with valid/invalid inputs, edge cases, and defaults
 */

import { describe, it, expect } from 'vitest';
import {
  ReceiptSchema,
  RunCapsuleSchema,
  ToolTraceEntrySchema,
  BoundsSchema,
  WorkItemSchema,
  ProjectionManifestSchema,
  KGCMarkdownSchema,
  validateReceipt,
  validateRunCapsule,
  validateToolTraceEntry,
  validateBounds,
  validateWorkItem,
  validateProjectionManifest,
  validateKGCMarkdown,
} from '../src/schemas.mjs';

// =============================================================================
// Receipt Schema Tests
// =============================================================================

describe('ReceiptSchema', () => {
  it('should validate a complete receipt', () => {
    const receipt = {
      version: '1.0.0',
      id: '550e8400-e29b-41d4-a716-446655440000',
      timestamp: 1703001600000,
      runId: 'run-2024-001',
      actor: 'agent:orchestrator',
      action: 'execute',
      payload: { workflowId: 'wf-001', input: { x: 42 } },
      result: {
        success: true,
        output: { y: 84 },
        duration: 2314,
      },
      contentHash: 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855',
      previousHash: 'a1b2c3d4e5f67890123456789012345678901234567890123456789012345678',
      signature: {
        algorithm: 'ed25519',
        publicKey: '0x1234567890abcdef',
        value: '0xabcdef1234567890',
      },
    };

    const result = validateReceipt(receipt);
    expect(result.success).toBe(true);
    expect(result.data).toBeDefined();
    expect(result.data.version).toBe('1.0.0');
  });

  it('should validate minimal receipt', () => {
    const receipt = {
      id: '550e8400-e29b-41d4-a716-446655440000',
      timestamp: 1703001600000,
      runId: 'run-001',
      actor: 'agent:test',
      action: 'execute',
      payload: {},
    };

    const result = validateReceipt(receipt);
    expect(result.success).toBe(true);
    expect(result.data.version).toBe('1.0.0'); // Default
  });

  it('should reject invalid actor format', () => {
    const receipt = {
      id: '550e8400-e29b-41d4-a716-446655440000',
      timestamp: 1703001600000,
      runId: 'run-001',
      actor: 'invalid-actor', // Missing type prefix
      action: 'execute',
      payload: {},
    };

    const result = validateReceipt(receipt);
    expect(result.success).toBe(false);
    expect(result.errors.length).toBeGreaterThan(0);
  });

  it('should reject invalid UUID', () => {
    const receipt = {
      id: 'not-a-uuid',
      timestamp: 1703001600000,
      runId: 'run-001',
      actor: 'agent:test',
      action: 'execute',
      payload: {},
    };

    const result = validateReceipt(receipt);
    expect(result.success).toBe(false);
  });

  it('should reject invalid SHA-256 hash', () => {
    const receipt = {
      id: '550e8400-e29b-41d4-a716-446655440000',
      timestamp: 1703001600000,
      runId: 'run-001',
      actor: 'agent:test',
      action: 'execute',
      payload: {},
      contentHash: 'invalid-hash', // Not 64 hex chars
    };

    const result = validateReceipt(receipt);
    expect(result.success).toBe(false);
  });
});

// =============================================================================
// ToolTraceEntry Schema Tests
// =============================================================================

describe('ToolTraceEntrySchema', () => {
  it('should validate a complete tool trace entry', () => {
    const entry = {
      id: '123e4567-e89b-12d3-a456-426614174000',
      timestamp: 1703001600000,
      toolName: 'Bash',
      input: { command: 'npm test', timeout: 5000 },
      output: { stdout: '✅ All tests passed', stderr: '', exitCode: 0 },
      duration: 2314,
      status: 'success',
      parentId: null,
      dependencies: [],
      resources: {
        cpuTime: 1200,
        memoryPeak: 45678901,
        ioBytes: 1024,
      },
    };

    const result = validateToolTraceEntry(entry);
    expect(result.success).toBe(true);
    expect(result.data.toolName).toBe('Bash');
    expect(result.data.status).toBe('success');
  });

  it('should validate minimal trace entry', () => {
    const entry = {
      id: '123e4567-e89b-12d3-a456-426614174000',
      timestamp: 1703001600000,
      toolName: 'Read',
      input: { file_path: '/home/user/file.txt' },
      duration: 10,
      status: 'success',
    };

    const result = validateToolTraceEntry(entry);
    expect(result.success).toBe(true);
  });

  it('should validate error status with error details', () => {
    const entry = {
      id: '123e4567-e89b-12d3-a456-426614174000',
      timestamp: 1703001600000,
      toolName: 'Bash',
      input: { command: 'invalid-command' },
      duration: 50,
      status: 'error',
      error: {
        message: 'Command not found',
        code: 'ENOENT',
      },
    };

    const result = validateToolTraceEntry(entry);
    expect(result.success).toBe(true);
    expect(result.data.error).toBeDefined();
  });
});

// =============================================================================
// RunCapsule Schema Tests
// =============================================================================

describe('RunCapsuleSchema', () => {
  it('should validate a complete run capsule', () => {
    const capsule = {
      id: 'run-2024-12-26-001',
      version: '1.0.0',
      startTime: 1703001600000,
      endTime: 1703001620000,
      status: 'completed',
      input: {
        task: 'Implement feature X',
        parameters: { timeout: 5000 },
        context: { workingDir: '/home/user/project' },
        artifacts: [
          {
            type: 'file',
            path: '/home/user/spec.md',
            hash: 'abc1234567890def1234567890abc1234567890def1234567890abc123456789',
          },
        ],
      },
      output: {
        success: true,
        results: { filesChanged: 5, testsAdded: 12 },
        artifacts: [
          {
            type: 'file',
            path: '/home/user/project/src/feature.mjs',
            hash: 'def1234567890abc1234567890def1234567890abc1234567890def123456789',
            size: 4567,
          },
        ],
      },
      trace: [
        {
          id: '123e4567-e89b-12d3-a456-426614174000',
          timestamp: 1703001605000,
          toolName: 'Bash',
          input: { command: 'npm test' },
          duration: 2000,
          status: 'success',
        },
      ],
      bounds: {
        maxFiles: 100,
        maxBytes: 10485760,
        maxOps: 1000,
        maxRuntime: 300000,
      },
      actor: 'agent:backend-dev',
    };

    const result = validateRunCapsule(capsule);
    expect(result.success).toBe(true);
    expect(result.data.status).toBe('completed');
  });

  it('should validate minimal run capsule', () => {
    const capsule = {
      id: 'run-001',
      startTime: 1703001600000,
      status: 'pending',
      input: {
        task: 'Simple task',
      },
      actor: 'agent:test',
    };

    const result = validateRunCapsule(capsule);
    expect(result.success).toBe(true);
    expect(result.data.version).toBe('1.0.0'); // Default
  });

  it('should validate failed run with error', () => {
    const capsule = {
      id: 'run-002',
      startTime: 1703001600000,
      endTime: 1703001605000,
      status: 'failed',
      input: { task: 'Failing task' },
      output: {
        success: false,
        error: {
          message: 'Test failed',
          code: 'TEST_FAILURE',
          recoverable: true,
        },
      },
      actor: 'agent:test',
    };

    const result = validateRunCapsule(capsule);
    expect(result.success).toBe(true);
    expect(result.data.output.success).toBe(false);
  });
});

// =============================================================================
// Bounds Schema Tests
// =============================================================================

describe('BoundsSchema', () => {
  it('should validate complete bounds with usage', () => {
    const bounds = {
      maxFiles: 100,
      maxBytes: 10485760,
      maxOps: 1000,
      maxRuntime: 300000,
      maxGraphRewrites: 50,
      enforcementPolicy: 'strict',
      warnings: {
        filesThreshold: 0.8,
        bytesThreshold: 0.9,
        opsThreshold: 0.75,
        runtimeThreshold: 0.9,
        graphRewritesThreshold: 0.8,
      },
      currentUsage: {
        files: 45,
        bytes: 5242880,
        ops: 500,
        runtime: 120000,
        graphRewrites: 20,
      },
    };

    const result = validateBounds(bounds);
    expect(result.success).toBe(true);
    expect(result.data.enforcementPolicy).toBe('strict');
  });

  it('should apply defaults', () => {
    const bounds = {};

    const result = validateBounds(bounds);
    expect(result.success).toBe(true);
    expect(result.data.maxFiles).toBe(100);
    expect(result.data.maxBytes).toBe(10 * 1024 * 1024);
    expect(result.data.maxOps).toBe(1000);
    expect(result.data.maxRuntime).toBe(300000);
    expect(result.data.maxGraphRewrites).toBe(50);
    expect(result.data.enforcementPolicy).toBe('strict');
  });

  it('should reject invalid enforcement policy', () => {
    const bounds = {
      enforcementPolicy: 'invalid',
    };

    const result = validateBounds(bounds);
    expect(result.success).toBe(false);
  });

  it('should reject out-of-range values', () => {
    const bounds = {
      maxFiles: 100000, // Exceeds max of 10000
    };

    const result = validateBounds(bounds);
    expect(result.success).toBe(false);
  });
});

// =============================================================================
// WorkItem Schema Tests
// =============================================================================

describe('WorkItemSchema', () => {
  it('should validate complete work item', () => {
    const workItem = {
      id: '123e4567-e89b-12d3-a456-426614174000',
      type: 'file_operation',
      state: 'running',
      priority: 75,
      createdAt: 1703001600000,
      startedAt: 1703001605000,
      payload: {
        operation: 'write',
        path: '/home/user/file.txt',
        content: 'Hello World',
      },
      dependencies: ['223e4567-e89b-12d3-a456-426614174000'],
      retries: {
        max: 3,
        current: 0,
        backoff: 'exponential',
        delay: 1000,
      },
      timeout: 30000,
      assignedTo: 'agent:worker-01',
      progress: 0.5,
    };

    const result = validateWorkItem(workItem);
    expect(result.success).toBe(true);
    expect(result.data.state).toBe('running');
    expect(result.data.priority).toBe(75);
  });

  it('should validate minimal work item', () => {
    const workItem = {
      id: '123e4567-e89b-12d3-a456-426614174000',
      type: 'task',
      state: 'queued',
      createdAt: 1703001600000,
      payload: {},
    };

    const result = validateWorkItem(workItem);
    expect(result.success).toBe(true);
    expect(result.data.priority).toBe(50); // Default
    expect(result.data.timeout).toBe(30000); // Default
  });

  it('should validate failed work item with error', () => {
    const workItem = {
      id: '123e4567-e89b-12d3-a456-426614174000',
      type: 'computation',
      state: 'failed',
      createdAt: 1703001600000,
      startedAt: 1703001605000,
      completedAt: 1703001610000,
      payload: { x: 42 },
      error: {
        message: 'Division by zero',
        code: 'MATH_ERROR',
        retryable: false,
      },
    };

    const result = validateWorkItem(workItem);
    expect(result.success).toBe(true);
    expect(result.data.state).toBe('failed');
  });

  it('should reject invalid priority range', () => {
    const workItem = {
      id: '123e4567-e89b-12d3-a456-426614174000',
      type: 'task',
      state: 'queued',
      priority: 150, // Exceeds max of 100
      createdAt: 1703001600000,
      payload: {},
    };

    const result = validateWorkItem(workItem);
    expect(result.success).toBe(false);
  });
});

// =============================================================================
// ProjectionManifest Schema Tests
// =============================================================================

describe('ProjectionManifestSchema', () => {
  it('should validate complete projection manifest', () => {
    const manifest = {
      version: '1.0.0',
      surfaces: {
        cli: {
          commands: [
            {
              name: 'run',
              description: 'Execute a workflow',
              aliases: ['r'],
              options: [
                {
                  name: 'file',
                  type: 'string',
                  required: true,
                  description: 'Workflow file path',
                },
              ],
              examples: ['kgc run workflow.yml'],
            },
          ],
        },
        docs: {
          generator: 'typedoc',
          outputDir: './docs',
          includes: ['**/*.mjs'],
          theme: 'default',
        },
        ide: {
          lsp: {
            enabled: true,
            port: 9000,
            features: ['completion', 'hover', 'goto'],
          },
          snippets: [
            {
              prefix: 'run',
              body: 'RunCapsuleSchema.parse({ ... })',
              description: 'Create run capsule',
            },
          ],
        },
      },
    };

    const result = validateProjectionManifest(manifest);
    expect(result.success).toBe(true);
    expect(result.data.surfaces.cli).toBeDefined();
  });

  it('should validate minimal manifest', () => {
    const manifest = {
      surfaces: {},
    };

    const result = validateProjectionManifest(manifest);
    expect(result.success).toBe(true);
    expect(result.data.version).toBe('1.0.0'); // Default
  });

  it('should validate API surface', () => {
    const manifest = {
      surfaces: {
        api: {
          type: 'rest',
          baseUrl: 'https://api.example.com',
          endpoints: [
            {
              path: '/runs',
              method: 'POST',
              auth: 'bearer',
            },
          ],
          versioning: {
            strategy: 'url',
            current: '1.0.0',
          },
        },
      },
    };

    const result = validateProjectionManifest(manifest);
    expect(result.success).toBe(true);
  });
});

// =============================================================================
// KGCMarkdown Schema Tests
// =============================================================================

describe('KGCMarkdownSchema', () => {
  it('should validate complete markdown document', () => {
    const doc = {
      type: 'document',
      frontMatter: {
        title: 'KGC Example',
        version: '1.0.0',
        author: 'Test Author',
        date: new Date('2024-12-26'),
        ontology: ['http://schema.org/'],
        tags: ['example', 'test'],
      },
      children: [
        {
          type: 'heading',
          level: 1,
          content: 'Introduction',
          id: 'intro',
          metadata: {},
        },
        {
          type: 'paragraph',
          content: 'This is a paragraph.',
          metadata: {},
        },
        {
          type: 'fenced-block',
          language: 'javascript',
          attributes: { executable: true },
          content: 'console.log("Hello");',
          executable: true,
          metadata: {},
        },
      ],
      metadata: {},
    };

    const result = validateKGCMarkdown(doc);
    expect(result.success).toBe(true);
    expect(result.data.children.length).toBe(3);
  });

  it('should validate minimal document', () => {
    const doc = {
      type: 'document',
      children: [
        {
          type: 'paragraph',
          content: 'Simple paragraph',
          metadata: {},
        },
      ],
    };

    const result = validateKGCMarkdown(doc);
    expect(result.success).toBe(true);
  });

  it('should validate list node', () => {
    const doc = {
      type: 'document',
      children: [
        {
          type: 'list',
          ordered: false,
          items: ['Item 1', 'Item 2', 'Item 3'],
          metadata: {},
        },
      ],
    };

    const result = validateKGCMarkdown(doc);
    expect(result.success).toBe(true);
  });

  it('should validate table node', () => {
    const doc = {
      type: 'document',
      children: [
        {
          type: 'table',
          headers: ['Column 1', 'Column 2'],
          rows: [
            ['A', 'B'],
            ['C', 'D'],
          ],
          alignment: ['left', 'right'],
          metadata: {},
        },
      ],
    };

    const result = validateKGCMarkdown(doc);
    expect(result.success).toBe(true);
  });

  it('should reject invalid heading level', () => {
    const doc = {
      type: 'document',
      children: [
        {
          type: 'heading',
          level: 7, // Invalid, max is 6
          content: 'Invalid heading',
          metadata: {},
        },
      ],
    };

    const result = validateKGCMarkdown(doc);
    expect(result.success).toBe(false);
  });
});

// =============================================================================
// Integration Tests - Schema Composition
// =============================================================================

describe('Schema Integration', () => {
  it('should compose RunCapsule with ToolTraceEntry and Receipt', () => {
    const trace1 = {
      id: '123e4567-e89b-12d3-a456-426614174000',
      timestamp: 1703001600000,
      toolName: 'Bash',
      input: { command: 'npm test' },
      duration: 2000,
      status: 'success',
    };

    const trace2 = {
      id: '223e4567-e89b-12d3-a456-426614174000',
      timestamp: 1703001602000,
      toolName: 'Write',
      input: { file_path: '/home/user/file.txt', content: 'test' },
      duration: 50,
      status: 'success',
    };

    const receipt = {
      id: '550e8400-e29b-41d4-a716-446655440000',
      timestamp: 1703001610000,
      runId: 'run-001',
      actor: 'agent:test',
      action: 'execute',
      payload: {},
      result: { success: true },
    };

    const capsule = {
      id: 'run-001',
      startTime: 1703001600000,
      endTime: 1703001610000,
      status: 'completed',
      input: { task: 'Test task' },
      output: { success: true, results: {} },
      trace: [trace1, trace2],
      actor: 'agent:test',
      receipt,
    };

    const result = validateRunCapsule(capsule);
    expect(result.success).toBe(true);
    expect(result.data.trace.length).toBe(2);
    expect(result.data.receipt).toBeDefined();
  });

  it('should compose WorkItem with Bounds', () => {
    const bounds = {
      maxFiles: 50,
      maxBytes: 5242880,
      maxOps: 500,
      maxRuntime: 60000,
      maxGraphRewrites: 25,
    };

    const workItem = {
      id: '123e4567-e89b-12d3-a456-426614174000',
      type: 'bounded_task',
      state: 'running',
      createdAt: 1703001600000,
      payload: { bounds },
    };

    const result = validateWorkItem(workItem);
    expect(result.success).toBe(true);
    expect(result.data.payload.bounds.maxFiles).toBe(50);
  });
});
