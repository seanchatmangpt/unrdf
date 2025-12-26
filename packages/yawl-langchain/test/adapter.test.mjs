/**
 * Tests for YAWL-LangChain Adapter
 *
 * @module @unrdf/yawl-langchain/test/adapter
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { createStore, dataFactory } from '@unrdf/oxigraph';
import { TaskInstance, TaskDefinition } from '@unrdf/yawl';
import {
  YAWLLangChainAdapter,
  createLangChainTaskExecutor,
  createPromptEngineeringHook,
  YAWL_LC_NS,
} from '../src/adapter.mjs';

const { quad, namedNode, literal } = dataFactory;

// =============================================================================
// Mock LangChain Agent
// =============================================================================

class MockAgent {
  constructor(responseText = 'Mock LLM response') {
    this.responseText = responseText;
    this.invocations = [];
  }

  async invoke(input) {
    this.invocations.push(input);
    return {
      output: this.responseText,
    };
  }

  getInvocations() {
    return this.invocations;
  }
}

// =============================================================================
// Test Suite
// =============================================================================

describe('YAWLLangChainAdapter', () => {
  let adapter;
  let mockAgent;
  let taskInstance;
  let rdfStore;

  beforeEach(() => {
    mockAgent = new MockAgent('Test analysis result');
    adapter = new YAWLLangChainAdapter({
      taskId: 'test-task',
      taskName: 'Test Task',
      agent: mockAgent,
      contextQuery: 'SELECT ?value WHERE { ?s <http://test.org/property> ?value }',
      rdfPredicate: 'http://test.org/result',
      timeout: 5000,
    });

    // Create RDF store with test data
    rdfStore = createStore();
    rdfStore.add(
      quad(
        namedNode('http://test.org/subject'),
        namedNode('http://test.org/property'),
        literal('test-value')
      )
    );

    // Create task instance
    const taskDef = new TaskDefinition({
      id: 'test-task',
      kind: 'AtomicTask',
    });
    taskInstance = new TaskInstance(taskDef, 'case-001', {
      inputData: { input: 'test input' },
    });
  });

  describe('Constructor and Configuration', () => {
    it('should create adapter with valid config', () => {
      expect(adapter).toBeDefined();
      expect(adapter.config.taskId).toBe('test-task');
      expect(adapter.config.taskName).toBe('Test Task');
      expect(adapter.config.timeout).toBe(5000);
    });

    it('should throw error for invalid config', () => {
      expect(() => {
        new YAWLLangChainAdapter({ taskId: '' }); // Empty taskId
      }).toThrow();
    });

    it('should set default values for optional fields', () => {
      const simpleAdapter = new YAWLLangChainAdapter({
        taskId: 'simple-task',
        agent: mockAgent,
      });

      expect(simpleAdapter.config.outputField).toBe('output');
      expect(simpleAdapter.config.timeout).toBe(30000);
    });
  });

  describe('Task Definition Creation', () => {
    it('should create valid YAWL TaskDefinition', () => {
      const taskDef = adapter.createTaskDefinition();

      expect(taskDef).toBeInstanceOf(TaskDefinition);
      expect(taskDef.id).toBe('test-task');
      expect(taskDef.name).toBe('Test Task');
      expect(taskDef.kind).toBe('AtomicTask');
      expect(taskDef.timeout).toBe(5000);
    });

    it('should include pre-condition and post-condition hooks', () => {
      const taskDef = adapter.createTaskDefinition();

      expect(taskDef.preCondition).toBeDefined();
      expect(taskDef.postCondition).toBeDefined();
      expect(typeof taskDef.preCondition).toBe('function');
      expect(typeof taskDef.postCondition).toBe('function');
    });

    it('should merge additional options', () => {
      const taskDef = adapter.createTaskDefinition({
        inputConditions: ['start'],
        outputConditions: ['end'],
      });

      expect(taskDef.inputConditions).toEqual(['start']);
      expect(taskDef.outputConditions).toEqual(['end']);
    });
  });

  describe('Agent Execution', () => {
    it('should execute LangChain agent successfully', async () => {
      const result = await adapter.execute(taskInstance, { rdfStore });

      expect(result).toBeDefined();
      expect(result.output).toBe('Test analysis result');
      expect(result.executionTime).toBeGreaterThan(0n);
      expect(result.metadata).toBeDefined();
      expect(result.metadata.taskId).toBe(taskInstance.id);
    });

    it('should pass input data to agent', async () => {
      await adapter.execute(taskInstance, { rdfStore });

      const invocations = mockAgent.getInvocations();
      expect(invocations.length).toBe(1);
      expect(invocations[0].input).toContain('test input');
    });

    it('should handle agent without RDF context', async () => {
      const simpleAdapter = new YAWLLangChainAdapter({
        taskId: 'simple',
        agent: mockAgent,
      });

      const result = await simpleAdapter.execute(taskInstance, {});

      expect(result.output).toBe('Test analysis result');
      expect(result.metadata.contextUsed).toBe(false);
    });

    it('should handle string response from agent', async () => {
      const stringAgent = {
        invoke: async () => 'Direct string response',
      };

      const stringAdapter = new YAWLLangChainAdapter({
        taskId: 'string-task',
        agent: stringAgent,
      });

      const result = await stringAdapter.execute(taskInstance, {});
      expect(result.output).toBe('Direct string response');
    });

    it('should handle execution errors gracefully', async () => {
      const failingAgent = {
        invoke: async () => {
          throw new Error('LLM API error');
        },
      };

      const failingAdapter = new YAWLLangChainAdapter({
        taskId: 'failing-task',
        agent: failingAgent,
      });

      await expect(
        failingAdapter.execute(taskInstance, {})
      ).rejects.toThrow('LangChain agent execution failed');
    });
  });

  describe('RDF Context Integration', () => {
    it('should query RDF context using SPARQL', async () => {
      const result = await adapter.execute(taskInstance, { rdfStore });

      expect(result.metadata.contextUsed).toBe(true);
    });

    it('should inject RDF context into prompt', async () => {
      await adapter.execute(taskInstance, { rdfStore });

      const invocations = mockAgent.getInvocations();
      const prompt = invocations[0].input;

      expect(prompt).toContain('Knowledge Graph Context');
      expect(prompt).toContain('test-value');
    });

    it('should handle SPARQL query errors gracefully', async () => {
      const badAdapter = new YAWLLangChainAdapter({
        taskId: 'bad-query',
        agent: mockAgent,
        contextQuery: 'INVALID SPARQL', // Invalid query
      });

      // Should not throw, just warn and continue
      const result = await badAdapter.execute(taskInstance, { rdfStore });
      expect(result).toBeDefined();
    });

    it('should work without RDF store when no context query configured', async () => {
      const noContextAdapter = new YAWLLangChainAdapter({
        taskId: 'no-context',
        agent: mockAgent,
      });

      const result = await noContextAdapter.execute(taskInstance, {});
      expect(result.metadata.contextUsed).toBe(false);
    });
  });

  describe('RDF Triple Storage', () => {
    it('should store agent output as RDF triples', async () => {
      const result = await adapter.execute(taskInstance, { rdfStore });

      expect(result.rdfTriples).toBeDefined();
      expect(result.rdfTriples.length).toBeGreaterThan(0);
    });

    it('should store execution metadata', async () => {
      await adapter.execute(taskInstance, { rdfStore });

      const store = adapter.getRDFStore();
      const quads = [...store.quads()];

      // Should have triples for output, timestamp, execution time
      expect(quads.length).toBeGreaterThanOrEqual(3);

      const predicates = quads.map(q => q.predicate.value);
      expect(predicates).toContain(YAWL_LC_NS.agentOutput);
      expect(predicates).toContain(YAWL_LC_NS.executedAt);
      expect(predicates).toContain(YAWL_LC_NS.executionTime);
    });

    it('should store with custom RDF predicate', async () => {
      await adapter.execute(taskInstance, { rdfStore });

      const store = adapter.getRDFStore();
      const quads = [...store.quads()];

      const customPredicate = quads.find(
        q => q.predicate.value === 'http://test.org/result'
      );
      expect(customPredicate).toBeDefined();
      expect(customPredicate.object.value).toBe('Test analysis result');
    });

    it('should export RDF store as Turtle', async () => {
      await adapter.execute(taskInstance, { rdfStore });

      const turtle = adapter.exportAsTurtle();
      expect(turtle).toBeDefined();
      expect(typeof turtle).toBe('string');
      expect(turtle.length).toBeGreaterThan(0);
    });
  });

  describe('Execution History', () => {
    it('should record execution history', async () => {
      await adapter.execute(taskInstance, { rdfStore });

      const history = adapter.getExecutionHistory();
      expect(history.length).toBe(1);
      expect(history[0].taskId).toBe(taskInstance.id);
      expect(history[0].output).toBe('Test analysis result');
    });

    it('should track multiple executions', async () => {
      await adapter.execute(taskInstance, { rdfStore });

      const taskInstance2 = new TaskInstance(
        adapter.createTaskDefinition(),
        'case-002'
      );
      await adapter.execute(taskInstance2, { rdfStore });

      const history = adapter.getExecutionHistory();
      expect(history.length).toBe(2);
    });

    it('should include execution metrics in history', async () => {
      await adapter.execute(taskInstance, { rdfStore });

      const history = adapter.getExecutionHistory();
      const record = history[0];

      expect(record.executionTime).toBeDefined();
      expect(record.prompt).toBeDefined();
      expect(record.rdfTriples).toBeGreaterThan(0);
      expect(record.timestamp).toBeDefined();
    });
  });

  describe('Prompt Engineering', () => {
    it('should build prompt from template', async () => {
      const templateAdapter = new YAWLLangChainAdapter({
        taskId: 'template-task',
        agent: mockAgent,
        promptTemplate: 'Process this: {input}',
      });

      taskInstance.setInputData({ input: 'test data' });
      await templateAdapter.execute(taskInstance, {});

      const invocations = mockAgent.getInvocations();
      expect(invocations[0].input).toContain('Process this: test data');
    });

    it('should map input data according to inputMapping', async () => {
      const mappedAdapter = new YAWLLangChainAdapter({
        taskId: 'mapped-task',
        agent: mockAgent,
        inputMapping: {
          yawlField: 'langchainField',
        },
      });

      taskInstance.setInputData({ yawlField: 'mapped value' });
      await mappedAdapter.execute(taskInstance, {});

      const invocations = mockAgent.getInvocations();
      expect(invocations[0].langchainField).toBe('mapped value');
    });
  });

  describe('Helper Functions', () => {
    it('should create task executor function', async () => {
      const executor = createLangChainTaskExecutor({
        taskId: 'executor-task',
        agent: mockAgent,
      });

      expect(typeof executor).toBe('function');

      const result = await executor(taskInstance, { rdfStore });
      expect(result.output).toBe('Test analysis result');
      expect(taskInstance.getOutputData().executedBy).toBe('langchain-adapter');
    });

    it('should create prompt engineering hook', async () => {
      const hook = createPromptEngineeringHook(async (prompt) => {
        return `Enhanced: ${prompt}`;
      });

      expect(typeof hook).toBe('function');

      taskInstance.setInputData({ prompt: 'original' });
      const result = await hook({ taskInstance });

      expect(result.valid).toBe(true);
      expect(taskInstance.inputData.prompt).toBe('Enhanced: original');
    });
  });
});

describe('Integration Tests', () => {
  it('should integrate with YAWL workflow lifecycle', async () => {
    const mockAgent = new MockAgent('Integration test result');
    const adapter = new YAWLLangChainAdapter({
      taskId: 'integration-task',
      agent: mockAgent,
      rdfPredicate: 'http://test.org/integration',
    });

    // Create task definition
    const taskDef = adapter.createTaskDefinition();
    expect(taskDef).toBeInstanceOf(TaskDefinition);

    // Create task instance
    const taskInstance = new TaskInstance(taskDef, 'integration-case-001', {
      inputData: { test: 'data' },
    });

    // Execute
    const result = await adapter.execute(taskInstance, {});

    // Verify results
    expect(result.output).toBe('Integration test result');
    expect(result.rdfTriples.length).toBeGreaterThan(0);
    expect(adapter.getExecutionHistory().length).toBe(1);
  });

  it('should chain multiple agents with RDF context sharing', async () => {
    const agent1 = new MockAgent('First agent result');
    const agent2 = new MockAgent('Second agent result');

    const adapter1 = new YAWLLangChainAdapter({
      taskId: 'chain-task-1',
      agent: agent1,
      rdfPredicate: 'http://test.org/step1',
    });

    const adapter2 = new YAWLLangChainAdapter({
      taskId: 'chain-task-2',
      agent: agent2,
      rdfPredicate: 'http://test.org/step2',
    });

    const taskDef1 = adapter1.createTaskDefinition();
    const taskDef2 = adapter2.createTaskDefinition();

    const task1 = new TaskInstance(taskDef1, 'chain-case', {
      inputData: { input: 'initial' },
    });
    const task2 = new TaskInstance(taskDef2, 'chain-case', {
      inputData: { input: 'follow-up' },
    });

    // Execute first agent
    const result1 = await adapter1.execute(task1, {});
    expect(result1.output).toBe('First agent result');

    // Execute second agent (could access first agent's RDF output)
    const result2 = await adapter2.execute(task2, {});
    expect(result2.output).toBe('Second agent result');

    // Both adapters maintain their own RDF stores
    expect([...adapter1.getRDFStore().quads()].length).toBeGreaterThan(0);
    expect([...adapter2.getRDFStore().quads()].length).toBeGreaterThan(0);
  });
});
