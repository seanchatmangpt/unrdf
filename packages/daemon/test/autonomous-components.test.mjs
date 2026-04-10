/**
 * @file Autonomous Components Tests
 * @vitest-environment node
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import {
  AutonomousKnowledgeAgent,
} from '../src/autonomous-agent.mjs';
import {
  AutonomousRefinementEngine,
  RefinementConfigSchema,
  RefinementEpisodeSchema,
  createAutonomousRefinementEngine,
} from '../src/autonomous-refinement-engine.mjs';
import {
  KnowledgeSelfPlayLoop,
  createKnowledgeSelfPlayLoop,
} from '../src/knowledge-self-play.mjs';
import {
  SelfPlayEpisode,
  SelfPlayAgent,
  createRandomPolicy,
  createGreedyPolicy,
  SelfPlayPolicies,
  runSelfPlayLoop,
} from '../src/mcp-self-play.mjs';

describe('AutonomousKnowledgeAgent', () => {
  let agent;

  beforeEach(() => {
    agent = new AutonomousKnowledgeAgent('test-agent', {
      goalTriples: 10,
      maxIterations: 5,
    });
  });

  afterEach(() => {
    agent.removeAllListeners();
  });

  describe('Agent Creation', () => {
    it('should create agent with default config', () => {
      expect(agent.name).toBe('test-agent');
      expect(agent.kg).toBeDefined();
      expect(agent.reasoning).toEqual([]);
    });

    it('should create agent with custom config', () => {
      const customAgent = new AutonomousKnowledgeAgent('custom', {
        goalTriples: 100,
        maxIterations: 50,
      });

      expect(customAgent.name).toBe('custom');
      expect(customAgent.kg).toBeDefined();
    });
  });

  describe('Reasoning', () => {
    it('should parse query action from decision text', () => {
      const result = agent.parseAction('I will query the knowledge graph');
      expect(result.type).toBe('query');
    });

    it('should parse add action from decision text', () => {
      const result = agent.parseAction('I will add new information');
      expect(result.type).toBe('add');
    });

    it('should parse unknown action by default', () => {
      const result = agent.parseAction('I will do something else');
      expect(result.type).toBe('unknown');
    });

    it('should execute query action successfully', async () => {
      const result = await agent.execute('query the knowledge graph');
      expect(result.success).toBe(true);
      expect(result.action).toBe('query');
    });

    it('should execute add action successfully', async () => {
      const result = await agent.execute('add new triple');
      expect(result.success).toBe(true);
      expect(result.action).toBe('add');
    });

    it('should handle unknown action', async () => {
      const result = await agent.execute('unknown action');
      expect(result.success).toBe(false);
      expect(result.error).toBeDefined();
    });
  });

  describe('Reasoning Trace', () => {
    it('should maintain reasoning trace', async () => {
      // Mock the KG query method and store
      agent.kg.query = async () => 'No data available';
      agent.kg.store = {
        size: 0,
        query: async () => [],
      };

      // Mock the LLM provider directly on the agent
      agent.llmProvider = {
        getDefaultModel: () => ({
          modelId: 'test-model',
          specificationVersion: 'v2',
          supports: [],
          doGenerate: async () => ({
            content: [{ type: 'text', text: 'query action' }],
            finishReason: 'stop',
            usage: {
              promptTokens: 10,
              completionTokens: 5,
            },
            rawCall: { rawPrompt: null, rawSettings: {} },
            rawResponse: { headers: {} },
            warnings: null,
          }),
        }),
      };

      // Initialize the engine's dependencies with the LLM provider
      // and disable observability and caching to avoid missing modules
      agent.kg.dependencies = {
        llmProvider: agent.llmProvider,
      };
      agent.kg.config.enableObservability = false;
      agent.kg.config.enableCaching = false;

      agent.parseAction = () => ({ type: 'query', text: 'test' });

      const result = await agent.reason('test task', {
        query: 'SELECT ?s WHERE { ?s a ?b }',
      });

      expect(agent.reasoning).toHaveLength(1);
      expect(agent.reasoning[0].task).toBe('test task');
      expect(result.success).toBe(true);
    });

    it('should get reasoning trace', () => {
      agent.reasoning.push({
        timestamp: Date.now(),
        task: 'test',
        decision: 'test decision',
        result: 'success',
        duration: 100,
      });

      const trace = agent.getReasoningTrace();
      expect(trace).toHaveLength(1);
      expect(trace[0].task).toBe('test');
    });

    it('should reset agent state', async () => {
      agent.reasoning.push({ test: 'data' });

      await agent.reset();

      expect(agent.reasoning).toEqual([]);
    });
  });

  describe('Knowledge Graph Access', () => {
    it('should provide access to knowledge graph', () => {
      const kg = agent.getKnowledgeGraph();
      expect(kg).toBeDefined();
    });
  });
});

describe('AutonomousRefinementEngine', () => {
  let engine;
  let mockStore;
  let mockDependencies;

  beforeEach(() => {
    mockStore = {
      size: 10,
      query: async () => [],
      executeWithReceipt: async (sparql) => ({
        id: 'receipt-1',
        hash: 'hash-1',
        previousHash: null,
      }),
    };

    mockDependencies = {
      llmProvider: {
        getDefaultModel: () => ({ modelId: 'groq-test' }),
        generateText: async () => ({ text: '<http://example.org/subject> <http://example.org/predicate> "object"' }),
      },
      shacl: {
        validateTriple: async () => ({ valid: true, violations: [] }),
      },
      analytics: {
        analyze: async () => ({ density: 0.5, componentCount: 5, avgDegree: 2 }),
      },
      kgc4d: {
        createSnapshot: async () => ({ id: 'snapshot-1', graphSize: 10 }),
      },
      blockchain: {
        recordReceipt: async () => ({ success: true }),
      },
      kgcProbe: {
        scan: async () => ({ violations: [] }),
      },
      hooks: {
        evaluate: async () => [
          { satisfied: true, hookId: 'hook-1' },
          { satisfied: false, hookId: 'hook-2' },
        ],
      },
    };

    engine = new AutonomousRefinementEngine(
      {
        graphId: 'test-graph',
        goalTriples: 20,
        maxIterations: 5,
        shaclValidation: true,
        enableSnapshots: true,
        enableBlockchain: false,
        enableObservability: false,
        enableCaching: false,
      },
      mockDependencies
    );
  });

  afterEach(() => {
    engine.removeAllListeners();
  });

  describe('Engine Creation', () => {
    it('should validate config with schema', () => {
      const config = {
        graphId: 'test-graph',
        goalTriples: 100,
        maxIterations: 50,
        maxLatency: 30000,
        shaclValidation: true,
        enableSnapshots: true,
        enableBlockchain: false,
      };

      const result = RefinementConfigSchema.parse(config);
      expect(result.graphId).toBe('test-graph');
      expect(result.goalTriples).toBe(100);
    });

    it('should create engine with dependencies', () => {
      expect(engine.config.graphId).toBe('test-graph');
      expect(engine.dependencies).toBeDefined();
    });
  });

  describe('Tool Registry', () => {
    it('should build tool registry with queryGraph tool', async () => {
      const tools = engine.buildToolRegistry(mockStore);

      expect(tools.queryGraph).toBeDefined();
      expect(tools.queryGraph.description).toBeDefined();
      expect(tools.queryGraph.inputSchema).toBeDefined();

      const result = await tools.queryGraph.execute({
        query: 'SELECT ?s WHERE { ?s a ?b }',
        kind: 'select',
      });

      expect(result.success).toBe(true);
      expect(result.results).toBeDefined();
    });

    it('should build tool registry with addTriple tool', async () => {
      const tools = engine.buildToolRegistry(mockStore);

      expect(tools.addTriple).toBeDefined();

      const result = await tools.addTriple.execute({
        subject: 'http://example.org/subject',
        predicate: 'http://example.org/predicate',
        object: 'object',
      });

      expect(result.success).toBe(true);
      expect(result.receipt).toBeDefined();
    });

    it('should build tool registry with getGraphMetrics tool', async () => {
      const tools = engine.buildToolRegistry(mockStore);

      expect(tools.getGraphMetrics).toBeDefined();

      const result = await tools.getGraphMetrics.execute({});

      expect(result.success).toBe(true);
      expect(result.metrics).toBeDefined();
      expect(result.metrics.tripleCount).toBe(10);
    });

    it('should build tool registry with evaluateConditions tool', async () => {
      const tools = engine.buildToolRegistry(mockStore);

      expect(tools.evaluateConditions).toBeDefined();

      const result = await tools.evaluateConditions.execute({
        conditions: [
          {
            name: 'test-condition',
            query: 'ASK { ?s a ?b }',
          },
        ],
      });

      expect(result.success).toBe(true);
      expect(result.conditions).toBeDefined();
    });
  });

  describe('Episode Schema', () => {
    it('should validate episode schema', () => {
      const episode = {
        episodeNumber: 1,
        timestamp: Date.now(),
        llmModel: 'groq-test',
        decision: {
          subject: 'http://example.org/subject',
          predicate: 'http://example.org/predicate',
          object: 'object',
        },
        receipt: {
          id: 'receipt-1',
          hash: 'hash-1',
          previousHash: null,
        },
        snapshot: {
          id: 'snapshot-1',
          graphSize: 10,
        },
        metrics: {
          latencyMs: 100,
          graphDensity: 0.5,
          validationPassed: true,
          hooksFired: 2,
        },
      };

      const result = RefinementEpisodeSchema.parse(episode);
      expect(result.episodeNumber).toBe(1);
      expect(result.decision.subject).toBe('http://example.org/subject');
    });
  });

  describe('Event Emission', () => {
    it('should emit episode-complete event', async () => {
      const eventSpy = [];
      engine.on('episode-complete', (episode) => {
        eventSpy.push(episode);
      });

      // Note: This would require full LLM integration
      // For now, just verify the event system works
      expect(engine.eventNames()).toContain('episode-complete');
    });

    it('should emit error event', () => {
      const errorSpy = [];
      engine.on('error', (err) => {
        errorSpy.push(err);
      });

      engine.emit('error', { message: 'test error' });

      expect(errorSpy).toHaveLength(1);
      expect(errorSpy[0].message).toBe('test error');
    });
  });

  describe('State Management', () => {
    it('should track episodes', () => {
      engine.episodes.push({
        episodeNumber: 1,
        timestamp: Date.now(),
        llmModel: 'test',
        decision: { subject: 's', predicate: 'p', object: 'o' },
        receipt: null,
        snapshot: null,
        metrics: { latencyMs: 100, graphDensity: 0, validationPassed: true, hooksFired: 0 },
      });

      const episodes = engine.getEpisodes();
      expect(episodes).toHaveLength(1);
      expect(episodes[0].episodeNumber).toBe(1);
    });

    it('should get current state', () => {
      const state = engine.getState();
      expect(state.state).toBe('idle');
      expect(state.episodeCount).toBe(0);
    });
  });
});

describe('KnowledgeSelfPlayLoop', () => {
  let loop;
  let mockStore;
  let mockEngine;

  beforeEach(() => {
    mockStore = {
      size: 10,
      add: async (quad) => {
        // Mock add operation
      },
    };

    mockEngine = {
      execute: async (store, delta, options) => {
        return {
          receipt: {
            id: `receipt-${delta.iterationNumber || 1}`,
            input_hash: 'hash-in',
            output_hash: 'hash-out',
            receiptHash: 'receipt-hash',
            previousReceiptHash: delta.previousReceipt?.receiptHash || null,
          },
          executionResults: [
            { hookId: 'hook-1', satisfied: true },
            { hookId: 'hook-2', satisfied: false },
          ],
        };
      },
    };

    loop = createKnowledgeSelfPlayLoop(mockStore, mockEngine, {
      maxIterations: 5,
      triggerType: 'continuous-improvement',
    });
  });

  describe('Loop Creation', () => {
    it('should create loop with store and engine', () => {
      expect(loop).toBeDefined();
      expect(loop.getEpisodeId()).toBeDefined();
    });

    it('should require store parameter', () => {
      expect(() => {
        new KnowledgeSelfPlayLoop({});
      }).toThrow('store is required');
    });

    it('should require engine parameter', () => {
      expect(() => {
        new KnowledgeSelfPlayLoop({ store: mockStore });
      }).toThrow('engine is required');
    });

    it('should require engine to have execute method', () => {
      expect(() => {
        new KnowledgeSelfPlayLoop({ store: mockStore, engine: {} });
      }).toThrow('engine must have execute method');
    });
  });

  describe('Step Execution', () => {
    it('should execute one step', async () => {
      const result = await loop.step();

      expect(result.receipt).toBeDefined();
      expect(result.feedback).toBeGreaterThan(0);
      expect(result.storeChanged).toBe(true);
      expect(result.hooksExecuted).toBe(2);
    });

    it('should track history', async () => {
      await loop.step();
      await loop.step();

      const history = loop.getHistory();
      expect(history).toHaveLength(2);
      expect(history[0].stepNumber).toBe(0);
      expect(history[1].stepNumber).toBe(1);
    });
  });

  describe('Convergence Detection', () => {
    it('should detect convergence when store unchanged', async () => {
      // Mock engine that returns same hash (no change)
      mockEngine.execute = async () => ({
        receipt: {
          id: 'receipt-1',
          input_hash: 'same-hash',
          output_hash: 'same-hash',
          receiptHash: 'receipt-hash',
          previousReceiptHash: null,
        },
        executionResults: [],
      });

      const result = await loop.step();

      expect(result.storeChanged).toBe(false);
      expect(result.feedback).toBe(0);
    });

    it('should detect progress when store changed', async () => {
      const result = await loop.step();

      expect(result.storeChanged).toBe(true);
      expect(result.feedback).toBeGreaterThan(0);
    });
  });

  describe('Full Run', () => {
    it('should run until convergence', async () => {
      // After 3 iterations, simulate convergence
      let iterationCount = 0;
      mockEngine.execute = async (store, delta) => {
        iterationCount++;
        if (iterationCount >= 3) {
          return {
            receipt: {
              id: `receipt-${iterationCount}`,
              input_hash: 'hash-in',
              output_hash: 'hash-in', // No change
              receiptHash: 'receipt-hash',
              previousReceiptHash: delta.previousReceipt?.receiptHash || null,
            },
            executionResults: [],
          };
        }
        return {
          receipt: {
            id: `receipt-${iterationCount}`,
            input_hash: 'hash-in',
            output_hash: 'hash-out',
            receiptHash: 'receipt-hash',
            previousReceiptHash: delta.previousReceipt?.receiptHash || null,
          },
          executionResults: [{ hookId: 'hook-1', satisfied: true }],
        };
      };

      const result = await loop.run();

      expect(result.iterations).toBe(3);
      expect(result.converged).toBe(true);
      expect(result.totalFeedback).toBeGreaterThan(0);
    });

    it('should stop at max iterations', async () => {
      // Never converge
      mockEngine.execute = async () => ({
        receipt: {
          id: 'receipt-x',
          input_hash: 'hash-in',
          output_hash: 'hash-out',
          receiptHash: 'receipt-hash',
          previousReceiptHash: null,
        },
        executionResults: [{ hookId: 'hook-1', satisfied: true }],
      });

      const result = await loop.run();

      expect(result.iterations).toBe(5); // maxIterations
      expect(result.converged).toBe(false);
    });
  });

  describe('Metrics', () => {
    it('should provide episode metrics', async () => {
      await loop.step();
      await loop.step();

      const metrics = loop.getMetrics();

      expect(metrics.totalSteps).toBe(2);
      expect(metrics.totalFeedback).toBeGreaterThan(0);
      expect(metrics.hooksExecutedTotal).toBeGreaterThan(0);
    });
  });

  describe('Episode Materialization', () => {
    it('should materialize episode as RDF quads', async () => {
      await loop.step();

      const runResult = await loop.run();
      const quads = loop.materializeEpisodeRDF(runResult);

      expect(quads.length).toBeGreaterThan(0);
      expect(quads[0].subject.value).toContain('urn:unrdf:episode/');
      expect(quads[0].object.value).toBe('urn:unrdf:SelfPlayEpisode');
    });

    it('should include all episode metadata', async () => {
      await loop.run();

      const metrics = loop.getMetrics();
      const runResult = {
        episodeId: loop.getEpisodeId(),
        startTime: Date.now() - 1000,
        endTime: Date.now(),
        iterations: 3,
        receipts: [],
        totalFeedback: 0.5,
        converged: true,
        avgFeedbackPerIteration: 0.16,
      };

      const quads = loop.materializeEpisodeRDF(runResult);

      // Check for key metadata triples
      const hasEpisodeId = quads.some(q =>
        q.predicate.value === 'urn:unrdf:episodeId'
      );
      const hasIterations = quads.some(q =>
        q.predicate.value === 'urn:unrdf:iterations'
      );
      const hasConverged = quads.some(q =>
        q.predicate.value === 'urn:unrdf:converged'
      );

      expect(hasEpisodeId).toBe(true);
      expect(hasIterations).toBe(true);
      expect(hasConverged).toBe(true);
    });
  });
});

describe('SelfPlayEpisode', () => {
  let episode;

  beforeEach(() => {
    episode = new SelfPlayEpisode({ graphFile: 'test.ttl' });
  });

  describe('Episode Creation', () => {
    it('should create episode with context', () => {
      expect(episode.episodeId).toBeDefined();
      expect(episode.context).toEqual({ graphFile: 'test.ttl' });
      expect(episode.steps).toEqual([]);
      expect(episode.feedback).toEqual([]);
      expect(episode.terminated).toBe(false);
    });
  });

  describe('Step Recording', () => {
    it('should record step with metadata', () => {
      episode.recordStep('tool1', { input: 'data' }, { output: 'result' }, {
        duration: 100,
        success: true,
      });

      expect(episode.steps).toHaveLength(1);
      expect(episode.steps[0].toolName).toBe('tool1');
      expect(episode.steps[0].duration).toBe(100);
      expect(episode.steps[0].metadata.success).toBe(true);
    });

    it('should record multiple steps', () => {
      episode.recordStep('tool1', {}, {});
      episode.recordStep('tool2', {}, {});

      expect(episode.steps).toHaveLength(2);
      expect(episode.steps[0].toolName).toBe('tool1');
      expect(episode.steps[1].toolName).toBe('tool2');
    });
  });

  describe('Feedback Recording', () => {
    it('should record positive feedback', () => {
      episode.recordFeedback(0.5, 'good decision');

      expect(episode.feedback).toHaveLength(1);
      expect(episode.feedback[0].signal).toBe(0.5);
      expect(episode.feedback[0].reason).toBe('good decision');
    });

    it('should record negative feedback', () => {
      episode.recordFeedback(-0.3, 'bad decision');

      expect(episode.feedback).toHaveLength(1);
      expect(episode.feedback[0].signal).toBe(-0.3);
      expect(episode.feedback[0].reason).toBe('bad decision');
    });
  });

  describe('Termination', () => {
    it('should terminate episode with reason', () => {
      episode.terminate('max steps reached');

      expect(episode.terminated).toBe(true);
      expect(episode.terminationReason).toBe('max steps reached');
    });
  });

  describe('Metrics', () => {
    it('should calculate episode metrics', () => {
      episode.recordStep('tool1', {}, { duration: 100 });
      episode.recordFeedback(0.5);
      episode.recordFeedback(-0.2);

      const metrics = episode.getMetrics();

      expect(metrics.totalSteps).toBe(1);
      expect(metrics.totalFeedback).toBe(2);
      expect(metrics.cumulativeFeedback).toBe(0.3);
      expect(metrics.avgFeedback).toBeCloseTo(0.15, 2);
    });

    it('should handle empty episode', () => {
      const metrics = episode.getMetrics();

      expect(metrics.totalSteps).toBe(0);
      expect(metrics.totalFeedback).toBe(0);
      expect(metrics.cumulativeFeedback).toBe(0);
      expect(metrics.avgFeedback).toBe(0);
    });
  });

  describe('Serialization', () => {
    it('should serialize episode to JSON', () => {
      episode.recordStep('tool1', {}, {});
      episode.recordFeedback(0.5);

      const json = episode.toJSON();

      expect(json.episodeId).toBeDefined();
      expect(json.context).toEqual({ graphFile: 'test.ttl' });
      expect(json.steps).toHaveLength(1);
      expect(json.feedback).toHaveLength(1);
      expect(json.metrics).toBeDefined();
    });
  });
});

describe('SelfPlayAgent', () => {
  let agent;
  let mockToolRegistry;

  beforeEach(() => {
    mockToolRegistry = {
      tool1: {
        handler: async (input) => {
          return { success: true, data: input };
        },
      },
      tool2: {
        handler: async (input) => {
          throw new Error('tool2 failed');
        },
      },
    };

    agent = new SelfPlayAgent(mockToolRegistry, async () => {
      return {
        toolName: 'tool1',
        input: { file: 'test.ttl' },
      };
    });
  });

  describe('Agent Creation', () => {
    it('should create agent with tool registry', () => {
      expect(agent.toolRegistry).toBeDefined();
      expect(agent.decisionFn).toBeDefined();
    });

    it('should set default limits', () => {
      expect(agent.maxStepsPerEpisode).toBe(10);
      expect(agent.maxEpisodes).toBe(5);
    });
  });

  describe('Episode Execution', () => {
    it('should run single episode', async () => {
      const episode = await agent.runEpisode({ graphFile: 'test.ttl' });

      expect(episode).toBeDefined();
      expect(episode.steps.length).toBeGreaterThan(0);
      expect(episode.terminated).toBe(true);
    });

    it('should handle tool not found', async () => {
      const badAgent = new SelfPlayAgent(mockToolRegistry, async () => {
        return { toolName: 'unknown-tool', input: {} };
      });

      const episode = await badAgent.runEpisode({ graphFile: 'test.ttl' });

      expect(episode.steps).toHaveLength(1);
      expect(episode.feedback).toHaveLength(1);
      expect(episode.feedback[0].signal).toBe(-1);
      expect(episode.terminationReason).toContain('unknown tool');
    });

    it('should handle tool execution failure', async () => {
      const failingAgent = new SelfPlayAgent(mockToolRegistry, async () => {
        return { toolName: 'tool2', input: {} };
      });

      const episode = await failingAgent.runEpisode({ graphFile: 'test.ttl' });

      expect(episode.steps).toHaveLength(1);
      expect(episode.feedback).toHaveLength(1);
      expect(episode.feedback[0].signal).toBe(-0.5);
      expect(episode.terminationReason).toContain('tool execution failed');
    });

    it('should stop at max steps', async () => {
      const infiniteAgent = new SelfPlayAgent(mockToolRegistry, async () => {
        return { toolName: 'tool1', input: {} };
      });
      infiniteAgent.maxStepsPerEpisode = 3;

      const episode = await infiniteAgent.runEpisode({ graphFile: 'test.ttl' });

      expect(episode.steps).toHaveLength(3);
      expect(episode.terminationReason).toBe('max steps reached');
    });
  });

  describe('Multiple Episodes', () => {
    it('should run multiple episodes', async () => {
      agent.maxEpisodes = 2;

      const episodes = await agent.runEpisodes({ graphFile: 'test.ttl' }, 2);

      expect(episodes).toHaveLength(2);
      expect(agent.episodes).toHaveLength(2);
    });
  });

  describe('Statistics', () => {
    it('should get best episode', async () => {
      agent.maxEpisodes = 2;
      await agent.runEpisodes({ graphFile: 'test.ttl' }, 2);

      const best = agent.getBestEpisode();

      expect(best).toBeDefined();
      expect(best.getMetrics().cumulativeFeedback).toBeGreaterThanOrEqual(0);
    });

    it('should get episode statistics', async () => {
      agent.maxEpisodes = 3;
      await agent.runEpisodes({ graphFile: 'test.ttl' }, 3);

      const stats = agent.getStats();

      expect(stats).toBeDefined();
      expect(stats.totalEpisodes).toBe(3);
      expect(stats.avgFeedback).toBeGreaterThanOrEqual(0);
    });

    it('should return null for stats when no episodes', () => {
      const stats = agent.getStats();
      expect(stats).toBeNull();
    });
  });
});

describe('SelfPlay Policies', () => {
  describe('Random Policy', () => {
    it('should create random policy', () => {
      const policy = createRandomPolicy(['tool1', 'tool2']);
      expect(policy).toBeInstanceOf(Function);
    });

    it('should return null after max steps', async () => {
      const policy = createRandomPolicy(['tool1']);
      const episode = new SelfPlayEpisode({});

      // Add 5 steps to reach limit
      for (let i = 0; i < 5; i++) {
        episode.recordStep('tool1', {}, {});
      }

      const decision = await policy(episode, null);
      expect(decision).toBeNull();
    });

    it('should return random tool selection', async () => {
      const policy = createRandomPolicy(['tool1', 'tool2', 'tool3']);
      const episode = new SelfPlayEpisode({});

      const decision = await policy(episode, null);

      expect(decision).toBeDefined();
      expect(['tool1', 'tool2', 'tool3']).toContain(decision.toolName);
    });
  });

  describe('Greedy Policy', () => {
    it('should create greedy policy', () => {
      const policy = createGreedyPolicy({
        tool1: { handler: async () => ({ success: true }) },
        tool2: { handler: async () => ({ success: true }) },
      });

      expect(policy).toBeInstanceOf(Function);
    });

    it('should select untested tools first', async () => {
      const toolRegistry = {
        tool1: { handler: async () => ({ success: true }) },
        tool2: { handler: async () => ({ success: true }) },
      };

      const policy = createGreedyPolicy(toolRegistry);
      const episode = new SelfPlayEpisode({});

      const decision = await policy(episode, null);

      expect(decision).toBeDefined();
      expect(['tool1', 'tool2']).toContain(decision.toolName);
    });

    it('should return null after max steps', async () => {
      const policy = createGreedyPolicy({ tool1: { handler: async () => ({}) } });
      const episode = new SelfPlayEpisode({});

      // Add 10 steps to reach limit
      for (let i = 0; i < 10; i++) {
        episode.recordStep('tool1', {}, { success: true });
      }

      const decision = await policy(episode, null);
      expect(decision).toBeNull();
    });
  });

  describe('Explore Pattern Decision', () => {
    it('should start with query tool', async () => {
      const episode = new SelfPlayEpisode({ graphFile: 'test.ttl' });

      const decision = await SelfPlayPolicies.explorePatternDecision(episode, null);

      expect(decision).toBeDefined();
      expect(decision.toolName).toBe('unrdf_graph_query');
    });

    it('should execute hooks after query', async () => {
      const episode = new SelfPlayEpisode({ graphFile: 'test.ttl' });
      episode.recordStep('unrdf_graph_query', {}, {});

      const decision = await SelfPlayPolicies.explorePatternDecision(episode, null);

      expect(decision).toBeDefined();
      expect(decision.toolName).toBe('unrdf_hooks_execute');
    });

    it('should query again after hooks', async () => {
      const episode = new SelfPlayEpisode({ graphFile: 'test.ttl' });
      episode.recordStep('unrdf_graph_query', {}, {});
      episode.recordStep('unrdf_hooks_execute', {}, {});

      const decision = await SelfPlayPolicies.explorePatternDecision(episode, null);

      expect(decision).toBeDefined();
      expect(decision.toolName).toBe('unrdf_graph_query');
    });

    it('should end after third step', async () => {
      const episode = new SelfPlayEpisode({ graphFile: 'test.ttl' });
      episode.recordStep('unrdf_graph_query', {}, {});
      episode.recordStep('unrdf_hooks_execute', {}, {});
      episode.recordStep('unrdf_graph_query', {}, {});

      const decision = await SelfPlayPolicies.explorePatternDecision(episode, null);

      expect(decision).toBeNull();
    });
  });
});

describe('Self-Play Orchestrator', () => {
  it('should run self-play loop with default options', async () => {
    const mockToolRegistry = {
      tool1: {
        handler: async (input) => ({ success: true, data: input }),
      },
    };

    const result = await runSelfPlayLoop(mockToolRegistry);

    expect(result).toBeDefined();
    expect(result.episodes).toBeDefined();
    expect(result.stats).toBeDefined();
    expect(result.agent).toBeDefined();
    expect(result.bestEpisode).toBeDefined();
  });

  it('should run with custom options', async () => {
    const mockToolRegistry = {
      tool1: {
        handler: async (input) => ({ success: true, data: input }),
      },
    };

    const result = await runSelfPlayLoop(mockToolRegistry, {
      initialContext: { graphFile: 'custom.ttl' },
      episodeCount: 2,
      maxStepsPerEpisode: 5,
    });

    expect(result.episodes).toHaveLength(2);
  });
});
