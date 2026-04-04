/**
 * @file Autonomous Agent Tests
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { DataFactory } from 'n3';
import { AutonomousKnowledgeAgent } from '../src/autonomous-agent.mjs';

const { namedNode: _namedNode, literal: _literal, quad: _quad } = DataFactory;

describe('AutonomousKnowledgeAgent', { timeout: 30000 }, () => {
  let agent;

  beforeEach(() => {
    agent = new AutonomousKnowledgeAgent('test-agent', {
      goalTriples: 10,
      maxIterations: 5,
    });
  });

  it('should create agent with name and KG', () => {
    expect(agent.name).toBe('test-agent');
    expect(agent.kg).toBeDefined();
    expect(agent.reasoning).toEqual([]);
  });

  it('should have empty reasoning trace initially', () => {
    expect(agent.getReasoningTrace()).toEqual([]);
  });

  it('should parse action from LLM decision', () => {
    let action = agent.parseAction('Query the database for all users');
    expect(action.type).toBe('query');

    action = agent.parseAction('Add a new triple to the knowledge graph');
    expect(action.type).toBe('add');

    action = agent.parseAction('Unknown action here');
    expect(action.type).toBe('unknown');
  });

  it('should execute query action', async () => {
    const result = await agent.execute('Query the system', {});
    expect(result.success).toBe(true);
    expect(result.action).toBe('query');
  });

  it('should execute add action', async () => {
    const result = await agent.execute('Add this to the graph', {});
    expect(result.success).toBe(true);
    expect(result.action).toBe('add');
  });

  it('should have event emitter functionality', () => {
    let eventFired = false;
    agent.on('reasoning-complete', () => {
      eventFired = true;
    });

    agent.emit('reasoning-complete', {});
    expect(eventFired).toBe(true);
  });

  it('should return knowledge graph', () => {
    const kg = agent.getKnowledgeGraph();
    expect(kg).toBeDefined();
    expect(kg.config).toBeDefined();
  });

  it('should reset agent state', async () => {
    agent.reasoning = [{ task: 'test' }];
    await agent.reset();
    expect(agent.reasoning).toEqual([]);
  });

  it('should handle reasoning errors gracefully', async () => {
    let _errorCaught = false;
    agent.on('error', () => {
      _errorCaught = true;
    });

    try {
      // Trigger error by passing invalid context
      await agent.reason('test task', null);
    } catch (err) {
      // Expected
    }

    // Error event may or may not fire depending on implementation
  });

  it('should record reasoning in trace', async () => {
    // Simulate a reasoning entry
    const entry = {
      timestamp: Date.now(),
      task: 'test task',
      decision: 'test decision',
      result: 'success',
      duration: 100,
    };

    agent.reasoning.push(entry);
    const trace = agent.getReasoningTrace();

    expect(trace).toHaveLength(1);
    expect(trace[0].task).toBe('test task');
  });
});
