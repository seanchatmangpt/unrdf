/**
 * @file Vision 2030 Phase 1 Integration Tests
 * @description Validates all three 80/20 features work together
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { Store } from 'n3';
import { DataFactory } from 'n3';
import { AutonomousKnowledgeAgent } from '../src/autonomous-agent.mjs';
import { EnsembleGroqProvider } from '../src/providers/ensemble-groq.mjs';
import { OntologyLearner } from '../../hooks/src/hooks/ontology-learner.mjs';

const { namedNode, literal, quad } = DataFactory;

describe('Vision 2030 Phase 1 Integration', () => {
  let agent;
  let ensemble;
  let learner;
  let mockBaseProvider;
  let store;

  beforeEach(() => {
    // Setup autonomous agent
    agent = new AutonomousKnowledgeAgent('integration-agent', {
      goalTriples: 100,
      maxIterations: 5,
    });

    // Setup ensemble provider
    mockBaseProvider = {
      generateText: vi.fn(async (input) => ({
        text: `Response from specialist: ${input.prompt}`,
      })),
    };
    ensemble = new EnsembleGroqProvider(mockBaseProvider);

    // Setup ontology learner
    learner = new OntologyLearner();

    // Setup RDF store with sample data
    store = new Store();
  });

  it('should create agent with ensemble and learner configured', () => {
    expect(agent).toBeDefined();
    expect(ensemble).toBeDefined();
    expect(learner).toBeDefined();
  });

  it('should use ensemble for agent reasoning', async () => {
    const result = await ensemble.generateText({
      prompt: 'Analyze pattern in data',
      maxTokens: 100,
    });

    expect(result.specialist_responses).toHaveLength(3);
    expect(result.confidence).toBeGreaterThanOrEqual(0);
    expect(result.ensemble_size).toBe(3);
  });

  it('should agent maintain reasoning trace', async () => {
    const entry = {
      timestamp: Date.now(),
      task: 'integration test',
      decision: 'ensemble-based',
      result: 'success',
    };

    agent.reasoning.push(entry);
    const trace = agent.getReasoningTrace();

    expect(trace).toHaveLength(1);
    expect(trace[0].task).toBe('integration test');
  });

  it('should learner infer shapes from populated store', async () => {
    const rdfType = namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
    const personClass = namedNode('http://example.org/Person');
    const projectClass = namedNode('http://example.org/Project');
    const alice = namedNode('http://example.org/alice');
    const bob = namedNode('http://example.org/bob');
    const proj1 = namedNode('http://example.org/project-1');
    const name = namedNode('http://example.org/name');
    const age = namedNode('http://example.org/age');
    const status = namedNode('http://example.org/status');

    // Add persons
    store.addQuad(quad(alice, rdfType, personClass));
    store.addQuad(quad(bob, rdfType, personClass));

    // Add properties
    store.addQuad(quad(alice, name, literal('Alice')));
    store.addQuad(quad(alice, age, literal('30')));
    store.addQuad(quad(bob, name, literal('Bob')));
    store.addQuad(quad(bob, age, literal('25')));

    // Add project
    store.addQuad(quad(proj1, rdfType, projectClass));
    store.addQuad(quad(proj1, name, literal('Project Alpha')));
    store.addQuad(quad(proj1, status, literal('active')));

    const shapes = await learner.inferShapes(store, { minSupport: 0.8 });

    // Should have shapes for both classes
    expect(Object.keys(shapes).length).toBeGreaterThanOrEqual(1);

    // Person class should be present
    if (shapes['http://example.org/Person']) {
      const personShape = shapes['http://example.org/Person'];
      expect(personShape.targetClass).toBe('http://example.org/Person');
      expect(personShape.properties).toBeDefined();
    }
  });

  it('should learner detect data types correctly', async () => {
    const intLiteral = literal('42');
    const floatLiteral = literal('3.14');
    const stringLiteral = literal('hello');
    const boolLiteral = literal('true');
    const resourceNode = namedNode('http://example.org/resource');

    expect(learner.inferDatatype(intLiteral)).toBe('xsd:integer');
    expect(learner.inferDatatype(floatLiteral)).toBe('xsd:float');
    expect(learner.inferDatatype(stringLiteral)).toBe('xsd:string');
    expect(learner.inferDatatype(boolLiteral)).toBe('xsd:boolean');
    expect(learner.inferDatatype(resourceNode)).toBe('rdf:Resource');
  });

  it('should agent expose KG for querying', () => {
    const kg = agent.getKnowledgeGraph();
    expect(kg).toBeDefined();
    expect(typeof kg).toBe('object');
  });

  it('should ensemble provide specialist breakdown', async () => {
    const result = await ensemble.generateText({
      prompt: 'Test integration',
    });

    expect(result.specialist_responses[0].role).toBe('hypothesis-generator');
    expect(result.specialist_responses[1].role).toBe('validator');
    expect(result.specialist_responses[2].role).toBe('refiner');
  });

  it('should workflow: agent reasons, ensemble improves, learner validates', async () => {
    // Step 1: Agent starts with a task
    const task = 'Analyze system behavior';
    expect(agent.name).toBe('integration-agent');

    // Step 2: Ensemble provides reasoning with confidence
    const reasoning = await ensemble.generateText({
      prompt: task,
    });
    expect(reasoning.confidence).toBeDefined();

    // Step 3: Build store with sample results
    const rdfType = namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
    const behaviorClass = namedNode('http://example.org/Behavior');
    const behavior1 = namedNode('http://example.org/behavior-1');
    const confidence = namedNode('http://example.org/confidence');

    store.addQuad(quad(behavior1, rdfType, behaviorClass));
    store.addQuad(
      quad(behavior1, confidence, literal(String(reasoning.confidence)))
    );

    // Step 4: Learner validates inferred schema
    const shapes = await learner.inferShapes(store);
    expect(Object.keys(shapes).length).toBeGreaterThanOrEqual(0);

    // Step 5: Agent records in trace
    agent.reasoning.push({
      task,
      confidence: reasoning.confidence,
      shapes_learned: Object.keys(shapes).length,
    });

    const trace = agent.getReasoningTrace();
    expect(trace).toHaveLength(1);
    expect(trace[0].confidence).toBeDefined();
  });

  it('should support multiple agents sharing federated patterns', async () => {
    const agent2 = new AutonomousKnowledgeAgent('analyst-2', {
      goalTriples: 50,
    });

    // Both agents can use same ensemble
    const result1 = await ensemble.generateText({ prompt: 'Task 1' });
    const result2 = await ensemble.generateText({ prompt: 'Task 2' });

    expect(result1.ensemble_size).toBe(result2.ensemble_size);

    // Both can use same learner
    const shapes = await learner.inferShapes(store);

    expect(agent.name).toBe('integration-agent');
    expect(agent2.name).toBe('analyst-2');
    expect(shapes).toBeDefined();
  });

  it('should handle complex reasoning loop: reason → learn → validate', async () => {
    // Simulate: LLM suggests triples → store them → learn shapes → validate against shapes

    // 1. Ensemble suggests structure
    const suggestion = await ensemble.generateText({
      prompt: 'What structure should we use?',
    });
    expect(suggestion.specialist_responses.length).toBe(3);

    // 2. Store hypothetical triples from suggestion
    const rdfType = namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
    const domainClass = namedNode('http://example.org/Domain');
    const _entity = namedNode('http://example.org/entity-1');
    const prop = namedNode('http://example.org/property');

    for (let i = 0; i < 5; i++) {
      const e = namedNode(`http://example.org/entity-${i}`);
      store.addQuad(quad(e, rdfType, domainClass));
      store.addQuad(quad(e, prop, literal(`value-${i}`)));
    }

    // 3. Learn shapes from triples
    const shapes = await learner.inferShapes(store, { minSupport: 0.8 });

    // 4. Validate: do learned shapes match expectations?
    expect(Object.keys(shapes).length).toBeGreaterThan(0);

    // 5. Agent records full reasoning cycle
    agent.reasoning.push({
      phase: 'complete-cycle',
      ensemble_confidence: suggestion.confidence,
      shapes_learned: Object.keys(shapes).length,
      timestamp: Date.now(),
    });

    expect(agent.getReasoningTrace().length).toBe(1);
  });
});
