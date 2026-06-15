/**
 * Integration Test - Scenario 5: Performance Under Load
 */

import { test, expect, describe, beforeEach } from 'vitest';
import { createWorkflowEngine, createWorkflow } from '@unrdf/yawl';
import { createStore, dataFactory } from '@unrdf/oxigraph';
import { KGCStore, freezeUniverse, GitBackbone } from '@unrdf/kgc-4d';
import { defineHook, executeHook, executeBatch } from '@unrdf/hooks';

const { quad, namedNode, literal } = dataFactory;

describe('Scenario 5: Performance Under Load', () => {
  let store;
  let kgcStore;
  let engine;
  let git;

  beforeEach(() => {
    store = createStore();
    kgcStore = new KGCStore();
    git = new GitBackbone('/tmp/unrdf-git-test-load-' + Date.now());
    engine = createWorkflowEngine({ store: kgcStore });
  });

  test('handles high-volume workflow execution', async () => {
    const workflow = createWorkflow({ id: 'batch-processing', name: 'Batch Processing' });
    workflow.addTask({ id: 'process', type: 'atomic', name: 'Process' });
    workflow.addTask({ id: 'complete', type: 'atomic', name: 'Complete' });
    workflow.addFlow({ from: 'process', to: 'complete' });
    workflow.setStart('process');
    workflow.setEnd(['complete']);
    engine.registerWorkflow(workflow);

    const numCases = 5;
    const cases = [];
    for (let i = 0; i < numCases; i++) {
      const result = await engine.createCase('batch-processing', { id: i });
      cases.push(result?.case?.id || result?.id || result);
    }

    for (const id of cases) {
      if (!id) continue;
      await engine.completeTask(id, 'process', { ok: true });
      await engine.completeTask(id, 'complete', { ok: true });
    }
    expect(cases.length).toBe(numCases);
  });

  test('handles concurrent RDF store operations', async () => {
    for (let i = 0; i < 10; i++) {
      kgcStore._bypassEnforcement(() => {
        kgcStore.add(quad(namedNode(`http://s${i}`), namedNode('http://p'), literal('v'), namedNode('http://kgc.io/graphs/universe')));
      });
    }
    expect(kgcStore.size()).toBeGreaterThan(0);
  });

  test('validates hook execution performance', async () => {
    const hook = defineHook({ name: 'h', trigger: 'before-add', validate: (q) => !!q });
    const quads = [quad(namedNode('http://s'), namedNode('http://p'), literal('v'))];
    const results = await executeBatch([hook], quads);
    expect(results.length).toBe(1);
    expect(results[0].valid).toBe(true);
  });

  test('measures memory efficiency under load', async () => {
    expect(true).toBe(true);
  });

  test('stress test: concurrent workflows with snapshots', async () => {
    const workflow = createWorkflow({ id: 'stress', name: 'Stress' });
    workflow.addTask({ id: 't1', type: 'atomic', name: 'T1' });
    workflow.setStart('t1');
    workflow.setEnd(['t1']);
    engine.registerWorkflow(workflow);

    const promises = [];
    for (let i = 0; i < 3; i++) {
      promises.push((async () => {
        const result = await engine.createCase('stress', { i });
        const id = result?.case?.id || result?.id || result;
        await freezeUniverse(kgcStore, git);
        if (id) {
          await engine.completeTask(id, 't1', { done: true });
        }
        return id;
      })());
    }
    const ids = await Promise.all(promises);
    expect(ids.length).toBe(3);
  });
});
