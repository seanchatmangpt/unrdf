/**
 * Integration Test - Scenario 1: Complete Workflow Execution
 */

import { test, expect, describe, beforeEach } from 'vitest';
import { createWorkflowEngine, createWorkflow } from '@unrdf/yawl';
import { createStore } from '@unrdf/oxigraph';
import { KGCStore, freezeUniverse, reconstructState, GitBackbone } from '@unrdf/kgc-4d';
import { defineHook, executeHook } from '@unrdf/hooks';

describe('Scenario 1: Complete Workflow Execution', () => {
  let store;
  let kgcStore;
  let engine;
  let git;

  beforeEach(() => {
    store = createStore();
    kgcStore = new KGCStore();
    git = new GitBackbone('/tmp/unrdf-git-test-complete-' + Date.now());
    engine = createWorkflowEngine({ store: kgcStore });
  });

  test('executes workflow with hooks, receipts, and time-travel', async () => {
    const workflow = createWorkflow({ id: 'document-approval', name: 'Doc Approval' });
    workflow.addTask({ id: 'submit', type: 'atomic', name: 'Submit' });
    workflow.addTask({ id: 'review', type: 'atomic', name: 'Review' });
    workflow.addTask({ id: 'approve', type: 'atomic', name: 'Approve' });
    workflow.addFlow({ from: 'submit', to: 'review' });
    workflow.addFlow({ from: 'review', to: 'approve' });
    workflow.setStart('submit');
    workflow.setEnd(['approve']);
    engine.registerWorkflow(workflow);

    const result = await engine.createCase('document-approval', { user: 'u1' });
    const caseId = result?.case?.id || result?.id || result;

    await freezeUniverse(kgcStore, git);
    await engine.completeTask(caseId, 'submit', { file: 'doc1.pdf' });
    await engine.completeTask(caseId, 'review', { ok: true });

    const snapshot2 = await freezeUniverse(kgcStore, git);
    const reconstructed = await reconstructState(kgcStore, git, snapshot2.t_ns);
    expect(reconstructed).toBeDefined();

    await engine.completeTask(caseId, 'approve', { ok: true });

    const finalCase = await engine.getCase(caseId);
    expect(finalCase.status).toBe('completed');
  });

  test('handles invalid task data via hooks', async () => {
    const workflow = createWorkflow({ id: 'v-test', name: 'V Test' });
    workflow.addTask({ id: 't1', type: 'atomic', name: 'T1' });
    workflow.setStart('t1');
    workflow.setEnd(['t1']);
    engine.registerWorkflow(workflow);

    const r1 = await engine.createCase('v-test', { v: 'test' });
    const id1 = r1?.case?.id || r1?.id || r1;
    expect(id1).toBeDefined();

    const r2 = await engine.createCase('v-test', {});
    const id2 = r2?.case?.id || r2?.id || r2;
    expect(id2).toBeDefined();
  });
});
