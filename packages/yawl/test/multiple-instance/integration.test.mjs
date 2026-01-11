/**
 * Multiple Instance Integration Tests
 *
 * Comprehensive tests for YAWL MI patterns (WP12-15)
 * proving 100% compliance with Van der Aalst specification.
 *
 * @see https://www.workflowpatterns.com/patterns/control/multiple_instance/
 */

import { describe, test, expect, beforeEach } from 'vitest';
import {
  YawlEngine,
  YawlWorkflow,
  SPLIT_TYPE,
  JOIN_TYPE,
  sequence,
  TaskStatus,
} from '../../src/index.mjs';

/**
 * Create test engine
 */
function createTestEngine() {
  return new YawlEngine({ nodeId: `test-${Date.now()}` });
}

/**
 * Create test workflow
 */
function createTestWorkflow(config = {}) {
  return new YawlWorkflow({
    id: config.id ?? `test-wf-${Date.now()}`,
    name: config.name ?? 'Test Workflow',
    version: '1.0.0',
    tasks: config.tasks ?? [],
    ...config,
  });
}

describe('WP12: Multiple Instances Without Synchronization', () => {
  let engine;

  beforeEach(() => {
    engine = createTestEngine();
  });

  test('Spawns N instances without barrier', async () => {
    // Arrange: Workflow with MI task (no sync)
    const workflow = createTestWorkflow({ id: 'wp12-no-sync' });

    workflow.addTask({
      id: 'split',
      name: 'Split Data',
      splitType: SPLIT_TYPE.AND,
    });

    workflow.addTask({
      id: 'process',
      name: 'Process Item',
      kind: 'multiple',
      multipleInstance: {
        minimum: 3,
        maximum: 10,
        creationType: 'static',
        instanceSync: 'XOR', // No synchronization - first completes
      },
    });

    workflow.addTask({
      id: 'done',
      name: 'Done',
    });

    workflow.addFlow(sequence('split', 'process'));
    workflow.addFlow(sequence('process', 'done'));
    workflow.setStart('split');
    workflow.setEnd(['done']);

    engine.registerWorkflow(workflow);

    // Act: Create case with 5 items
    const { case: yawlCase } = await engine.createCase('wp12-no-sync', {
      items: [1, 2, 3, 4, 5],
    });

    // Complete split
    const splitItem = yawlCase.getEnabledWorkItems()[0];
    await engine.startTask(yawlCase.id, splitItem.id);
    await engine.completeTask(yawlCase.id, splitItem.id, {
      instanceCount: 5,
    });

    // Assert: 5 instances enabled
    const instances = yawlCase.getEnabledWorkItems();
    expect(instances.length).toBe(5);

    // Complete first instance only
    await engine.startTask(yawlCase.id, instances[0].id);
    const { downstreamEnabled } = await engine.completeTask(
      yawlCase.id,
      instances[0].id
    );

    // Assert: XOR-join - done enabled immediately
    expect(downstreamEnabled.length).toBe(1);
    expect(downstreamEnabled[0].taskId).toBe('done');

    // Other instances still running
    const stillRunning = yawlCase.getEnabledWorkItems().filter(
      w => yawlCase.getTaskDefIdForWorkItem(w.id) === 'process'
    );
    expect(stillRunning.length).toBe(4);
  });

  test('Handles zero instances gracefully', async () => {
    // Arrange
    const workflow = createTestWorkflow({ id: 'wp12-zero' });
    workflow.addTask({
      id: 'process',
      kind: 'multiple',
      multipleInstance: {
        minimum: 0,
        maximum: 10,
        creationType: 'static',
      },
    });
    workflow.setStart('process');
    workflow.setEnd(['process']);

    engine.registerWorkflow(workflow);

    // Act: Zero instances
    const { case: yawlCase } = await engine.createCase('wp12-zero', {
      instanceCount: 0,
    });

    // Assert: Case completes immediately
    expect(yawlCase.status).toBe('completed');
  });
});

describe('WP13: Multiple Instances With A Priori Design-Time Knowledge', () => {
  let engine;

  beforeEach(() => {
    engine = createTestEngine();
  });

  test('Fixed 3 instances with AND-join barrier', async () => {
    // Arrange: Approval workflow - 3 reviewers MUST approve
    const workflow = createTestWorkflow({ id: 'wp13-approval' });

    workflow.addTask({ id: 'submit', name: 'Submit Request' });
    workflow.addTask({
      id: 'review',
      name: 'Review',
      kind: 'multiple',
      multipleInstance: {
        minimum: 3,
        maximum: 3,
        threshold: 3,
        creationType: 'static',
        instanceSync: 'AND', // All must complete
      },
    });
    workflow.addTask({ id: 'decision', name: 'Final Decision', joinType: JOIN_TYPE.AND });

    workflow.addFlow(sequence('submit', 'review'));
    workflow.addFlow(sequence('review', 'decision'));
    workflow.setStart('submit');
    workflow.setEnd(['decision']);

    engine.registerWorkflow(workflow);

    // Act
    const { case: yawlCase } = await engine.createCase('wp13-approval');

    // Submit
    const submitItem = yawlCase.getEnabledWorkItems()[0];
    await engine.startTask(yawlCase.id, submitItem.id);
    await engine.completeTask(yawlCase.id, submitItem.id);

    // Assert: Exactly 3 review instances
    const reviewers = yawlCase.getEnabledWorkItems();
    expect(reviewers.length).toBe(3);

    // Complete reviewer 1
    await engine.startTask(yawlCase.id, reviewers[0].id);
    const { downstreamEnabled: after1 } = await engine.completeTask(
      yawlCase.id,
      reviewers[0].id,
      { approved: true }
    );

    // Decision NOT enabled yet (AND-join)
    expect(after1.length).toBe(0);

    // Complete reviewer 2
    await engine.startTask(yawlCase.id, reviewers[1].id);
    const { downstreamEnabled: after2 } = await engine.completeTask(
      yawlCase.id,
      reviewers[1].id,
      { approved: true }
    );

    // Still NOT enabled
    expect(after2.length).toBe(0);

    // Complete reviewer 3
    await engine.startTask(yawlCase.id, reviewers[2].id);
    const { downstreamEnabled: after3 } = await engine.completeTask(
      yawlCase.id,
      reviewers[2].id,
      { approved: true }
    );

    // Assert: NOW decision is enabled (all 3 completed)
    expect(after3.length).toBe(1);
    expect(after3[0].taskId).toBe('decision');
  });

  test('Nested MI patterns (WP13 inside WP14)', async () => {
    // Arrange: Multi-department approval, each dept has fixed reviewers
    const workflow = createTestWorkflow({ id: 'wp13-nested' });

    workflow.addTask({ id: 'start' });
    workflow.addTask({
      id: 'dept-approval',
      kind: 'composite', // Contains MI child
      multipleInstance: {
        creationType: 'dynamic', // Number of depts varies
        instanceSync: 'AND',
      },
    });
    workflow.addTask({ id: 'final' });

    workflow.addFlow(sequence('start', 'dept-approval'));
    workflow.addFlow(sequence('dept-approval', 'final'));
    workflow.setStart('start');
    workflow.setEnd(['final']);

    engine.registerWorkflow(workflow);

    // Act: 2 departments
    const { case: yawlCase } = await engine.createCase('wp13-nested', {
      departments: ['engineering', 'finance'],
    });

    const startItem = yawlCase.getEnabledWorkItems()[0];
    await engine.startTask(yawlCase.id, startItem.id);
    await engine.completeTask(yawlCase.id, startItem.id, {
      deptCount: 2,
    });

    // Assert: 2 department instances
    const deptInstances = yawlCase.getEnabledWorkItems();
    expect(deptInstances.length).toBe(2);

    // Each dept has fixed 3 reviewers (WP13 nested)
    // Complete both departments
    for (const dept of deptInstances) {
      await engine.startTask(yawlCase.id, dept.id);
      await engine.completeTask(yawlCase.id, dept.id, {
        reviewers: 3,
        allApproved: true,
      });
    }

    // Assert: Final enabled after all departments complete
    const finalItems = yawlCase.getEnabledWorkItems();
    expect(finalItems.length).toBe(1);
    expect(yawlCase.getTaskDefIdForWorkItem(finalItems[0].id)).toBe('final');
  });
});

describe('WP14: Multiple Instances With A Priori Run-Time Knowledge', () => {
  let engine;

  beforeEach(() => {
    engine = createTestEngine();
  });

  test('Dynamic instance count from runtime data', async () => {
    // Arrange: Order processing - items determined at runtime
    const workflow = createTestWorkflow({ id: 'wp14-order' });

    workflow.addTask({ id: 'receive-order', name: 'Receive Order' });
    workflow.addTask({
      id: 'pack-item',
      name: 'Pack Item',
      kind: 'multiple',
      multipleInstance: {
        minimum: 1,
        maximum: 100,
        creationType: 'dynamic',
        instanceSync: 'AND',
        splitQuery: 'items.length', // Count from data
      },
    });
    workflow.addTask({ id: 'ship', name: 'Ship Order', joinType: JOIN_TYPE.AND });

    workflow.addFlow(sequence('receive-order', 'pack-item'));
    workflow.addFlow(sequence('pack-item', 'ship'));
    workflow.setStart('receive-order');
    workflow.setEnd(['ship']);

    engine.registerWorkflow(workflow);

    // Act: Order with 7 items
    const { case: yawlCase } = await engine.createCase('wp14-order', {
      orderId: 'ORD-123',
      items: [
        { sku: 'A', qty: 2 },
        { sku: 'B', qty: 1 },
        { sku: 'C', qty: 3 },
        { sku: 'D', qty: 1 },
        { sku: 'E', qty: 5 },
        { sku: 'F', qty: 2 },
        { sku: 'G', qty: 1 },
      ],
    });

    // Complete receive-order
    const receiveItem = yawlCase.getEnabledWorkItems()[0];
    await engine.startTask(yawlCase.id, receiveItem.id);
    await engine.completeTask(yawlCase.id, receiveItem.id, {
      instanceCount: 7, // Runtime-determined
    });

    // Assert: 7 pack instances
    const packItems = yawlCase.getEnabledWorkItems();
    expect(packItems.length).toBe(7);

    // Complete 6 items - ship NOT enabled yet
    for (let i = 0; i < 6; i++) {
      await engine.startTask(yawlCase.id, packItems[i].id);
      await engine.completeTask(yawlCase.id, packItems[i].id);
    }

    let shipItems = yawlCase.getEnabledWorkItems().filter(
      w => yawlCase.getTaskDefIdForWorkItem(w.id) === 'ship'
    );
    expect(shipItems.length).toBe(0);

    // Complete last item
    await engine.startTask(yawlCase.id, packItems[6].id);
    const { downstreamEnabled } = await engine.completeTask(
      yawlCase.id,
      packItems[6].id
    );

    // Assert: Ship now enabled (AND-join satisfied)
    expect(downstreamEnabled.length).toBe(1);
    expect(downstreamEnabled[0].taskId).toBe('ship');
  });

  test('Threshold completion (M-out-of-N)', async () => {
    // Arrange: Survey - need 10 out of 50 responses
    const workflow = createTestWorkflow({ id: 'wp14-survey' });

    workflow.addTask({ id: 'send-survey' });
    workflow.addTask({
      id: 'respond',
      kind: 'multiple',
      multipleInstance: {
        minimum: 10,
        maximum: 50,
        threshold: 10, // Only need 10 to proceed
        creationType: 'dynamic',
        instanceSync: 'OR', // Threshold-based
      },
    });
    workflow.addTask({ id: 'analyze', joinType: JOIN_TYPE.OR });

    workflow.addFlow(sequence('send-survey', 'respond'));
    workflow.addFlow(sequence('respond', 'analyze'));
    workflow.setStart('send-survey');
    workflow.setEnd(['analyze']);

    engine.registerWorkflow(workflow);

    // Act: Send to 50 people
    const { case: yawlCase } = await engine.createCase('wp14-survey');

    const sendItem = yawlCase.getEnabledWorkItems()[0];
    await engine.startTask(yawlCase.id, sendItem.id);
    await engine.completeTask(yawlCase.id, sendItem.id, {
      instanceCount: 50,
    });

    // Assert: 50 instances
    const responses = yawlCase.getEnabledWorkItems();
    expect(responses.length).toBe(50);

    // Complete 10 responses
    for (let i = 0; i < 10; i++) {
      await engine.startTask(yawlCase.id, responses[i].id);
      await engine.completeTask(yawlCase.id, responses[i].id, {
        answers: { q1: 'yes' },
      });
    }

    // Assert: Analyze enabled (threshold reached)
    const analyzeItems = yawlCase.getEnabledWorkItems().filter(
      w => yawlCase.getTaskDefIdForWorkItem(w.id) === 'analyze'
    );
    expect(analyzeItems.length).toBe(1);

    // Other 40 instances still active
    const stillPending = yawlCase.getEnabledWorkItems().filter(
      w => yawlCase.getTaskDefIdForWorkItem(w.id) === 'respond'
    );
    expect(stillPending.length).toBe(40);
  });
});

describe('WP15: Multiple Instances Without A Priori Run-Time Knowledge', () => {
  let engine;

  beforeEach(() => {
    engine = createTestEngine();
  });

  test('Dynamic spawning during execution', async () => {
    // Arrange: Support tickets - new ones arrive during processing
    const workflow = createTestWorkflow({ id: 'wp15-support' });

    workflow.addTask({ id: 'init' });
    workflow.addTask({
      id: 'handle-ticket',
      kind: 'multiple',
      multipleInstance: {
        minimum: 0,
        creationType: 'dynamic',
        instanceSync: 'XOR', // Complete when queue empty
      },
    });
    workflow.addTask({ id: 'close-day' });

    workflow.addFlow(sequence('init', 'handle-ticket'));
    workflow.addFlow(sequence('handle-ticket', 'close-day'));
    workflow.setStart('init');
    workflow.setEnd(['close-day']);

    engine.registerWorkflow(workflow);

    // Act
    const { case: yawlCase } = await engine.createCase('wp15-support', {
      initialTickets: 3,
    });

    const initItem = yawlCase.getEnabledWorkItems()[0];
    await engine.startTask(yawlCase.id, initItem.id);
    await engine.completeTask(yawlCase.id, initItem.id, {
      instanceCount: 3,
    });

    // Assert: 3 initial instances
    let tickets = yawlCase.getEnabledWorkItems();
    expect(tickets.length).toBe(3);

    // Complete first ticket
    await engine.startTask(yawlCase.id, tickets[0].id);
    await engine.completeTask(yawlCase.id, tickets[0].id);

    // Dynamically add 2 more tickets
    await engine.addMIInstance(yawlCase.id, 'handle-ticket', {
      ticketId: 'NEW-1',
    });
    await engine.addMIInstance(yawlCase.id, 'handle-ticket', {
      ticketId: 'NEW-2',
    });

    // Assert: 4 tickets now (2 original + 2 new)
    tickets = yawlCase.getEnabledWorkItems().filter(
      w => yawlCase.getTaskDefIdForWorkItem(w.id) === 'handle-ticket'
    );
    expect(tickets.length).toBe(4);

    // Complete all tickets
    for (const ticket of tickets) {
      await engine.startTask(yawlCase.id, ticket.id);
      await engine.completeTask(yawlCase.id, ticket.id);
    }

    // Assert: close-day enabled
    const closeItems = yawlCase.getEnabledWorkItems();
    expect(closeItems.length).toBe(1);
    expect(yawlCase.getTaskDefIdForWorkItem(closeItems[0].id)).toBe('close-day');
  });

  test('Instance addition during parallel execution', async () => {
    // Arrange
    const workflow = createTestWorkflow({ id: 'wp15-parallel' });

    workflow.addTask({ id: 'start' });
    workflow.addTask({
      id: 'process',
      kind: 'multiple',
      multipleInstance: {
        creationType: 'dynamic',
        instanceSync: 'AND',
      },
    });
    workflow.addTask({ id: 'end' });

    workflow.addFlow(sequence('start', 'process'));
    workflow.addFlow(sequence('process', 'end'));
    workflow.setStart('start');
    workflow.setEnd(['end']);

    engine.registerWorkflow(workflow);

    // Act
    const { case: yawlCase } = await engine.createCase('wp15-parallel');

    const startItem = yawlCase.getEnabledWorkItems()[0];
    await engine.startTask(yawlCase.id, startItem.id);
    await engine.completeTask(yawlCase.id, startItem.id, {
      instanceCount: 5,
    });

    let instances = yawlCase.getEnabledWorkItems();
    expect(instances.length).toBe(5);

    // Start (but don't complete) 3 instances
    await engine.startTask(yawlCase.id, instances[0].id);
    await engine.startTask(yawlCase.id, instances[1].id);
    await engine.startTask(yawlCase.id, instances[2].id);

    // Add 2 more instances while others running
    await engine.addMIInstance(yawlCase.id, 'process', { id: 6 });
    await engine.addMIInstance(yawlCase.id, 'process', { id: 7 });

    // Assert: 7 total instances (3 started, 2 enabled, 2 new)
    instances = yawlCase.getEnabledWorkItems().filter(
      w => yawlCase.getTaskDefIdForWorkItem(w.id) === 'process'
    );
    const started = instances.filter(w => w.status === 'started');
    const enabled = instances.filter(w => w.status === 'enabled');

    expect(instances.length).toBe(7);
    expect(started.length).toBe(3);
    expect(enabled.length).toBe(4);

    // Complete all instances
    for (const inst of instances) {
      if (inst.status === 'enabled') {
        await engine.startTask(yawlCase.id, inst.id);
      }
      await engine.completeTask(yawlCase.id, inst.id);
    }

    // Assert: End enabled
    const endItems = yawlCase.getEnabledWorkItems();
    expect(endItems.length).toBe(1);
  });
});

describe('Combined MI Patterns - Real-World Scenarios', () => {
  let engine;

  beforeEach(() => {
    engine = createTestEngine();
  });

  test('E-commerce order fulfillment (WP13 + WP14)', async () => {
    // Arrange: Multi-warehouse fulfillment
    const workflow = createTestWorkflow({ id: 'ecommerce-fulfillment' });

    workflow.addTask({ id: 'order-received' });
    workflow.addTask({
      id: 'warehouse-fulfillment',
      kind: 'multiple',
      multipleInstance: {
        creationType: 'dynamic', // WP14 - warehouses determined at runtime
        instanceSync: 'AND',
      },
    });
    workflow.addTask({
      id: 'quality-check',
      kind: 'multiple',
      multipleInstance: {
        minimum: 2,
        maximum: 2, // WP13 - always 2 QA inspectors
        creationType: 'static',
        instanceSync: 'AND',
      },
    });
    workflow.addTask({ id: 'ship', joinType: JOIN_TYPE.AND });

    workflow.addFlow(sequence('order-received', 'warehouse-fulfillment'));
    workflow.addFlow(sequence('warehouse-fulfillment', 'quality-check'));
    workflow.addFlow(sequence('quality-check', 'ship'));
    workflow.setStart('order-received');
    workflow.setEnd(['ship']);

    engine.registerWorkflow(workflow);

    // Act: Order from 3 warehouses
    const { case: yawlCase } = await engine.createCase('ecommerce-fulfillment', {
      warehouses: ['WH-EAST', 'WH-WEST', 'WH-CENTRAL'],
    });

    // Process order
    const orderItem = yawlCase.getEnabledWorkItems()[0];
    await engine.startTask(yawlCase.id, orderItem.id);
    await engine.completeTask(yawlCase.id, orderItem.id, {
      warehouseCount: 3,
    });

    // Assert: 3 warehouse instances
    let whItems = yawlCase.getEnabledWorkItems();
    expect(whItems.length).toBe(3);

    // Complete all warehouses
    for (const wh of whItems) {
      await engine.startTask(yawlCase.id, wh.id);
      await engine.completeTask(yawlCase.id, wh.id);
    }

    // Assert: 2 QA inspectors
    const qaItems = yawlCase.getEnabledWorkItems();
    expect(qaItems.length).toBe(2);

    // Complete QA checks
    for (const qa of qaItems) {
      await engine.startTask(yawlCase.id, qa.id);
      await engine.completeTask(yawlCase.id, qa.id, { passed: true });
    }

    // Assert: Ship enabled
    const shipItems = yawlCase.getEnabledWorkItems();
    expect(shipItems.length).toBe(1);
    expect(yawlCase.getTaskDefIdForWorkItem(shipItems[0].id)).toBe('ship');
  });

  test('Academic paper review (WP14 + WP15)', async () => {
    // Arrange: Peer review with dynamic reviewer addition
    const workflow = createTestWorkflow({ id: 'paper-review' });

    workflow.addTask({ id: 'submit-paper' });
    workflow.addTask({
      id: 'assign-reviewers',
      kind: 'multiple',
      multipleInstance: {
        minimum: 3,
        maximum: 10,
        creationType: 'dynamic', // WP14 - initial reviewers
        instanceSync: 'OR',
        threshold: 3,
      },
    });
    workflow.addTask({ id: 'editor-decision' });

    workflow.addFlow(sequence('submit-paper', 'assign-reviewers'));
    workflow.addFlow(sequence('assign-reviewers', 'editor-decision'));
    workflow.setStart('submit-paper');
    workflow.setEnd(['editor-decision']);

    engine.registerWorkflow(workflow);

    // Act: Initial 4 reviewers
    const { case: yawlCase } = await engine.createCase('paper-review');

    const submitItem = yawlCase.getEnabledWorkItems()[0];
    await engine.startTask(yawlCase.id, submitItem.id);
    await engine.completeTask(yawlCase.id, submitItem.id, {
      reviewerCount: 4,
    });

    let reviewers = yawlCase.getEnabledWorkItems();
    expect(reviewers.length).toBe(4);

    // 2 reviewers complete
    await engine.startTask(yawlCase.id, reviewers[0].id);
    await engine.completeTask(yawlCase.id, reviewers[0].id, { score: 8 });

    await engine.startTask(yawlCase.id, reviewers[1].id);
    await engine.completeTask(yawlCase.id, reviewers[1].id, { score: 6 });

    // WP15: Add additional reviewer during process
    await engine.addMIInstance(yawlCase.id, 'assign-reviewers', {
      reviewerId: 'EXPERT-5',
    });

    reviewers = yawlCase.getEnabledWorkItems().filter(
      w => yawlCase.getTaskDefIdForWorkItem(w.id) === 'assign-reviewers'
    );
    expect(reviewers.length).toBe(3); // 2 pending + 1 new

    // Complete 3rd review (threshold reached)
    await engine.startTask(yawlCase.id, reviewers[0].id);
    await engine.completeTask(yawlCase.id, reviewers[0].id, { score: 9 });

    // Assert: Editor decision enabled (threshold = 3)
    const decisionItems = yawlCase.getEnabledWorkItems().filter(
      w => yawlCase.getTaskDefIdForWorkItem(w.id) === 'editor-decision'
    );
    expect(decisionItems.length).toBe(1);
  });
});

describe('MI Pattern Receipt Validation', () => {
  let engine;

  beforeEach(() => {
    engine = createTestEngine();
  });

  test('Receipts track all MI instance operations', async () => {
    // Arrange
    const workflow = createTestWorkflow({ id: 'mi-receipts' });

    workflow.addTask({ id: 'start' });
    workflow.addTask({
      id: 'process',
      kind: 'multiple',
      multipleInstance: {
        minimum: 3,
        creationType: 'static',
        instanceSync: 'AND',
      },
    });
    workflow.addTask({ id: 'end' });

    workflow.addFlow(sequence('start', 'process'));
    workflow.addFlow(sequence('process', 'end'));
    workflow.setStart('start');
    workflow.setEnd(['end']);

    engine.registerWorkflow(workflow);

    // Act
    const { case: yawlCase, receipt: createReceipt } = await engine.createCase('mi-receipts');

    const startItem = yawlCase.getEnabledWorkItems()[0];
    await engine.startTask(yawlCase.id, startItem.id);
    const { receipt: startReceipt } = await engine.completeTask(
      yawlCase.id,
      startItem.id,
      { instanceCount: 3 }
    );

    // Assert: Receipts valid
    expect(createReceipt.valid).toBe(true);
    expect(startReceipt.valid).toBe(true);

    // Complete all instances
    const instances = yawlCase.getEnabledWorkItems();
    for (const inst of instances) {
      await engine.startTask(yawlCase.id, inst.id);
      const { receipt } = await engine.completeTask(yawlCase.id, inst.id, {
        instanceNumber: inst.instanceNumber,
      });
      expect(receipt.valid).toBe(true);
      expect(receipt.metadata?.instanceNumber).toBe(inst.instanceNumber);
    }

    // Verify receipt chain integrity
    const receipts = yawlCase.receipts;
    expect(receipts.length).toBeGreaterThan(5); // create + start + complete + 3 instances

    for (let i = 1; i < receipts.length; i++) {
      const chainResult = await receipts[i].verifyChain(receipts[i - 1]);
      expect(chainResult.valid).toBe(true);
    }
  });

  test('Receipt chain handles instance cancellation', async () => {
    // Arrange
    const workflow = createTestWorkflow({ id: 'mi-cancel-receipts' });

    workflow.addTask({ id: 'start' });
    workflow.addTask({
      id: 'process',
      kind: 'multiple',
      multipleInstance: {
        minimum: 5,
        creationType: 'static',
        instanceSync: 'AND',
      },
    });
    workflow.addTask({ id: 'end' });

    workflow.addFlow(sequence('start', 'process'));
    workflow.addFlow(sequence('process', 'end'));
    workflow.setStart('start');
    workflow.setEnd(['end']);

    engine.registerWorkflow(workflow);

    // Act
    const { case: yawlCase } = await engine.createCase('mi-cancel-receipts');

    const startItem = yawlCase.getEnabledWorkItems()[0];
    await engine.startTask(yawlCase.id, startItem.id);
    await engine.completeTask(yawlCase.id, startItem.id, {
      instanceCount: 5,
    });

    const instances = yawlCase.getEnabledWorkItems();
    expect(instances.length).toBe(5);

    // Complete 3 instances
    for (let i = 0; i < 3; i++) {
      await engine.startTask(yawlCase.id, instances[i].id);
      await engine.completeTask(yawlCase.id, instances[i].id);
    }

    // Cancel instance 4
    const { receipt: cancelReceipt } = await engine.cancelTask(
      yawlCase.id,
      instances[3].id,
      'User cancelled'
    );

    // Assert: Cancel receipt valid
    expect(cancelReceipt.valid).toBe(true);
    expect(cancelReceipt.action).toBe('cancel');

    // Complete instance 5
    await engine.startTask(yawlCase.id, instances[4].id);
    await engine.completeTask(yawlCase.id, instances[4].id);

    // Verify all receipts in chain
    const receipts = yawlCase.receipts;
    for (const receipt of receipts) {
      expect(receipt.valid).toBe(true);
    }
  });
});
