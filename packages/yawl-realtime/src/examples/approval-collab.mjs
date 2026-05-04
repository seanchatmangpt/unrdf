/**
 * @file Multi-User Approval Workflow - Real-time collaboration example
 * @module @unrdf/yawl-realtime/examples/approval-collab
 *
 * @description
 * Demonstrates real-time collaboration with 3 simulated users:
 * - Alice: Submitter
 * - Bob: First approver
 * - Carol: Second approver
 *
 * They collaboratively execute a multi-level approval workflow.
 */

import { createWorkflowEngine, YawlWorkflow } from '@unrdf/yawl';
import { YAWLRealtimeServer } from '../server.mjs';
import { YAWLRealtimeClient } from '../client.mjs';

// =============================================================================
// Workflow Definition
// =============================================================================

/**
 * Create a multi-level approval workflow
 * @returns {YawlWorkflow}
 */
function createApprovalWorkflow() {
  const workflow = new YawlWorkflow({
    id: 'multi-approval',
    name: 'Multi-Level Approval Workflow',
    version: '1.0.0',
  });

  // Tasks
  workflow.addTask({
    id: 'submit',
    name: 'Submit Request',
    role: 'submitter',
  });

  workflow.addTask({
    id: 'level1-approve',
    name: 'Level 1 Approval',
    role: 'approver-l1',
    split: 'XOR', // Either approve or reject
  });

  workflow.addTask({
    id: 'level2-approve',
    name: 'Level 2 Approval',
    role: 'approver-l2',
    split: 'XOR',
  });

  workflow.addTask({
    id: 'approved',
    name: 'Request Approved',
  });

  workflow.addTask({
    id: 'rejected',
    name: 'Request Rejected',
  });

  // Flow
  workflow.setStart('submit');
  workflow.addFlow('submit', 'level1-approve');
  workflow.addFlow('level1-approve', 'level2-approve', { condition: 'approved' });
  workflow.addFlow('level1-approve', 'rejected', { condition: 'rejected' });
  workflow.addFlow('level2-approve', 'approved', { condition: 'approved' });
  workflow.addFlow('level2-approve', 'rejected', { condition: 'rejected' });
  workflow.setEnd(['approved', 'rejected']);

  return workflow;
}

// =============================================================================
// Simulated User Actions
// =============================================================================

/**
 * Simulate Alice (submitter) actions
 * @param {YAWLRealtimeClient} client
 * @param {string} caseId
 */
async function simulateAlice(client, caseId) {
  console.log('\n[Alice] Starting submission...');

  // Listen for events
  client.on('yawl:event', (event) => {
    console.log(`[Alice] Received event: ${event.type}`);
  });

  // Wait a bit for case creation
  await sleep(500);

  // Find submit task
  const state = await client.syncState(caseId);
  console.log(`[Alice] Synced state, ${Object.keys(state.state?.workItems || {}).length} work items`);

  // Try to claim submit task
  const workItems = Object.values(state.state?.workItems || {});
  const submitTask = workItems.find(wi => wi.status === 'enabled' || wi.status === 'running');

  if (submitTask) {
    console.log(`[Alice] Found submit task: ${submitTask.id}`);

    const claimResult = await client.claimTask(caseId, submitTask.id);

    if (claimResult.success) {
      console.log('[Alice] Successfully claimed submit task');

      // Complete submission
      await sleep(1000);
      const completeResult = await client.completeTask(caseId, submitTask.id, {
        requestType: 'budget-increase',
        amount: 50000,
        justification: 'Need additional budget for Q4 marketing campaign',
        submittedBy: 'alice@example.com',
      });

      if (completeResult.success) {
        console.log('[Alice] Request submitted successfully!');
        console.log(`[Alice] Downstream tasks enabled: ${completeResult.downstreamEnabled?.length || 0}`);
      }
    } else {
      console.log(`[Alice] Failed to claim task: ${claimResult.conflict?.resolution}`);
    }
  } else {
    console.log('[Alice] No submit task found');
  }
}

/**
 * Simulate Bob (Level 1 approver) actions
 * @param {YAWLRealtimeClient} client
 * @param {string} caseId
 */
async function simulateBob(client, caseId) {
  console.log('\n[Bob] Waiting for Level 1 approval task...');

  let level1TaskId = null;

  // Listen for task enabled events
  const unsubscribe = client.on('task:enabled', async (event) => {
    console.log(`[Bob] Task enabled: ${event.taskId}`);

    if (event.taskId === 'level1-approve' && !level1TaskId) {
      level1TaskId = event.workItemId;
      console.log(`[Bob] Level 1 approval task available: ${level1TaskId}`);

      // Wait a bit before claiming
      await sleep(800);

      try {
        const claimResult = await client.claimTask(caseId, level1TaskId);

        if (claimResult.success) {
          console.log('[Bob] Successfully claimed Level 1 approval task');

          // Review and approve
          await sleep(1500);
          const completeResult = await client.completeTask(caseId, level1TaskId, {
            decision: 'approved',
            comments: 'Budget increase is reasonable for Q4 campaign',
            approvedBy: 'bob@example.com',
            approvedAt: new Date().toISOString(),
          });

          if (completeResult.success) {
            console.log('[Bob] Level 1 approval completed!');
          }
        } else {
          console.log(`[Bob] Failed to claim task: ${claimResult.conflict?.resolution}`);
        }
      } catch (error) {
        console.error('[Bob] Error:', error.message);
      }
    }
  });

  return unsubscribe;
}

/**
 * Simulate Carol (Level 2 approver) actions
 * @param {YAWLRealtimeClient} client
 * @param {string} caseId
 */
async function simulateCarol(client, caseId) {
  console.log('\n[Carol] Waiting for Level 2 approval task...');

  let level2TaskId = null;

  // Listen for task enabled events
  const unsubscribe = client.on('task:enabled', async (event) => {
    console.log(`[Carol] Task enabled: ${event.taskId}`);

    if (event.taskId === 'level2-approve' && !level2TaskId) {
      level2TaskId = event.workItemId;
      console.log(`[Carol] Level 2 approval task available: ${level2TaskId}`);

      // Wait a bit before claiming
      await sleep(1200);

      try {
        const claimResult = await client.claimTask(caseId, level2TaskId);

        if (claimResult.success) {
          console.log('[Carol] Successfully claimed Level 2 approval task');

          // Review and approve
          await sleep(2000);
          const completeResult = await client.completeTask(caseId, level2TaskId, {
            decision: 'approved',
            comments: 'Final approval granted. Q4 budget increase authorized.',
            approvedBy: 'carol@example.com',
            approvedAt: new Date().toISOString(),
          });

          if (completeResult.success) {
            console.log('[Carol] Level 2 approval completed! Workflow should complete.');
          }
        } else {
          console.log(`[Carol] Failed to claim task: ${claimResult.conflict?.resolution}`);
        }
      } catch (error) {
        console.error('[Carol] Error:', error.message);
      }
    }
  });

  return unsubscribe;
}

// =============================================================================
// Main Example
// =============================================================================

/**
 * Sleep helper
 * @param {number} ms - Milliseconds to sleep
 */
function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

/**
 * Run the multi-user approval workflow example
 */
async function main() {
  console.log('='.repeat(80));
  console.log('Multi-User Approval Workflow - Real-time Collaboration Example');
  console.log('='.repeat(80));

  // Create engine and workflow
  const engine = createWorkflowEngine({ enableEventLog: false });
  const workflow = createApprovalWorkflow();
  engine.registerWorkflow(workflow);

  console.log('\n[Server] Starting YAWL Realtime Server...');
  const server = new YAWLRealtimeServer(engine, { port: 3000 });
  await server.start();

  // Wait for server to fully initialize
  await sleep(500);

  // Create case
  console.log('\n[Server] Creating approval case...');
  const { case: approvalCase } = await engine.createCase('multi-approval', {
    department: 'Marketing',
    fiscalYear: 2024,
  });
  const caseId = approvalCase.id;
  console.log(`[Server] Case created: ${caseId}`);

  // Create clients for three users
  console.log('\n[Server] Connecting users...');

  const alice = new YAWLRealtimeClient({
    serverUrl: 'http://localhost:3000',
    userId: 'alice@example.com',
  });

  const bob = new YAWLRealtimeClient({
    serverUrl: 'http://localhost:3000',
    userId: 'bob@example.com',
  });

  const carol = new YAWLRealtimeClient({
    serverUrl: 'http://localhost:3000',
    userId: 'carol@example.com',
  });

  await Promise.all([
    alice.connect(),
    bob.connect(),
    carol.connect(),
  ]);

  console.log('\n[Server] All users connected!');

  // Setup workflows for each user
  const bobUnsubscribe = await simulateBob(bob, caseId);
  const carolUnsubscribe = await simulateCarol(carol, caseId);

  // Alice starts the workflow
  await simulateAlice(alice, caseId);

  // Wait for workflow to complete
  console.log('\n[Server] Waiting for workflow to complete...');
  await sleep(8000);

  // Get final state
  console.log('\n' + '='.repeat(80));
  console.log('Workflow Completed!');
  console.log('='.repeat(80));

  const finalCase = engine.cases.get(caseId);
  console.log(`\nFinal case status: ${finalCase.status}`);
  console.log(`Total work items: ${finalCase.workItems.size}`);
  console.log(`Events logged: ${engine.events.length}`);

  // Display case data
  console.log('\nCase data:');
  console.log(JSON.stringify(finalCase.data, null, 2));

  // Server stats
  console.log('\nServer statistics:');
  console.log(JSON.stringify(server.getStats(), null, 2));

  // Cleanup
  console.log('\n[Server] Cleaning up...');
  bobUnsubscribe();
  carolUnsubscribe();
  await alice.disconnect();
  await bob.disconnect();
  await carol.disconnect();
  await server.stop();

  console.log('\n[Server] Example completed!');
  process.exit(0);
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch(error => {
    console.error('Error:', error);
    process.exit(1);
  });
}

export { main, createApprovalWorkflow, simulateAlice, simulateBob, simulateCarol };
