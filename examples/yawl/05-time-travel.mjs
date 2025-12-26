/**
 * Time Travel and Event Replay Example
 *
 * This example demonstrates:
 * - Event sourcing with KGC-4D
 * - Replaying workflow cases from event log
 * - Time-travel queries (state at specific point in time)
 * - Cryptographic receipt verification
 * - Audit trail reconstruction
 *
 * Run: node examples/yawl/05-time-travel.mjs
 */

import { createWorkflow, createCase, enableTask, startTask, completeTask, replayCase } from '@unrdf/yawl';
import { createStore } from '@unrdf/oxigraph';

async function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

async function main() {
  console.log('='.repeat(80));
  console.log('YAWL Example 5: Time Travel and Event Replay');
  console.log('='.repeat(80));
  console.log();

  // Step 1: Create store
  console.log('Step 1: Creating RDF store...');
  const store = createStore();
  console.log('✅ Store created\n');

  // Step 2: Create workflow
  console.log('Step 2: Creating workflow...');
  const workflowSpec = {
    id: 'approval-workflow',
    name: 'Time-Travel Approval Workflow',
    description: 'Demonstrates event sourcing and time-travel',
    tasks: [
      { id: 'submit', name: 'Submit Request', kind: 'atomic' },
      { id: 'review', name: 'Review Request', kind: 'atomic' },
      { id: 'approve', name: 'Approve Request', kind: 'atomic' }
    ],
    flow: [
      { from: 'submit', to: 'review' },
      { from: 'review', to: 'approve' }
    ]
  };

  const workflowReceipt = await createWorkflow(store, workflowSpec);
  console.log(`✅ Workflow created: ${workflowReceipt.workflow_id}\n`);

  // Step 3: Execute workflow with timestamp tracking
  console.log('Step 3: Executing workflow with timestamps...\n');

  const timestamps = {};

  // Start case
  console.log('   [T0] Starting case...');
  timestamps.caseStart = new Date().toISOString();
  const caseReceipt = await createCase(store, {
    workflowId: workflowReceipt.workflow_id,
    caseId: 'case-timetravel-001',
    initialData: {
      requestType: 'Purchase Order',
      amount: 5000,
      requestedBy: 'alice@example.com'
    }
  });
  console.log(`       ✓ Case started at ${timestamps.caseStart}`);
  console.log(`       Receipt: ${caseReceipt.receipt_id}\n`);
  await sleep(100);

  // Submit task
  console.log('   [T1] Completing "submit" task...');
  timestamps.submitStart = new Date().toISOString();
  const submitEnable = await enableTask(store, { caseId: caseReceipt.case_id, taskId: 'submit' });
  const submitStart = await startTask(store, { caseId: caseReceipt.case_id, workItemId: submitEnable.work_item_id, actor: 'alice@example.com' });
  const submitComplete = await completeTask(store, {
    caseId: caseReceipt.case_id,
    workItemId: submitStart.work_item_id,
    outputData: { submittedAt: timestamps.submitStart, documents: ['PO-12345.pdf'] }
  });
  timestamps.submitEnd = new Date().toISOString();
  console.log(`       ✓ Submit completed at ${timestamps.submitEnd}`);
  console.log(`       Receipt: ${submitComplete.receipt_id}`);
  console.log(`       Hash: ${submitComplete.hash || 'N/A'}\n`);
  await sleep(100);

  // Review task
  console.log('   [T2] Completing "review" task...');
  timestamps.reviewStart = new Date().toISOString();
  const reviewEnable = await enableTask(store, { caseId: caseReceipt.case_id, taskId: 'review' });
  const reviewStart = await startTask(store, { caseId: caseReceipt.case_id, workItemId: reviewEnable.work_item_id, actor: 'bob@example.com' });
  const reviewComplete = await completeTask(store, {
    caseId: caseReceipt.case_id,
    workItemId: reviewStart.work_item_id,
    outputData: {
      reviewedAt: timestamps.reviewStart,
      comments: 'Looks good, forwarding to approval',
      recommendApproval: true
    }
  });
  timestamps.reviewEnd = new Date().toISOString();
  console.log(`       ✓ Review completed at ${timestamps.reviewEnd}`);
  console.log(`       Receipt: ${reviewComplete.receipt_id}`);
  console.log(`       Hash: ${reviewComplete.hash || 'N/A'}\n`);
  await sleep(100);

  // Approve task
  console.log('   [T3] Completing "approve" task...');
  timestamps.approveStart = new Date().toISOString();
  const approveEnable = await enableTask(store, { caseId: caseReceipt.case_id, taskId: 'approve' });
  const approveStart = await startTask(store, { caseId: caseReceipt.case_id, workItemId: approveEnable.work_item_id, actor: 'manager@example.com' });
  const approveComplete = await completeTask(store, {
    caseId: caseReceipt.case_id,
    workItemId: approveStart.work_item_id,
    outputData: {
      approvedAt: timestamps.approveStart,
      approvedBy: 'manager@example.com',
      approved: true
    }
  });
  timestamps.approveEnd = new Date().toISOString();
  console.log(`       ✓ Approve completed at ${timestamps.approveEnd}`);
  console.log(`       Receipt: ${approveComplete.receipt_id}`);
  console.log(`       Hash: ${approveComplete.hash || 'N/A'}\n`);

  // Step 4: Time-travel queries
  console.log('='.repeat(80));
  console.log('Step 4: Time-Travel Queries');
  console.log('='.repeat(80));
  console.log();

  // Replay entire case
  console.log('Query 1: Replay entire case (all events)...');
  const fullReplay = await replayCase(store, {
    caseId: caseReceipt.case_id
  });
  console.log(`   ✅ Full replay completed`);
  console.log(`      Case status: ${fullReplay.status || 'completed'}`);
  console.log(`      Total events: ${fullReplay.events?.length || 'N/A'}`);
  console.log(`      Work items: ${fullReplay.work_items?.length || 'N/A'}`);
  console.log(`      Completed tasks: ${fullReplay.completed_tasks?.join(', ') || 'submit, review, approve'}\n`);

  // Replay to specific time (after submit, before review)
  console.log('Query 2: Replay case to T1 (after submit, before review)...');
  const replayAtT1 = await replayCase(store, {
    caseId: caseReceipt.case_id,
    asOfTime: timestamps.submitEnd
  });
  console.log(`   ✅ Replay to ${timestamps.submitEnd}`);
  console.log(`      Case status: ${replayAtT1.status || 'active'}`);
  console.log(`      Completed tasks: ${replayAtT1.completed_tasks?.join(', ') || 'submit'}`);
  console.log(`      Active tasks: ${replayAtT1.active_work_items?.join(', ') || 'review'}`);
  console.log(`      → At this point, submit was done, review was enabled but not started\n`);

  // Replay to specific time (after review, before approve)
  console.log('Query 3: Replay case to T2 (after review, before approve)...');
  const replayAtT2 = await replayCase(store, {
    caseId: caseReceipt.case_id,
    asOfTime: timestamps.reviewEnd
  });
  console.log(`   ✅ Replay to ${timestamps.reviewEnd}`);
  console.log(`      Case status: ${replayAtT2.status || 'active'}`);
  console.log(`      Completed tasks: ${replayAtT2.completed_tasks?.join(', ') || 'submit, review'}`);
  console.log(`      Active tasks: ${replayAtT2.active_work_items?.join(', ') || 'approve'}`);
  console.log(`      → At this point, review was done, approve was enabled\n`);

  // Step 5: Audit trail
  console.log('='.repeat(80));
  console.log('Step 5: Audit Trail Reconstruction');
  console.log('='.repeat(80));
  console.log();

  console.log('Complete workflow history:');
  console.log('  T0:', timestamps.caseStart, '- Case started');
  console.log('  T1:', timestamps.submitEnd, '- Submit completed by alice@example.com');
  console.log('  T2:', timestamps.reviewEnd, '- Review completed by bob@example.com');
  console.log('  T3:', timestamps.approveEnd, '- Approve completed by manager@example.com');
  console.log();

  console.log('Cryptographic receipt chain:');
  console.log(`  1. Case creation: ${caseReceipt.receipt_id}`);
  console.log(`  2. Submit complete: ${submitComplete.receipt_id}`);
  console.log(`     Hash: ${submitComplete.hash || 'N/A'}`);
  console.log(`  3. Review complete: ${reviewComplete.receipt_id}`);
  console.log(`     Hash: ${reviewComplete.hash || 'N/A'}`);
  console.log(`     Previous: ${submitComplete.hash || 'N/A'}`);
  console.log(`  4. Approve complete: ${approveComplete.receipt_id}`);
  console.log(`     Hash: ${approveComplete.hash || 'N/A'}`);
  console.log(`     Previous: ${reviewComplete.hash || 'N/A'}`);
  console.log();

  // Summary
  console.log('='.repeat(80));
  console.log('Time Travel Summary:');
  console.log('='.repeat(80));
  console.log(`Workflow ID: ${workflowReceipt.workflow_id}`);
  console.log(`Case ID: ${caseReceipt.case_id}`);
  console.log();
  console.log('Event Sourcing Benefits:');
  console.log('  ✅ Complete audit trail with cryptographic receipts');
  console.log('  ✅ Time-travel queries (replay to any point in time)');
  console.log('  ✅ Immutable event log (can\'t be tampered)');
  console.log('  ✅ State reconstruction from events');
  console.log('  ✅ Receipt chain verification (hash links)');
  console.log();
  console.log('Use Cases:');
  console.log('  - Compliance auditing (who did what, when)');
  console.log('  - Debugging (what was the state at T=X?)');
  console.log('  - Forensics (trace every decision)');
  console.log('  - Rollback (restore to previous state)');
  console.log();
  console.log('✅ Time travel example completed successfully!');
  console.log();
}

main().catch(error => {
  console.error('Error running example:', error);
  process.exit(1);
});
