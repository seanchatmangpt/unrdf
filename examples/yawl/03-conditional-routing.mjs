/**
 * Conditional Routing Workflow Example
 *
 * This example demonstrates:
 * - XOR-split: Choose ONE path based on condition
 * - XOR-join: Merge paths back together
 * - Conditional branching based on case data
 * - Simple merge after exclusive choice
 *
 * Workflow Pattern: WP-4 (Exclusive Choice) + WP-5 (Simple Merge)
 *
 * Flow:
 *                → Auto Approve (if amount < $1000) →
 *   Check Amount                                       Done
 *                → Manual Review (if amount ≥ $1000) →
 *
 * Run: node examples/yawl/03-conditional-routing.mjs
 */

import { createWorkflow, createCase, enableTask, startTask, completeTask } from '@unrdf/yawl';
import { createStore } from '@unrdf/oxigraph';

async function runCase(store, workflowId, caseId, amount) {
  console.log(`\n${'='.repeat(80)}`);
  console.log(`Case: ${caseId} - Amount: $${amount}`);
  console.log('='.repeat(80));

  // Start case
  const caseReceipt = await createCase(store, {
    workflowId,
    caseId,
    initialData: { amount, requestedBy: 'alice@example.com' }
  });
  console.log(`✅ Case started: ${caseReceipt.case_id}\n`);

  // Execute "check" task
  console.log('Step 1: Checking amount...');
  const checkEnable = await enableTask(store, { caseId, taskId: 'check' });
  const checkStart = await startTask(store, { caseId, workItemId: checkEnable.work_item_id, actor: 'system@example.com' });
  const checkComplete = await completeTask(store, {
    caseId,
    workItemId: checkStart.work_item_id,
    outputData: {
      amount,
      requiresManualReview: amount >= 1000,
      checkedAt: new Date().toISOString()
    }
  });

  console.log(`   ✅ Amount checked: $${amount}`);
  console.log(`      XOR-split evaluated: ${checkComplete.enabled_tasks.join(', ')}`);

  const nextTask = checkComplete.enabled_tasks[0];
  console.log(`      → Routing to: ${nextTask}\n`);

  // Execute the chosen path
  if (nextTask === 'auto-approve') {
    console.log('Step 2: Auto-approving (amount < $1000)...');
    const approveEnable = await enableTask(store, { caseId, taskId: 'auto-approve' });
    const approveStart = await startTask(store, { caseId, workItemId: approveEnable.work_item_id, actor: 'system@example.com' });
    const approveComplete = await completeTask(store, {
      caseId,
      workItemId: approveStart.work_item_id,
      outputData: {
        approved: true,
        approvalType: 'automatic',
        approvedBy: 'system@example.com'
      }
    });
    console.log(`   ✅ Auto-approved by system`);
    console.log(`      Next: ${approveComplete.enabled_tasks.join(', ')}\n`);

  } else if (nextTask === 'manual-review') {
    console.log('Step 2: Manual review required (amount ≥ $1000)...');
    const reviewEnable = await enableTask(store, { caseId, taskId: 'manual-review' });
    const reviewStart = await startTask(store, { caseId, workItemId: reviewEnable.work_item_id, actor: 'manager@example.com' });
    const reviewComplete = await completeTask(store, {
      caseId,
      workItemId: reviewStart.work_item_id,
      outputData: {
        approved: true,
        approvalType: 'manual',
        approvedBy: 'manager@example.com',
        comments: 'Approved after review'
      }
    });
    console.log(`   ✅ Manually approved by ${reviewStart.actor}`);
    console.log(`      Next: ${reviewComplete.enabled_tasks.join(', ')}\n`);
  }

  // Complete "done" task (XOR-join)
  console.log('Step 3: Completing workflow...');
  const doneEnable = await enableTask(store, { caseId, taskId: 'done' });
  const doneStart = await startTask(store, { caseId, workItemId: doneEnable.work_item_id, actor: 'system@example.com' });
  const doneComplete = await completeTask(store, {
    caseId,
    workItemId: doneStart.work_item_id,
    outputData: { completedAt: new Date().toISOString() }
  });
  console.log(`   ✅ Workflow completed!\n`);

  return { caseId, amount, route: nextTask };
}

async function main() {
  console.log('='.repeat(80));
  console.log('YAWL Example 3: Conditional Routing (XOR-split/XOR-join)');
  console.log('='.repeat(80));
  console.log();

  // Step 1: Create store
  console.log('Step 1: Creating RDF store...');
  const store = createStore();
  console.log('✅ Store created\n');

  // Step 2: Create workflow with conditional routing
  console.log('Step 2: Creating conditional workflow...');
  const workflowSpec = {
    id: 'expense-approval',
    name: 'Expense Approval with Conditional Routing',
    description: 'Auto-approve small amounts, manual review for large amounts',
    tasks: [
      { id: 'check', name: 'Check Amount', kind: 'atomic' },
      { id: 'auto-approve', name: 'Auto Approve', kind: 'atomic' },
      { id: 'manual-review', name: 'Manual Review', kind: 'atomic' },
      { id: 'done', name: 'Done', kind: 'atomic' }
    ],
    flow: [
      // XOR-split: Choose ONE path based on condition
      {
        from: 'check',
        to: 'auto-approve',
        splitType: 'XOR',
        condition: { amount_lt: 1000 },
        isDefaultFlow: false
      },
      {
        from: 'check',
        to: 'manual-review',
        splitType: 'XOR',
        condition: { amount_gte: 1000 },
        isDefaultFlow: true
      },

      // XOR-join: Merge paths (simple merge)
      { from: 'auto-approve', to: 'done', joinType: 'XOR' },
      { from: 'manual-review', to: 'done', joinType: 'XOR' }
    ]
  };

  const workflowReceipt = await createWorkflow(store, workflowSpec);
  console.log(`✅ Workflow created: ${workflowReceipt.workflow_id}\n`);

  // Step 3: Run multiple cases with different amounts
  console.log('Step 3: Running cases with different amounts...');

  const results = [];

  // Case 1: Small amount (auto-approve)
  results.push(await runCase(store, workflowReceipt.workflow_id, 'case-small-001', 500));

  // Case 2: Large amount (manual review)
  results.push(await runCase(store, workflowReceipt.workflow_id, 'case-large-001', 2500));

  // Case 3: Exactly at threshold (manual review)
  results.push(await runCase(store, workflowReceipt.workflow_id, 'case-threshold-001', 1000));

  // Summary
  console.log('\n' + '='.repeat(80));
  console.log('Conditional Routing Summary:');
  console.log('='.repeat(80));
  console.log(`Workflow ID: ${workflowReceipt.workflow_id}`);
  console.log(`Pattern used: XOR-split (WP-4) + XOR-join (WP-5)`);
  console.log(`Cases executed: ${results.length}`);
  console.log();
  console.log('Results:');
  results.forEach(result => {
    const route = result.route === 'auto-approve' ? 'Auto-approved' : 'Manual review';
    console.log(`  - ${result.caseId}: $${result.amount.toLocaleString()} → ${route}`);
  });
  console.log();
  console.log('Key Insights:');
  console.log('  - XOR-split chooses exactly ONE outgoing path');
  console.log('  - Condition evaluated based on case data (amount)');
  console.log('  - XOR-join merges paths without synchronization');
  console.log('  - Same workflow handles different routing logic dynamically');
  console.log();
  console.log('✅ Conditional routing workflow completed successfully!');
  console.log();
}

main().catch(error => {
  console.error('Error running example:', error);
  process.exit(1);
});
