/**
 * Parallel Approval Workflow Example
 *
 * This example demonstrates:
 * - AND-split: One task enables multiple parallel tasks
 * - AND-join: Wait for all parallel tasks to complete
 * - Parallel execution of independent tasks
 * - Synchronization at join point
 *
 * Workflow Pattern: WP-2 (Parallel Split) + WP-3 (Synchronization)
 *
 * Flow:
 *                    → Legal Review   →
 *   Submit Document → Tech Review    → Finalize
 *                    → Finance Review →
 *
 * Run: node examples/yawl/02-parallel-approval.mjs
 */

import { createWorkflow, createCase, enableTask, startTask, completeTask } from '@unrdf/yawl';
import { createStore } from '@unrdf/oxigraph';

async function main() {
  console.log('='.repeat(80));
  console.log('YAWL Example 2: Parallel Approval Workflow (AND-split/AND-join)');
  console.log('='.repeat(80));
  console.log();

  // Step 1: Create store
  console.log('Step 1: Creating RDF store...');
  const store = createStore();
  console.log('✅ Store created\n');

  // Step 2: Create workflow with parallel review tasks
  console.log('Step 2: Creating parallel approval workflow...');
  const workflowSpec = {
    id: 'parallel-approval',
    name: 'Parallel Multi-Reviewer Approval',
    description: 'Document reviewed by 3 departments in parallel',
    tasks: [
      { id: 'submit', name: 'Submit Document', kind: 'atomic' },
      { id: 'legal-review', name: 'Legal Review', kind: 'atomic' },
      { id: 'tech-review', name: 'Technical Review', kind: 'atomic' },
      { id: 'finance-review', name: 'Finance Review', kind: 'atomic' },
      { id: 'finalize', name: 'Finalize Document', kind: 'atomic' }
    ],
    flow: [
      // AND-split: Submit enables all 3 reviews in parallel
      { from: 'submit', to: 'legal-review', splitType: 'AND' },
      { from: 'submit', to: 'tech-review', splitType: 'AND' },
      { from: 'submit', to: 'finance-review', splitType: 'AND' },

      // AND-join: Finalize waits for ALL reviews to complete
      { from: 'legal-review', to: 'finalize', joinType: 'AND' },
      { from: 'tech-review', to: 'finalize', joinType: 'AND' },
      { from: 'finance-review', to: 'finalize', joinType: 'AND' }
    ]
  };

  const workflowReceipt = await createWorkflow(store, workflowSpec);
  console.log(`✅ Workflow created: ${workflowReceipt.workflow_id}\n`);

  // Step 3: Start case
  console.log('Step 3: Starting case...');
  const caseReceipt = await createCase(store, {
    workflowId: workflowReceipt.workflow_id,
    caseId: 'case-parallel-001',
    initialData: {
      documentTitle: 'Enterprise Software License Agreement',
      submittedBy: 'alice@example.com',
      amount: 50000
    }
  });
  console.log(`✅ Case started: ${caseReceipt.case_id}\n`);

  // Step 4: Complete "submit" task to trigger parallel split
  console.log('Step 4: Completing "submit" task...');
  const submitEnable = await enableTask(store, { caseId: caseReceipt.case_id, taskId: 'submit' });
  const submitStart = await startTask(store, { caseId: caseReceipt.case_id, workItemId: submitEnable.work_item_id, actor: 'alice@example.com' });
  const submitComplete = await completeTask(store, {
    caseId: caseReceipt.case_id,
    workItemId: submitStart.work_item_id,
    outputData: { documentUrl: 'https://example.com/docs/license-agreement.pdf' }
  });

  console.log(`   ✅ Submit completed!`);
  console.log(`      AND-split activated: ${submitComplete.enabled_tasks.length} parallel tasks enabled`);
  console.log(`      Enabled tasks: ${submitComplete.enabled_tasks.join(', ')}\n`);

  // Step 5: Execute parallel reviews (can happen in any order, even simultaneously)
  console.log('Step 5: Executing parallel reviews...');
  console.log('   (In production, these would be done by different people concurrently)');
  console.log();

  // Legal review
  console.log('   → Legal Review:');
  const legalEnable = await enableTask(store, { caseId: caseReceipt.case_id, taskId: 'legal-review' });
  const legalStart = await startTask(store, { caseId: caseReceipt.case_id, workItemId: legalEnable.work_item_id, actor: 'legal@example.com' });
  const legalComplete = await completeTask(store, {
    caseId: caseReceipt.case_id,
    workItemId: legalStart.work_item_id,
    outputData: { approved: true, comments: 'Legal terms acceptable' }
  });
  console.log(`     ✅ Legal review completed by ${legalStart.actor}`);
  console.log(`        Enabled next tasks: ${legalComplete.enabled_tasks.join(', ') || 'none (waiting for other reviews)'}\n`);

  // Technical review
  console.log('   → Technical Review:');
  const techEnable = await enableTask(store, { caseId: caseReceipt.case_id, taskId: 'tech-review' });
  const techStart = await startTask(store, { caseId: caseReceipt.case_id, workItemId: techEnable.work_item_id, actor: 'tech@example.com' });
  const techComplete = await completeTask(store, {
    caseId: caseReceipt.case_id,
    workItemId: techStart.work_item_id,
    outputData: { approved: true, comments: 'Technical specifications meet requirements' }
  });
  console.log(`     ✅ Technical review completed by ${techStart.actor}`);
  console.log(`        Enabled next tasks: ${techComplete.enabled_tasks.join(', ') || 'none (waiting for finance review)'}\n`);

  // Finance review (last one triggers the AND-join)
  console.log('   → Finance Review:');
  const financeEnable = await enableTask(store, { caseId: caseReceipt.case_id, taskId: 'finance-review' });
  const financeStart = await startTask(store, { caseId: caseReceipt.case_id, workItemId: financeEnable.work_item_id, actor: 'finance@example.com' });
  const financeComplete = await completeTask(store, {
    caseId: caseReceipt.case_id,
    workItemId: financeStart.work_item_id,
    outputData: { approved: true, comments: 'Budget approved for $50,000' }
  });
  console.log(`     ✅ Finance review completed by ${financeStart.actor}`);
  console.log(`        AND-join satisfied! Next tasks enabled: ${financeComplete.enabled_tasks.join(', ')}\n`);

  // Step 6: Complete finalize task
  console.log('Step 6: Finalizing document...');
  const finalizeEnable = await enableTask(store, { caseId: caseReceipt.case_id, taskId: 'finalize' });
  const finalizeStart = await startTask(store, { caseId: caseReceipt.case_id, workItemId: finalizeEnable.work_item_id, actor: 'alice@example.com' });
  const finalizeComplete = await completeTask(store, {
    caseId: caseReceipt.case_id,
    workItemId: finalizeStart.work_item_id,
    outputData: {
      finalizedUrl: 'https://example.com/docs/license-agreement-final.pdf',
      approvedBy: ['legal@example.com', 'tech@example.com', 'finance@example.com']
    }
  });
  console.log(`   ✅ Document finalized!\n`);

  // Summary
  console.log('='.repeat(80));
  console.log('Parallel Workflow Summary:');
  console.log('='.repeat(80));
  console.log(`Workflow ID: ${workflowReceipt.workflow_id}`);
  console.log(`Case ID: ${caseReceipt.case_id}`);
  console.log(`Pattern used: AND-split (WP-2) + AND-join (WP-3)`);
  console.log(`Parallel tasks: 3 (legal, technical, finance reviews)`);
  console.log(`Total tasks: 5`);
  console.log();
  console.log('Key Insight:');
  console.log('  - AND-split enabled all 3 reviews simultaneously');
  console.log('  - AND-join only enabled "finalize" after ALL reviews completed');
  console.log('  - Reviews could be done in any order (or truly parallel in production)');
  console.log();
  console.log('✅ Parallel approval workflow completed successfully!');
  console.log();
}

main().catch(error => {
  console.error('Error running example:', error);
  process.exit(1);
});
