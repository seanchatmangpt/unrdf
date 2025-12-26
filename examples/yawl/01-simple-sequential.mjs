/**
 * Simple Sequential Workflow Example
 *
 * This example demonstrates:
 * - Creating a basic 3-task sequential workflow
 * - Starting a workflow case
 * - Executing tasks in order
 * - Handling workflow completion
 *
 * Estimated time: 2 minutes
 *
 * Run: node examples/yawl/01-simple-sequential.mjs
 */

import { createWorkflow, createCase, enableTask, startTask, completeTask } from '@unrdf/yawl';
import { createStore } from '@unrdf/oxigraph';

async function main() {
  console.log('='.repeat(80));
  console.log('YAWL Example 1: Simple Sequential Workflow');
  console.log('='.repeat(80));
  console.log();

  // Step 1: Create RDF store for workflow state
  console.log('Step 1: Creating RDF store...');
  const store = createStore();
  console.log('✅ Store created\n');

  // Step 2: Define workflow with 3 sequential tasks
  console.log('Step 2: Creating workflow definition...');
  const workflowSpec = {
    id: 'doc-approval',
    name: 'Document Approval Workflow',
    description: 'Simple 3-step document approval process',
    tasks: [
      {
        id: 'draft',
        name: 'Create Draft',
        kind: 'atomic',
        description: 'Author creates initial draft'
      },
      {
        id: 'review',
        name: 'Review Document',
        kind: 'atomic',
        description: 'Reviewer checks document for errors'
      },
      {
        id: 'publish',
        name: 'Publish Document',
        kind: 'atomic',
        description: 'Publish final document'
      }
    ],
    flow: [
      { from: 'draft', to: 'review' },
      { from: 'review', to: 'publish' }
    ]
  };

  const workflowReceipt = await createWorkflow(store, workflowSpec);
  console.log(`✅ Workflow created: ${workflowReceipt.workflow_id}`);
  console.log(`   Receipt ID: ${workflowReceipt.receipt_id}`);
  console.log(`   Hash: ${workflowReceipt.hash}\n`);

  // Step 3: Start a workflow case (instance)
  console.log('Step 3: Starting workflow case...');
  const caseReceipt = await createCase(store, {
    workflowId: workflowReceipt.workflow_id,
    caseId: 'case-001',
    initialData: {
      documentTitle: 'Q4 Financial Report',
      author: 'alice@example.com',
      createdAt: new Date().toISOString()
    }
  });

  console.log(`✅ Case started: ${caseReceipt.case_id}`);
  console.log(`   Initially enabled tasks: ${caseReceipt.enabled_tasks.join(', ')}\n`);

  // Step 4: Execute first task (draft)
  console.log('Step 4: Executing "draft" task...');

  // 4a. Enable task (already enabled at case start)
  const enableReceipt = await enableTask(store, {
    caseId: caseReceipt.case_id,
    taskId: 'draft'
  });
  console.log(`   ✓ Task enabled, work item: ${enableReceipt.work_item_id}`);

  // 4b. Start task (claim it)
  const startReceipt = await startTask(store, {
    caseId: caseReceipt.case_id,
    workItemId: enableReceipt.work_item_id,
    actor: 'alice@example.com'
  });
  console.log(`   ✓ Task started by ${startReceipt.actor} at ${startReceipt.started_at}`);

  // 4c. Complete task
  const completeReceipt1 = await completeTask(store, {
    caseId: caseReceipt.case_id,
    workItemId: startReceipt.work_item_id,
    outputData: {
      documentContent: 'Final draft content for Q4 financial report...',
      wordCount: 5000,
      completedBy: 'alice@example.com'
    }
  });
  console.log(`   ✅ Task completed!`);
  console.log(`      Next tasks enabled: ${completeReceipt1.enabled_tasks.join(', ')}\n`);

  // Step 5: Execute second task (review)
  console.log('Step 5: Executing "review" task...');

  const enableReceipt2 = await enableTask(store, {
    caseId: caseReceipt.case_id,
    taskId: 'review'
  });

  const startReceipt2 = await startTask(store, {
    caseId: caseReceipt.case_id,
    workItemId: enableReceipt2.work_item_id,
    actor: 'bob@example.com'
  });
  console.log(`   ✓ Task started by ${startReceipt2.actor}`);

  const completeReceipt2 = await completeTask(store, {
    caseId: caseReceipt.case_id,
    workItemId: startReceipt2.work_item_id,
    outputData: {
      reviewComments: 'Looks good, approved for publication',
      approved: true,
      reviewedBy: 'bob@example.com'
    }
  });
  console.log(`   ✅ Task completed!`);
  console.log(`      Next tasks enabled: ${completeReceipt2.enabled_tasks.join(', ')}\n`);

  // Step 6: Execute final task (publish)
  console.log('Step 6: Executing "publish" task...');

  const enableReceipt3 = await enableTask(store, {
    caseId: caseReceipt.case_id,
    taskId: 'publish'
  });

  const startReceipt3 = await startTask(store, {
    caseId: caseReceipt.case_id,
    workItemId: enableReceipt3.work_item_id,
    actor: 'system@example.com'
  });

  const completeReceipt3 = await completeTask(store, {
    caseId: caseReceipt.case_id,
    workItemId: startReceipt3.work_item_id,
    outputData: {
      publishedUrl: 'https://example.com/reports/q4-2024',
      publishedAt: new Date().toISOString()
    }
  });
  console.log(`   ✅ Task completed!`);
  console.log(`      Case status: ${completeReceipt3.case_status || 'completed'}\n`);

  // Summary
  console.log('='.repeat(80));
  console.log('Workflow Summary:');
  console.log('='.repeat(80));
  console.log(`Workflow ID: ${workflowReceipt.workflow_id}`);
  console.log(`Case ID: ${caseReceipt.case_id}`);
  console.log(`Total tasks executed: 3`);
  console.log(`Final status: Completed`);
  console.log();
  console.log('✅ Sequential workflow completed successfully!');
  console.log();
}

main().catch(error => {
  console.error('Error running example:', error);
  process.exit(1);
});
