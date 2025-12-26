/**
 * Cancellation Regions Example
 *
 * This example demonstrates:
 * - Defining cancellation regions in workflows
 * - Cancelling work items mid-execution
 * - Handling cancellation with cleanup tasks
 * - Error recovery with transactional rollback
 *
 * Workflow Pattern: WP-19 (Cancel Region)
 *
 * Use case: E-commerce order with cancellation support
 *
 * Flow:
 *   Place Order → Process Payment → Ship Order
 *                      ↓
 *                  Cancel Order (can cancel payment/shipping)
 *
 * Run: node examples/yawl/04-cancellation-regions.mjs
 */

import { createWorkflow, createCase, enableTask, startTask, completeTask, cancelWorkItem } from '@unrdf/yawl';
import { createStore } from '@unrdf/oxigraph';

async function main() {
  console.log('='.repeat(80));
  console.log('YAWL Example 4: Cancellation Regions');
  console.log('='.repeat(80));
  console.log();

  // Step 1: Create store
  console.log('Step 1: Creating RDF store...');
  const store = createStore();
  console.log('✅ Store created\n');

  // Step 2: Create workflow with cancellation region
  console.log('Step 2: Creating order processing workflow with cancellation...');
  const workflowSpec = {
    id: 'order-processing',
    name: 'Order Processing with Cancellation',
    description: 'E-commerce order that can be cancelled before shipping',
    tasks: [
      { id: 'place-order', name: 'Place Order', kind: 'atomic' },
      { id: 'payment', name: 'Process Payment', kind: 'atomic' },
      { id: 'ship', name: 'Ship Order', kind: 'atomic' },
      { id: 'cancel', name: 'Cancel Order', kind: 'atomic' },
      { id: 'refund', name: 'Process Refund', kind: 'atomic' },
      { id: 'complete', name: 'Order Complete', kind: 'atomic' }
    ],
    flow: [
      // Normal flow
      { from: 'place-order', to: 'payment' },
      { from: 'payment', to: 'ship' },
      { from: 'ship', to: 'complete' },

      // Cancellation path (can happen anytime after placing order)
      { from: 'place-order', to: 'cancel' },
      { from: 'cancel', to: 'refund' },
      { from: 'refund', to: 'complete' }
    ],
    cancellationRegions: [
      {
        id: 'order-cancellable-region',
        tasks: ['payment', 'ship'],        // Tasks that can be cancelled
        cancelTrigger: 'cancel',           // Task that triggers cancellation
        rollbackActions: ['refund']        // Cleanup tasks to run on cancel
      }
    ]
  };

  const workflowReceipt = await createWorkflow(store, workflowSpec);
  console.log(`✅ Workflow created: ${workflowReceipt.workflow_id}\n`);

  console.log('='.repeat(80));
  console.log('SCENARIO 1: Normal Order Completion (No Cancellation)');
  console.log('='.repeat(80));
  console.log();

  // Case 1: Normal flow (no cancellation)
  const case1Receipt = await createCase(store, {
    workflowId: workflowReceipt.workflow_id,
    caseId: 'order-001',
    initialData: {
      orderId: 'ORD-001',
      customerId: 'customer-123',
      items: [{ sku: 'WIDGET-A', quantity: 2, price: 29.99 }],
      total: 59.98
    }
  });
  console.log(`✅ Case 1 started: ${case1Receipt.case_id}\n`);

  // Place order
  console.log('Step 1: Placing order...');
  const placeEnable1 = await enableTask(store, { caseId: case1Receipt.case_id, taskId: 'place-order' });
  const placeStart1 = await startTask(store, { caseId: case1Receipt.case_id, workItemId: placeEnable1.work_item_id, actor: 'customer@example.com' });
  const placeComplete1 = await completeTask(store, {
    caseId: case1Receipt.case_id,
    workItemId: placeStart1.work_item_id,
    outputData: { orderPlacedAt: new Date().toISOString() }
  });
  console.log(`   ✅ Order placed\n`);

  // Process payment
  console.log('Step 2: Processing payment...');
  const paymentEnable1 = await enableTask(store, { caseId: case1Receipt.case_id, taskId: 'payment' });
  const paymentStart1 = await startTask(store, { caseId: case1Receipt.case_id, workItemId: paymentEnable1.work_item_id, actor: 'payment-gateway@example.com' });
  const paymentComplete1 = await completeTask(store, {
    caseId: case1Receipt.case_id,
    workItemId: paymentStart1.work_item_id,
    outputData: { transactionId: 'TXN-123', charged: 59.98 }
  });
  console.log(`   ✅ Payment processed: $59.98\n`);

  // Ship order
  console.log('Step 3: Shipping order...');
  const shipEnable1 = await enableTask(store, { caseId: case1Receipt.case_id, taskId: 'ship' });
  const shipStart1 = await startTask(store, { caseId: case1Receipt.case_id, workItemId: shipEnable1.work_item_id, actor: 'warehouse@example.com' });
  const shipComplete1 = await completeTask(store, {
    caseId: case1Receipt.case_id,
    workItemId: shipStart1.work_item_id,
    outputData: { trackingNumber: 'TRACK-789', shippedAt: new Date().toISOString() }
  });
  console.log(`   ✅ Order shipped\n`);

  // Complete
  const completeEnable1 = await enableTask(store, { caseId: case1Receipt.case_id, taskId: 'complete' });
  const completeStart1 = await startTask(store, { caseId: case1Receipt.case_id, workItemId: completeEnable1.work_item_id, actor: 'system@example.com' });
  await completeTask(store, {
    caseId: case1Receipt.case_id,
    workItemId: completeStart1.work_item_id,
    outputData: { completedAt: new Date().toISOString() }
  });
  console.log(`✅ Case 1 completed successfully!\n`);

  console.log('='.repeat(80));
  console.log('SCENARIO 2: Order Cancelled During Payment Processing');
  console.log('='.repeat(80));
  console.log();

  // Case 2: Cancellation during payment
  const case2Receipt = await createCase(store, {
    workflowId: workflowReceipt.workflow_id,
    caseId: 'order-002',
    initialData: {
      orderId: 'ORD-002',
      customerId: 'customer-456',
      items: [{ sku: 'GADGET-B', quantity: 1, price: 199.99 }],
      total: 199.99
    }
  });
  console.log(`✅ Case 2 started: ${case2Receipt.case_id}\n`);

  // Place order
  console.log('Step 1: Placing order...');
  const placeEnable2 = await enableTask(store, { caseId: case2Receipt.case_id, taskId: 'place-order' });
  const placeStart2 = await startTask(store, { caseId: case2Receipt.case_id, workItemId: placeEnable2.work_item_id, actor: 'customer@example.com' });
  const placeComplete2 = await completeTask(store, {
    caseId: case2Receipt.case_id,
    workItemId: placeStart2.work_item_id,
    outputData: { orderPlacedAt: new Date().toISOString() }
  });
  console.log(`   ✅ Order placed\n`);

  // Start payment (but don't complete it)
  console.log('Step 2: Starting payment processing...');
  const paymentEnable2 = await enableTask(store, { caseId: case2Receipt.case_id, taskId: 'payment' });
  const paymentStart2 = await startTask(store, { caseId: case2Receipt.case_id, workItemId: paymentEnable2.work_item_id, actor: 'payment-gateway@example.com' });
  console.log(`   ⏳ Payment in progress (work item: ${paymentStart2.work_item_id})\n`);

  // Customer decides to cancel
  console.log('Step 3: Customer cancels order...');
  console.log('   ⚠️  Triggering cancellation region...\n');

  // Cancel the payment work item
  const cancelReceipt = await cancelWorkItem(store, {
    caseId: case2Receipt.case_id,
    workItemId: paymentStart2.work_item_id,
    reason: 'Customer requested cancellation',
    actor: 'customer@example.com'
  });

  console.log(`   ✅ Payment work item cancelled`);
  console.log(`      Cancellation receipt: ${cancelReceipt.receipt_id}`);
  console.log(`      Cancelled tasks: ${cancelReceipt.cancelled_tasks?.join(', ') || 'payment'}\n`);

  // Execute cancel task
  console.log('Step 4: Processing cancellation...');
  const cancelEnable = await enableTask(store, { caseId: case2Receipt.case_id, taskId: 'cancel' });
  const cancelStart = await startTask(store, { caseId: case2Receipt.case_id, workItemId: cancelEnable.work_item_id, actor: 'system@example.com' });
  const cancelComplete = await completeTask(store, {
    caseId: case2Receipt.case_id,
    workItemId: cancelStart.work_item_id,
    outputData: {
      cancelledAt: new Date().toISOString(),
      reason: 'Customer requested cancellation'
    }
  });
  console.log(`   ✅ Cancellation processed\n`);

  // Process refund (rollback action)
  console.log('Step 5: Processing refund (rollback action)...');
  const refundEnable = await enableTask(store, { caseId: case2Receipt.case_id, taskId: 'refund' });
  const refundStart = await startTask(store, { caseId: case2Receipt.case_id, workItemId: refundEnable.work_item_id, actor: 'payment-gateway@example.com' });
  const refundComplete = await completeTask(store, {
    caseId: case2Receipt.case_id,
    workItemId: refundStart.work_item_id,
    outputData: {
      refundAmount: 199.99,
      refundedAt: new Date().toISOString()
    }
  });
  console.log(`   ✅ Refund processed: $199.99\n`);

  // Complete
  const completeEnable2 = await enableTask(store, { caseId: case2Receipt.case_id, taskId: 'complete' });
  const completeStart2 = await startTask(store, { caseId: case2Receipt.case_id, workItemId: completeEnable2.work_item_id, actor: 'system@example.com' });
  await completeTask(store, {
    caseId: case2Receipt.case_id,
    workItemId: completeStart2.work_item_id,
    outputData: { completedAt: new Date().toISOString(), status: 'cancelled' }
  });
  console.log(`✅ Case 2 completed (order cancelled and refunded)!\n`);

  // Summary
  console.log('='.repeat(80));
  console.log('Cancellation Regions Summary:');
  console.log('='.repeat(80));
  console.log(`Workflow ID: ${workflowReceipt.workflow_id}`);
  console.log(`Pattern used: WP-19 (Cancel Region)`);
  console.log();
  console.log('Case 1 (order-001): Completed normally (no cancellation)');
  console.log('  Flow: Place → Payment → Ship → Complete');
  console.log();
  console.log('Case 2 (order-002): Cancelled during payment');
  console.log('  Flow: Place → Payment (started) → CANCELLED → Refund → Complete');
  console.log();
  console.log('Key Insights:');
  console.log('  - Cancellation regions define which tasks can be interrupted');
  console.log('  - cancelWorkItem() stops in-progress work items');
  console.log('  - Rollback actions (refund) clean up partial state');
  console.log('  - Same workflow handles both success and cancellation paths');
  console.log();
  console.log('✅ Cancellation regions example completed successfully!');
  console.log();
}

main().catch(error => {
  console.error('Error running example:', error);
  process.exit(1);
});
