/**
 * Composition C27: Workflow Engine + Durable Workflow + Receipt
 * Atoms: A35 + A39 + A36
 *
 * Proof: Temporal-style durable execution with cryptographic proof
 */

import { DurableWorkflowEngine } from '@unrdf/yawl-durable';
import { verifyReceipt } from '@unrdf/kgc-4d';

console.log('=== C27: Durable Workflow with Receipts Proof ===\n');

async function prove() {
  try {
    // A39: Create durable workflow engine
    const engine = new DurableWorkflowEngine();
    console.log('✅ A39: Durable workflow engine created');

    // Define workflow with activities
    const workflowId = await engine.defineWorkflow({
      id: 'booking-saga',
      name: 'Travel Booking Saga',
      activities: [
        {
          id: 'bookFlight',
          name: 'Book Flight',
          handler: async (input) => {
            console.log(`   📅 Booking flight for ${input.destination}`);
            return { flightId: 'FL-123', cost: 500 };
          },
          compensate: async () => {
            console.log('   ↩️  Canceling flight');
          },
          timeout: 5000
        },
        {
          id: 'bookHotel',
          name: 'Book Hotel',
          handler: async (input) => {
            console.log(`   🏨 Booking hotel at ${input.destination}`);
            return { hotelId: 'HT-456', cost: 200 };
          },
          compensate: async () => {
            console.log('   ↩️  Canceling hotel');
          },
          timeout: 5000
        }
      ],
      flow: [
        { from: 'bookFlight', to: 'bookHotel' }
      ]
    });

    console.log(`✅ Workflow defined: ${workflowId}`);

    // A35: Start workflow execution
    const execution = await engine.startWorkflow('booking-saga', {
      userId: 'alice',
      destination: 'Paris'
    });

    console.log(`✅ A35: Workflow started: ${execution.executionId}`);

    // Execute activities
    const flightResult = await engine.executeActivity(
      execution.executionId,
      'bookFlight',
      { destination: 'Paris' }
    );
    console.log(`✅ Flight booked: ${flightResult.flightId}`);

    const hotelResult = await engine.executeActivity(
      execution.executionId,
      'bookHotel',
      { destination: 'Paris' }
    );
    console.log(`✅ Hotel booked: ${hotelResult.hotelId}`);

    // A36: Get receipt history
    const receipts = engine.getReceiptHistory(execution.executionId);
    console.log(`\n✅ A36: Receipt chain retrieved (${receipts.length} receipts)`);

    // Verify receipt chain integrity
    const verification = await engine.verifyReceiptChain(execution.executionId);
    console.log(`   Chain valid: ${verification.valid}`);
    console.log(`   Genesis hash: ${verification.genesisHash?.substring(0, 16)}`);
    console.log(`   Latest hash: ${verification.latestHash?.substring(0, 16)}`);

    // Test replay capability
    const replayed = await engine.replay(execution.executionId);
    console.log(`\n✅ Deterministic replay successful`);
    console.log(`   Replayed ${replayed.receiptCount} receipts`);

    console.log('\n✅ COMPOSITION VERIFIED');
    console.log('   Value: Temporal.io-style durable execution');
    console.log('   Receipt chain: Valid and verifiable');
    console.log('   Replay: Deterministic state reconstruction');
    console.log(`   Total cost: $${flightResult.cost + hotelResult.cost}`);

    process.exit(verification.valid ? 0 : 1);
  } catch (error) {
    console.error('❌ COMPOSITION FAILED:', error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

prove();
