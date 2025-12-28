/**
 * @fileoverview Demo 3: BEAM-Style Message Passing
 * Standalone version - no external dependencies
 *
 * Proves: BEAM message format roundtrip with validation
 */

// Message Validation
function validateBeamMessage(msg) {
  if (!msg.type || !msg.id || !msg.timestamp) {
    throw new Error('Invalid BEAM message: missing required fields');
  }
  if (msg.type === 'rpc' && (!msg.target || !msg.module || !msg.function || !msg.args)) {
    throw new Error('Invalid RPC message');
  }
  return true;
}

function createBeamMessage(type, payload) {
  const baseMessage = {
    id: `${Date.now()}-${Math.random().toString(36).substr(2, 9)}`,
    timestamp: Date.now(),
    ...payload,
  };
  const msg = { type, ...baseMessage };
  validateBeamMessage(msg);
  return msg;
}

function createBeamResponse(requestId, status, result = null, error = null) {
  return {
    type: 'response',
    id: requestId,
    status,
    result,
    error,
    timestamp: Date.now(),
  };
}

async function simulateBEAMProcess(message) {
  try {
    if (message.type === 'rpc') {
      const result = {
        module: message.module,
        function: message.function,
        args_count: message.args.length,
        processed_at: Date.now(),
      };
      return createBeamResponse(message.id, 'ok', result);
    } else if (message.type === 'triple_query') {
      const matchCount = Math.floor(Math.random() * 10) + 1;
      return createBeamResponse(message.id, 'ok', { matches: matchCount });
    }
  } catch (error) {
    return createBeamResponse(message.id, 'error', null, error.message);
  }
}

console.log('╔════════════════════════════════════════════════════════════════╗');
console.log('║  Demo 3: BEAM-Style Message Passing                           ║');
console.log('╚════════════════════════════════════════════════════════════════╝');

// Test 1: RPC Message Roundtrip
console.log('\n=== Test 1: RPC Message Roundtrip ===');
const message1 = createBeamMessage('rpc', {
  target: 'oxigraph_node',
  module: 'rdf_store',
  function: 'add_triples',
  args: [{ s: 'http://example.org/Alice', p: 'http://xmlns.com/foaf/0.1/name', o: 'Alice' }],
});
console.log('Sent message:');
console.log(JSON.stringify(message1, null, 2));
const response1 = await simulateBEAMProcess(message1);
console.log('\nReceived response:');
console.log(JSON.stringify(response1, null, 2));
const test1 = response1.id === message1.id && response1.status === 'ok' && response1.result !== null;
console.log(test1 ? '\n✅ SUCCESS: RPC message roundtrip verified' : '\n❌ FAIL: RPC message roundtrip failed');

// Test 2: Triple Query Message
console.log('\n=== Test 2: Triple Query Message ===');
const message2 = createBeamMessage('triple_query', {
  pattern: { subject: 'http://example.org/Alice', predicate: null, object: null },
});
console.log('Sent query:');
console.log(JSON.stringify(message2, null, 2));
const response2 = await simulateBEAMProcess(message2);
console.log('\nReceived response:');
console.log(JSON.stringify(response2, null, 2));
const test2 = response2.id === message2.id && response2.status === 'ok' && typeof response2.result.matches === 'number';
console.log(test2 ? `\n✅ SUCCESS: Query returned ${response2.result.matches} matches` : '\n❌ FAIL: Triple query failed');

// Test 3: Message Validation
console.log('\n=== Test 3: Message Validation ===');
let test3 = false;
try {
  const invalidMessage = { type: 'rpc', id: 'test-123', target: '', module: 'test', function: 'test', args: [], timestamp: Date.now() };
  try {
    validateBeamMessage(invalidMessage);
    console.log('❌ FAIL: Invalid message passed validation');
  } catch (validationError) {
    console.log('✅ SUCCESS: Invalid message rejected');
    console.log(`Validation error: ${validationError.message}`);
    test3 = true;
  }
} catch (error) {
  console.log(`❌ ERROR: ${error.message}`);
}

const allPassed = test1 && test2 && test3;
console.log('\n╔════════════════════════════════════════════════════════════════╗');
if (allPassed) {
  console.log('║  ✅ ALL TESTS PASSED - Message passing verified!              ║');
} else {
  console.log('║  ❌ SOME TESTS FAILED - See details above                      ║');
}
console.log('╚════════════════════════════════════════════════════════════════╝');
process.exit(allPassed ? 0 : 1);
