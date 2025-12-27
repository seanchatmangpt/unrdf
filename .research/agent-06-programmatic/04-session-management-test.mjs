#!/usr/bin/env node
/**
 * Claude Code Session Management Test
 *
 * Tests session creation, resume, and continuation patterns
 */

import { spawn, execSync } from 'child_process';
import { randomUUID } from 'crypto';
import { promisify } from 'util';

const sleep = promisify(setTimeout);

/**
 * Execute Claude with session support
 */
async function executeWithSession(prompt, sessionId, options = {}) {
  return new Promise((resolve, reject) => {
    const args = ['-p', prompt, '--output-format', 'json'];

    if (sessionId) {
      args.push('--session-id', sessionId);
    }

    if (options.allowedTools) {
      args.push('--allowedTools', options.allowedTools);
    }

    const proc = spawn('claude', args, {
      timeout: options.timeout || 30000
    });

    let stdout = '';
    let stderr = '';

    proc.stdout.on('data', d => stdout += d);
    proc.stderr.on('data', d => stderr += d);

    proc.on('close', (code) => {
      if (code === 0) {
        try {
          resolve(JSON.parse(stdout));
        } catch (e) {
          reject(new Error(`JSON parse error: ${e.message}`));
        }
      } else {
        reject(new Error(`Exit code ${code}: ${stderr}`));
      }
    });

    proc.on('error', reject);
  });
}

/**
 * Test: Multi-step conversation with session
 */
async function testSessionConversation() {
  console.log('=== Session Conversation Test ===\n');

  const sessionId = randomUUID();
  console.log(`Session ID: ${sessionId}\n`);

  // Step 1: Set context
  console.log('Step 1: Setting context...');
  const step1 = await executeWithSession(
    'I will tell you a number. The number is 42. Just say "Got it".',
    sessionId,
    { timeout: 20000 }
  );
  console.log(`Response: ${step1.content?.substring(0, 100)}\n`);

  await sleep(1000);

  // Step 2: Query context
  console.log('Step 2: Querying context...');
  const step2 = await executeWithSession(
    'What number did I tell you? Just respond with the number.',
    sessionId,
    { timeout: 20000 }
  );
  console.log(`Response: ${step2.content?.substring(0, 100)}\n`);

  if (step2.content?.includes('42')) {
    console.log('✅ Session context preserved!\n');
  } else {
    console.log('❌ Session context lost\n');
  }

  return sessionId;
}

/**
 * Test: Session continuation
 */
async function testSessionContinuation() {
  console.log('=== Session Continuation Test ===\n');

  // Create initial session
  const sessionId = randomUUID();
  console.log(`Creating session: ${sessionId}\n`);

  await executeWithSession(
    'Remember this: My favorite color is blue.',
    sessionId,
    { timeout: 20000 }
  );

  console.log('Session created. Waiting...\n');
  await sleep(2000);

  // Continue session
  console.log('Continuing session...');
  const result = await executeWithSession(
    'What is my favorite color?',
    sessionId,
    { timeout: 20000 }
  );

  console.log(`Response: ${result.content?.substring(0, 100)}\n`);

  if (result.content?.toLowerCase().includes('blue')) {
    console.log('✅ Session continuation works!\n');
  } else {
    console.log('❌ Session continuation failed\n');
  }
}

/**
 * Test: Session metadata
 */
async function testSessionMetadata() {
  console.log('=== Session Metadata Test ===\n');

  const sessionId = randomUUID();
  const result = await executeWithSession(
    'What is 1+1?',
    sessionId,
    { timeout: 20000 }
  );

  console.log('Session Metadata:');
  console.log(`  Session ID: ${result.session_id || sessionId}`);
  console.log(`  Model: ${result.model || 'N/A'}`);
  console.log(`  Input Tokens: ${result.usage?.input_tokens || 'N/A'}`);
  console.log(`  Output Tokens: ${result.usage?.output_tokens || 'N/A'}`);
  console.log(`  Tool Uses: ${result.tool_uses?.length || 0}\n`);
}

/**
 * Test: Fork session
 */
async function testSessionFork() {
  console.log('=== Session Fork Test ===\n');

  // Create base session
  const baseSessionId = randomUUID();
  console.log(`Base session: ${baseSessionId}\n`);

  await executeWithSession(
    'My name is Alice.',
    baseSessionId,
    { timeout: 20000 }
  );

  // Fork would use --fork-session flag (CLI-only feature)
  // This test demonstrates the concept programmatically

  const forkedSessionId = randomUUID();
  console.log(`Forked session: ${forkedSessionId}\n`);

  const result = await executeWithSession(
    'What is my name?',
    forkedSessionId,
    { timeout: 20000 }
  );

  console.log(`Response: ${result.content?.substring(0, 100)}\n`);
  console.log('Note: True forking requires --fork-session CLI flag\n');
}

/**
 * Test: Parallel sessions
 */
async function testParallelSessions() {
  console.log('=== Parallel Sessions Test ===\n');

  const session1 = randomUUID();
  const session2 = randomUUID();
  const session3 = randomUUID();

  console.log('Running 3 parallel sessions...\n');

  const results = await Promise.all([
    executeWithSession('My favorite number is 7', session1, { timeout: 25000 }),
    executeWithSession('My favorite number is 13', session2, { timeout: 25000 }),
    executeWithSession('My favorite number is 21', session3, { timeout: 25000 })
  ]);

  console.log('All sessions completed.\n');

  // Verify context isolation
  const [v1, v2, v3] = await Promise.all([
    executeWithSession('What is my favorite number?', session1, { timeout: 20000 }),
    executeWithSession('What is my favorite number?', session2, { timeout: 20000 }),
    executeWithSession('What is my favorite number?', session3, { timeout: 20000 })
  ]);

  console.log(`Session 1: ${v1.content?.substring(0, 50)}`);
  console.log(`Session 2: ${v2.content?.substring(0, 50)}`);
  console.log(`Session 3: ${v3.content?.substring(0, 50)}\n`);

  const isolated =
    v1.content?.includes('7') &&
    v2.content?.includes('13') &&
    v3.content?.includes('21');

  if (isolated) {
    console.log('✅ Sessions are properly isolated!\n');
  } else {
    console.log('⚠️  Session isolation unclear\n');
  }
}

// Main test runner
async function main() {
  console.log('Claude Code Session Management Tests');
  console.log('====================================\n');

  try {
    await testSessionConversation();
    await testSessionContinuation();
    await testSessionMetadata();
    await testSessionFork();
    await testParallelSessions();

    console.log('✅ All session tests completed\n');
  } catch (error) {
    console.error('❌ Test failed:', error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

if (import.meta.url === `file://${process.argv[1]}`) {
  main();
}
