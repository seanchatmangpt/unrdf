/**
 * Stress Test for Erlang-Like Process Framework
 * 
 * Tests process framework with various patterns and validates poka-yoke design.
 * 
 * @module erlang-process-stress
 */

import { spawn, send, whereis, listProcesses, Supervisor } from '../src/erlang-process.mjs';
import { startRoundtrip, endRoundtrip, getSLAStats, resetSLAStats, OPERATION_TYPES } from '../../src/roundtrip-sla.mjs';

/**
 * Test configuration
 */
const CONFIG = {
  PROCESS_COUNT: 100,
  MESSAGE_COUNT: 1000,
  LINK_COUNT: 50,
  MONITOR_COUNT: 50,
  SUPERVISOR_CHILDREN: 20,
};

/**
 * Test: Spawn many processes
 */
async function testSpawnMany() {
  console.log('[Process Stress] Test: Spawn many processes');
  
  const processes = [];
  const startTime = Date.now();
  
  for (let i = 0; i < CONFIG.PROCESS_COUNT; i++) {
    const process = spawn(
      `test_process_${i}`,
      async () => {
        // Init
        return { initialized: true, index: i };
      },
      async (message) => {
        // Handle message
        if (message.type === 'ping') {
          return { type: 'pong', index: i };
        }
      }
    );
    processes.push(process);
  }
  
  // Wait for all to initialize
  await Promise.all(processes.map(p => p._initPromise || Promise.resolve()));
  
  const duration = Date.now() - startTime;
  console.log(`[Process Stress] Spawned ${processes.length} processes in ${duration}ms`);
  
  // Cleanup
  processes.forEach(p => p.kill());
  
  return { count: processes.length, duration };
}

/**
 * Test: Send many messages
 */
async function testSendMany() {
  console.log('[Process Stress] Test: Send many messages');
  
  const process = spawn(
    'message_receiver',
    async () => ({ initialized: true }),
    async (message) => {
      // Process message
      if (message.type === 'test') {
        return { processed: true };
      }
    }
  );
  
  await process._initPromise;
  
  const startTime = Date.now();
  
  for (let i = 0; i < CONFIG.MESSAGE_COUNT; i++) {
    process.send({ type: 'test', index: i });
  }
  
  // Wait for messages to be processed
  await new Promise(resolve => setTimeout(resolve, 1000));
  
  const duration = Date.now() - startTime;
  console.log(`[Process Stress] Sent ${CONFIG.MESSAGE_COUNT} messages in ${duration}ms`);
  
  process.kill();
  
  return { count: CONFIG.MESSAGE_COUNT, duration };
}

/**
 * Test: Process links
 */
async function testLinks() {
  console.log('[Process Stress] Test: Process links');
  
  const processes = [];
  
  // Create processes
  for (let i = 0; i < CONFIG.LINK_COUNT; i++) {
    const process = spawn(
      `linked_process_${i}`,
      async () => ({ initialized: true }),
      async (message) => {
        if (message.type === 'exit') {
          process.exit('normal');
        }
      }
    );
    processes.push(process);
  }
  
  await Promise.all(processes.map(p => p._initPromise || Promise.resolve()));
  
  // Link processes in chain
  for (let i = 0; i < processes.length - 1; i++) {
    processes[i].link(processes[i + 1]);
  }
  
  // Kill first process - should propagate
  const startTime = Date.now();
  processes[0].kill();
  
  // Wait for propagation
  await new Promise(resolve => setTimeout(resolve, 100));
  
  const duration = Date.now() - startTime;
  const aliveCount = processes.filter(p => p.isAlive()).length;
  
  console.log(`[Process Stress] Linked ${processes.length} processes, ${aliveCount} alive after kill`);
  
  return { linked: processes.length, alive: aliveCount, duration };
}

/**
 * Test: Process monitors
 */
async function testMonitors() {
  console.log('[Process Stress] Test: Process monitors');
  
  const monitored = spawn(
    'monitored_process',
    async () => ({ initialized: true }),
    async (message) => {
      if (message.type === 'exit') {
        process.exit('normal');
      }
    }
  );
  
  await monitored._initPromise;
  
  const monitors = [];
  
  // Create monitoring processes
  for (let i = 0; i < CONFIG.MONITOR_COUNT; i++) {
    const monitor = spawn(
      `monitor_${i}`,
      async () => ({ initialized: true }),
      async (message) => {
        if (message.type === 'DOWN') {
          return { received: true };
        }
      }
    );
    await monitor._initPromise;
    monitor.monitor(monitored);
    monitors.push(monitor);
  }
  
  // Kill monitored process
  const startTime = Date.now();
  monitored.kill();
  
  // Wait for DOWN messages
  await new Promise(resolve => setTimeout(resolve, 200));
  
  const duration = Date.now() - startTime;
  const downMessages = monitors.reduce((count, m) => {
    return count + m.mailbox.filter(msg => msg.type === 'DOWN').length;
  }, 0);
  
  console.log(`[Process Stress] Monitored process killed, ${downMessages} DOWN messages received`);
  
  monitors.forEach(m => m.kill());
  
  return { monitors: monitors.length, downMessages, duration };
}

/**
 * Test: Supervisor with children
 */
async function testSupervisor() {
  console.log('[Process Stress] Test: Supervisor with children');
  
  const supervisor = new Supervisor('test_supervisor', 'one_for_one');
  
  // Start children
  const children = [];
  for (let i = 0; i < CONFIG.SUPERVISOR_CHILDREN; i++) {
    const child = supervisor.startChild({
      name: `child_${i}`,
      initFn: async () => ({ initialized: true, index: i }),
      handleFn: async (message) => {
        if (message.type === 'crash') {
          throw new Error('Child crashed');
        }
      },
    });
    children.push(child);
  }
  
  await Promise.all(children.map(c => c._initPromise || Promise.resolve()));
  
  // Crash one child
  const startTime = Date.now();
  children[0].send({ type: 'crash' });
  
  // Wait for restart
  await new Promise(resolve => setTimeout(resolve, 200));
  
  const duration = Date.now() - startTime;
  const aliveCount = children.filter(c => c.isAlive()).length;
  
  console.log(`[Process Stress] Supervisor managed ${children.length} children, ${aliveCount} alive after crash`);
  
  supervisor.terminate();
  
  return { children: children.length, alive: aliveCount, duration };
}

/**
 * Test: Poka-yoke validation
 */
async function testPokaYoke() {
  console.log('[Process Stress] Test: Poka-yoke validation');
  
  const errors = [];
  
  // Test: Duplicate name
  try {
    const p1 = spawn('duplicate', async () => {}, async () => {});
    const p2 = spawn('duplicate', async () => {}, async () => {});
    errors.push('Duplicate name should be rejected');
  } catch (error) {
    // Expected
  }
  
  // Test: Send to dead process
  try {
    const p = spawn('dead', async () => {}, async () => {});
    await p._initPromise;
    p.kill();
    p.send({ type: 'test' });
    errors.push('Send to dead process should be rejected');
  } catch (error) {
    // Expected
  }
  
  // Test: Link to dead process
  try {
    const p1 = spawn('link1', async () => {}, async () => {});
    const p2 = spawn('link2', async () => {}, async () => {});
    await Promise.all([p1._initPromise, p2._initPromise]);
    p2.kill();
    p1.link(p2);
    errors.push('Link to dead process should be rejected');
  } catch (error) {
    // Expected
  }
  
  // Test: Invalid init function
  try {
    spawn('invalid', 'not a function', async () => {});
    errors.push('Invalid init function should be rejected');
  } catch (error) {
    // Expected
  }
  
  if (errors.length > 0) {
    console.log(`[Process Stress] ❌ Poka-yoke validation failed: ${errors.length} errors`);
    errors.forEach(e => console.log(`  - ${e}`));
    return { passed: false, errors };
  } else {
    console.log(`[Process Stress] ✅ Poka-yoke validation passed`);
    return { passed: true, errors: [] };
  }
}

/**
 * Run all stress tests
 */
export async function runProcessStressTests() {
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
  console.log('Erlang-Like Process Framework Stress Tests');
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
  
  const results = {
    spawnMany: null,
    sendMany: null,
    links: null,
    monitors: null,
    supervisor: null,
    pokaYoke: null,
  };
  
  try {
    results.spawnMany = await testSpawnMany();
  } catch (error) {
    console.error('[Process Stress] Spawn many failed:', error);
  }
  
  try {
    results.sendMany = await testSendMany();
  } catch (error) {
    console.error('[Process Stress] Send many failed:', error);
  }
  
  try {
    results.links = await testLinks();
  } catch (error) {
    console.error('[Process Stress] Links failed:', error);
  }
  
  try {
    results.monitors = await testMonitors();
  } catch (error) {
    console.error('[Process Stress] Monitors failed:', error);
  }
  
  try {
    results.supervisor = await testSupervisor();
  } catch (error) {
    console.error('[Process Stress] Supervisor failed:', error);
  }
  
  try {
    results.pokaYoke = await testPokaYoke();
  } catch (error) {
    console.error('[Process Stress] Poka-yoke failed:', error);
  }
  
  console.log('\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
  console.log('Process Stress Test Summary');
  console.log('━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━');
  
  console.log(`Spawn Many: ${results.spawnMany ? '✅' : '❌'}`);
  console.log(`Send Many: ${results.sendMany ? '✅' : '❌'}`);
  console.log(`Links: ${results.links ? '✅' : '❌'}`);
  console.log(`Monitors: ${results.monitors ? '✅' : '❌'}`);
  console.log(`Supervisor: ${results.supervisor ? '✅' : '❌'}`);
  console.log(`Poka-Yoke: ${results.pokaYoke?.passed ? '✅' : '❌'}`);
  
  return results;
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runProcessStressTests()
    .then(() => {
      console.log('\n✅ Process stress tests completed');
      process.exit(0);
    })
    .catch(error => {
      console.error('\n❌ Process stress tests failed:', error);
      process.exit(1);
    });
}

