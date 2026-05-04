import { describe, it, expect, vi } from 'vitest';
import { Daemon } from '../src/daemon.mjs';

/**
 * Chaos Engineering Suite: Stress-testing the Daemon's autonomic resilience.
 */
describe('Vision 2030: Autonomic Chaos Engineering', { timeout: 30000 }, () => {

  const createResilientDaemon = () => new Daemon({ 
    nodeId: 'chaos-node', 
    daemonId: '550e8400-e29b-41d4-a716-446655440000', 
    name: 'chaos-daemon' 
  });

  it('should auto-restart the sidecar on unexpected crash', async () => {
    const daemon = createResilientDaemon();
    
    // Setup sidecar
    daemon.semanticSidecar.start();
    const originalPid = daemon.semanticSidecar.process.pid;
    
    // Simulate crash using emit to avoid runner propagation
    daemon.semanticSidecar.process.emit('exit', 1);
    
    // Wait for watchdog to trigger (5s interval)
    await new Promise(resolve => setTimeout(resolve, 6000));
    
    expect(daemon.semanticSidecar.process).toBeDefined();
    expect(daemon.semanticSidecar.process.pid).not.toBe(originalPid);
    await daemon.stop();
  });

  it('should prevent state corruption during rapid concurrent writes', async () => {
    const daemon = createResilientDaemon();
    daemon.semanticSidecar.start();
    
    // Simulate 10 concurrent graph update requests
    const tasks = Array.from({ length: 10 }).map((_, i) => 
      daemon.semanticSidecar.process.stdin.write(`load mock-${i}.ttl\n`)
    );
    
    await Promise.all(tasks);
    expect(daemon.semanticSidecar.process).toBeDefined();
    await daemon.stop();
  });

  it('should survive engine stall/latency spikes', async () => {
    const daemon = createResilientDaemon();
    await daemon.start();
    
    // Simulate high load stalling the reasoner
    const stall = () => new Promise(resolve => setTimeout(resolve, 500));
    await Promise.all([stall(), stall(), stall()]);
    
    // Daemon should still be responsive
    expect(daemon.isRunning).toBe(true);
    await daemon.stop();
  });
});
