/**
 * Bridge Tests
 *
 * **80/20 Focus**: Tests that prove bridge works correctly:
 * - Event emission works
 * - Hook registration works
 * - Intent processing works
 * - Error handling works
 *
 * @module bridge.test
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { KGC4DBridge, getBridge, setBridge } from '../src/kgc4d-bridge.mjs';
import { interceptAtomVMOutput } from '../src/bridge-interceptor.mjs';
import { resetSLAStats } from '../../src/roundtrip-sla.mjs';

describe('KGC4DBridge', () => {
  let bridge;
  const logMessages = [];

  beforeEach(() => {
    // Poka-Yoke: Reset SLA stats between tests to prevent error rate violations
    resetSLAStats();
    logMessages.length = 0;
    bridge = new KGC4DBridge({
      log: (message) => logMessages.push(message)
    });
  });

  describe('emitEvent', () => {
    it('should emit event to KGC-4D store', async () => {
      const result = await bridge.emitEvent('TEST', { test: true });

      expect(result.success).toBe(true);
      expect(result.receipt).toBeDefined();
      expect(result.receipt.id).toBeDefined();
      expect(result.receipt.timestamp).toBeDefined();
    });

    it('should handle event emission errors', async () => {
      // Create a bridge with invalid store (simulate error)
      const errorBridge = new KGC4DBridge({
        log: () => {}
      });
      
      // Mock store to throw error
      errorBridge.store = {
        appendEvent: async () => {
          throw new Error('Store error');
        }
      };

      const result = await errorBridge.emitEvent('TEST', {});

      expect(result.success).toBe(false);
      expect(result.error).toBeDefined();
    });

    it('should add event to event log', async () => {
      await bridge.emitEvent('TEST', { test: true });

      const eventLog = bridge.getEventLog();
      expect(eventLog.length).toBeGreaterThan(0);
      expect(eventLog[0].type).toBe('TEST');
    });
  });

  describe('registerHook', () => {
    it('should register hook successfully', () => {
      const result = bridge.registerHook({
        name: 'test-hook',
        trigger: 'before-add'
      });

      expect(result.success).toBe(true);
      expect(result.hookName).toBe('test-hook');
    });

    it('should handle hook registration errors', () => {
      // Register same hook twice should fail (poka-yoke prevents duplicate registration)
      bridge.registerHook({
        name: 'duplicate-hook',
        trigger: 'before-add'
      });

      // Poka-Yoke: Duplicate registration should throw error
      expect(() => {
        bridge.registerHook({
          name: 'duplicate-hook',
          trigger: 'before-add'
        });
      }).toThrow('Hook already registered: duplicate-hook');
    });
  });

  describe('processIntent', () => {
    it('should process intent and return outcome', async () => {
      const result = await bridge.processIntent('intent-1', {
        description: 'Test intent'
      });

      expect(result.success).toBe(true);
      expect(result.outcome).toBeDefined();
      expect(result.outcome.intentId).toBe('intent-1');
      expect(result.outcome.accepted).toBe(true);
    });

    it('should store intent outcome', async () => {
      await bridge.processIntent('intent-2', {
        description: 'Test intent 2'
      });

      const outcome = bridge.getIntentOutcome('intent-2');
      expect(outcome).toBeDefined();
      expect(outcome.intent).toBeDefined();
      expect(outcome.outcome).toBeDefined();
    });

    it('should handle intent processing errors', async () => {
      // Mock emitEvent to fail
      bridge.emitEvent = async () => ({
        success: false,
        error: 'Emission failed'
      });

      const result = await bridge.processIntent('intent-3', {
        description: 'Test intent 3'
      });

      expect(result.success).toBe(false);
      expect(result.error).toBeDefined();
    });
  });

  describe('getEventLog', () => {
    it('should return event log', async () => {
      await bridge.emitEvent('TEST1', {});
      await bridge.emitEvent('TEST2', {});

      const eventLog = bridge.getEventLog();
      expect(eventLog.length).toBe(2);
    });
  });

  describe('getIntentOutcome', () => {
    it('should return intent outcome if exists', async () => {
      await bridge.processIntent('intent-1', { description: 'Test' });

      const outcome = bridge.getIntentOutcome('intent-1');
      expect(outcome).toBeDefined();
    });

    it('should return null if intent does not exist', () => {
      const outcome = bridge.getIntentOutcome('nonexistent');
      expect(outcome).toBeNull();
    });
  });

  describe('clear', () => {
    it('should clear event log and intents', async () => {
      await bridge.emitEvent('TEST', {});
      await bridge.processIntent('intent-1', { description: 'Test' });

      bridge.clear();

      expect(bridge.getEventLog().length).toBe(0);
      expect(bridge.getIntentOutcome('intent-1')).toBeNull();
    });
  });
});

describe('Bridge Interceptor', () => {
  it('should intercept emit_event command', () => {
    const logMessages = [];
    const mockModule = {
      print: (text) => logMessages.push(text),
      printErr: (text) => logMessages.push(text)
    };

    interceptAtomVMOutput(mockModule, (msg) => logMessages.push(`[LOG] ${msg}`));

    // Simulate Erlang output
    mockModule.print('KGC4D_BRIDGE:emit_event:TEST:{"test":true}');

    // Should have logged the bridge command
    expect(logMessages.some(msg => msg.includes('[Bridge]') || msg.includes('Event emitted'))).toBe(true);
  });

  it('should intercept register_hook command', () => {
    const logMessages = [];
    const mockModule = {
      print: (text) => logMessages.push(text),
      printErr: (text) => logMessages.push(text)
    };

    interceptAtomVMOutput(mockModule, (msg) => logMessages.push(`[LOG] ${msg}`));

    // Simulate Erlang output
    mockModule.print('KGC4D_BRIDGE:register_hook:test-hook:before-add');

    // Should have logged the bridge command (synchronous)
    expect(logMessages.some(msg => msg.includes('[Bridge]') || msg.includes('Hook registered'))).toBe(true);
  });

  it('should pass through non-bridge commands', () => {
    const logMessages = [];
    const mockModule = {
      print: (text) => logMessages.push(text),
      printErr: (text) => logMessages.push(text)
    };

    interceptAtomVMOutput(mockModule, () => {});

    // Simulate normal Erlang output
    mockModule.print('Normal Erlang output');

    // Should have passed through
    expect(logMessages).toContain('Normal Erlang output');
  });

  it('should handle invalid bridge commands', () => {
    const logMessages = [];
    const mockModule = {
      print: (text) => logMessages.push(text),
      printErr: (text) => logMessages.push(text)
    };

    interceptAtomVMOutput(mockModule, (msg) => logMessages.push(`[LOG] ${msg}`));

    // Simulate invalid command
    mockModule.print('KGC4D_BRIDGE:unknown_command:args');

    // Should have logged error
    expect(logMessages.some(msg => msg.includes('Unknown command') || msg.includes('unknown_command'))).toBe(true);
  });
});

describe('getBridge / setBridge', () => {
  it('should return same bridge instance', () => {
    const bridge1 = getBridge();
    const bridge2 = getBridge();

    expect(bridge1).toBe(bridge2);
  });

  it('should allow setting bridge instance', () => {
    const customBridge = new KGC4DBridge();
    setBridge(customBridge);

    const retrieved = getBridge();
    expect(retrieved).toBe(customBridge);
  });
});

