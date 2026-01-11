/**
 * @file Custom Events Tests
 * @module observability/test/custom-events
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { createCustomEvents, EventType, EventSeverity } from '../src/custom-events.mjs';

describe('CustomEvents', () => {
  let events;

  beforeEach(() => {
    events = createCustomEvents({
      serviceName: 'test-service',
      enabled: true,
    });
    events.clearEvents();
  });

  describe('Security Events', () => {
    it('should emit authentication failure event', () => {
      const event = events.emitAuthFailure({
        userId: 'user@example.com',
        reason: 'invalid_password',
        ip: '192.168.1.100',
      });

      expect(event).toBeDefined();
      expect(event.type).toBe(EventType.SECURITY_AUTH_FAILURE);
      expect(event.severity).toBe(EventSeverity.WARNING);
      expect(event.attributes['auth.user_id']).toBe('user@example.com');
    });

    it('should emit injection attempt event', () => {
      const event = events.emitInjectionAttempt({
        attackType: 'SPARQL',
        payload: 'DROP ALL; --',
        userId: 'attacker@evil.com',
        ip: '1.2.3.4',
      });

      expect(event).toBeDefined();
      expect(event.type).toBe(EventType.SECURITY_INJECTION_ATTEMPT);
      expect(event.severity).toBe(EventSeverity.CRITICAL);
      expect(event.attributes['injection.type']).toBe('SPARQL');
    });

    it('should hash sensitive payloads', () => {
      const event = events.emitInjectionAttempt({
        attackType: 'SQL',
        payload: 'SELECT * FROM users WHERE password="secret123"',
        ip: '1.2.3.4',
      });

      expect(event.attributes['injection.payload_hash']).toBeDefined();
      expect(event.attributes['injection.payload_hash']).not.toContain('secret123');
    });
  });

  describe('Performance Events', () => {
    it('should emit slow query event', () => {
      const event = events.emitSlowQuery({
        query: 'SELECT * WHERE { ?s ?p ?o }',
        duration: 2500,
        threshold: 1000,
      });

      expect(event).toBeDefined();
      expect(event.type).toBe(EventType.PERFORMANCE_SLOW_QUERY);
      expect(event.severity).toBe(EventSeverity.WARNING);
      expect(event.attributes['query.duration_ms']).toBe(2500);
    });

    it('should emit timeout warning event', () => {
      const event = events.emitTimeoutWarning({
        operation: 'federation-query',
        elapsed: 8500,
        timeout: 10000,
      });

      expect(event).toBeDefined();
      expect(event.type).toBe(EventType.PERFORMANCE_TIMEOUT_WARNING);
      expect(event.attributes['operation.remaining_ms']).toBe(1500);
    });

    it('should mark near-timeout as critical', () => {
      const event = events.emitTimeoutWarning({
        operation: 'critical-op',
        elapsed: 9500,
        timeout: 10000,
      });

      expect(event.severity).toBe(EventSeverity.CRITICAL);
    });

    it('should emit high memory event', () => {
      const event = events.emitHighMemory({
        heapUsed: 850 * 1024 * 1024,
        heapTotal: 1000 * 1024 * 1024,
        threshold: 0.85,
      });

      expect(event).toBeDefined();
      expect(event.type).toBe(EventType.PERFORMANCE_MEMORY_HIGH);
      expect(event.attributes['memory.usage_ratio']).toBeCloseTo(0.85, 2);
    });

    it('should mark critical memory usage', () => {
      const event = events.emitHighMemory({
        heapUsed: 950 * 1024 * 1024,
        heapTotal: 1000 * 1024 * 1024,
        threshold: 0.85,
      });

      expect(event.severity).toBe(EventSeverity.CRITICAL);
    });
  });

  describe('Business Events', () => {
    it('should emit workflow completion event', () => {
      const event = events.emitWorkflowComplete({
        workflowId: 'workflow-789',
        workflowType: 'data-ingestion',
        duration: 5400,
        success: true,
      });

      expect(event).toBeDefined();
      expect(event.type).toBe(EventType.BUSINESS_WORKFLOW_COMPLETE);
      expect(event.severity).toBe(EventSeverity.INFO);
      expect(event.correlationId).toBe('workflow-789');
    });

    it('should emit state change event', () => {
      const event = events.emitStateChange({
        entity: 'Dataset',
        entityId: 'dataset-123',
        fromState: 'processing',
        toState: 'complete',
        userId: 'user-456',
      });

      expect(event).toBeDefined();
      expect(event.type).toBe(EventType.BUSINESS_STATE_CHANGE);
      expect(event.attributes['state.from']).toBe('processing');
      expect(event.attributes['state.to']).toBe('complete');
    });
  });

  describe('Event Storage', () => {
    it('should store events', () => {
      events.emitAuthFailure({
        userId: 'user1',
        reason: 'test',
        ip: '1.2.3.4',
      });

      events.emitAuthFailure({
        userId: 'user2',
        reason: 'test',
        ip: '1.2.3.5',
      });

      expect(events.events.length).toBe(2);
    });

    it('should limit stored events to maxEvents', () => {
      // Emit more than max events
      for (let i = 0; i < 1100; i++) {
        events.emitBusinessEvent({
          type: 'test.event',
          message: `Event ${i}`,
        });
      }

      expect(events.events.length).toBeLessThanOrEqual(1000);
    });

    it('should clear events', () => {
      events.emitAuthFailure({
        userId: 'user',
        reason: 'test',
        ip: '1.2.3.4',
      });

      expect(events.events.length).toBeGreaterThan(0);

      events.clearEvents();

      expect(events.events.length).toBe(0);
    });
  });

  describe('Event Querying', () => {
    beforeEach(() => {
      // Create test events
      events.emitAuthFailure({
        userId: 'user1',
        reason: 'test',
        ip: '1.2.3.4',
      });

      events.emitInjectionAttempt({
        attackType: 'SPARQL',
        payload: 'test',
        ip: '1.2.3.5',
      });

      events.emitWorkflowComplete({
        workflowId: 'workflow-1',
        workflowType: 'test',
        duration: 100,
        success: true,
      });
    });

    it('should get events by type', () => {
      const authEvents = events.getEventsByType(EventType.SECURITY_AUTH_FAILURE);

      expect(authEvents.length).toBe(1);
      expect(authEvents[0].type).toBe(EventType.SECURITY_AUTH_FAILURE);
    });

    it('should limit returned events', () => {
      // Emit many events
      for (let i = 0; i < 20; i++) {
        events.emitBusinessEvent({
          type: 'test.event',
          message: `Event ${i}`,
        });
      }

      const limited = events.getEventsByType('test.event', { limit: 5 });

      expect(limited.length).toBeLessThanOrEqual(5);
    });

    it('should filter by timestamp', () => {
      const now = Date.now();

      const recent = events.getEventsByType(EventType.SECURITY_AUTH_FAILURE, {
        since: now - 1000,
      });

      expect(recent.length).toBeGreaterThan(0);
    });

    it('should get events by severity', () => {
      const criticalEvents = events.getEventsBySeverity(EventSeverity.CRITICAL);

      expect(criticalEvents.length).toBe(1); // Only injection attempt
      expect(criticalEvents[0].type).toBe(EventType.SECURITY_INJECTION_ATTEMPT);
    });

    it('should get events by correlation ID', () => {
      const correlatedEvents = events.getEventsByCorrelationId('workflow-1');

      expect(correlatedEvents.length).toBe(1);
      expect(correlatedEvents[0].correlationId).toBe('workflow-1');
    });
  });

  describe('Event Statistics', () => {
    beforeEach(() => {
      events.emitAuthFailure({
        userId: 'user1',
        reason: 'test',
        ip: '1.2.3.4',
      });

      events.emitAuthFailure({
        userId: 'user2',
        reason: 'test',
        ip: '1.2.3.5',
      });

      events.emitInjectionAttempt({
        attackType: 'SPARQL',
        payload: 'test',
        ip: '1.2.3.6',
      });
    });

    it('should compute event statistics', () => {
      const stats = events.getStats();

      expect(stats.total).toBe(3);
      expect(stats.bySeverity[EventSeverity.WARNING]).toBe(2);
      expect(stats.bySeverity[EventSeverity.CRITICAL]).toBe(1);
      expect(stats.byType[EventType.SECURITY_AUTH_FAILURE]).toBe(2);
      expect(stats.byCategory.security).toBe(3);
    });
  });

  describe('Custom Handler', () => {
    it('should call custom event handler', () => {
      const handlerCalls = [];

      const eventsWithHandler = createCustomEvents({
        enabled: true,
        eventHandler: event => handlerCalls.push(event),
      });

      eventsWithHandler.emitAuthFailure({
        userId: 'user',
        reason: 'test',
        ip: '1.2.3.4',
      });

      expect(handlerCalls.length).toBe(1);
      expect(handlerCalls[0].type).toBe(EventType.SECURITY_AUTH_FAILURE);
    });

    it('should handle handler errors gracefully', () => {
      const eventsWithBrokenHandler = createCustomEvents({
        enabled: true,
        eventHandler: () => {
          throw new Error('Handler error');
        },
      });

      // Should not throw
      expect(() => {
        eventsWithBrokenHandler.emitAuthFailure({
          userId: 'user',
          reason: 'test',
          ip: '1.2.3.4',
        });
      }).not.toThrow();
    });
  });

  describe('Disabled Events', () => {
    it('should not emit when disabled', () => {
      const disabledEvents = createCustomEvents({
        enabled: false,
      });

      const event = disabledEvents.emitAuthFailure({
        userId: 'user',
        reason: 'test',
        ip: '1.2.3.4',
      });

      expect(event).toBeNull();
      expect(disabledEvents.events.length).toBe(0);
    });
  });

  describe('Performance', () => {
    it('should emit events in <0.1ms', () => {
      const start = performance.now();

      for (let i = 0; i < 1000; i++) {
        events.emitBusinessEvent({
          type: 'perf.test',
          message: 'Performance test',
        });
      }

      const elapsed = performance.now() - start;
      const avgTime = elapsed / 1000;

      expect(avgTime).toBeLessThan(0.1); // <0.1ms per event
    });

    it('should have minimal memory overhead', () => {
      const before = process.memoryUsage().heapUsed;

      // Emit 1000 events
      for (let i = 0; i < 1000; i++) {
        events.emitBusinessEvent({
          type: 'memory.test',
          message: `Event ${i}`,
          attributes: {
            'test.index': i,
          },
        });
      }

      const after = process.memoryUsage().heapUsed;
      const overhead = (after - before) / 1024 / 1024; // MB

      expect(overhead).toBeLessThan(5); // <5MB for 1000 events
    });
  });
});
