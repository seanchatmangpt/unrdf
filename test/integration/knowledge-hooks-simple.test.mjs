/**
 * @fileoverview Simple tests for Knowledge Hooks
 * 
 * Tests the knowledge hooks system using the EventBus.
 * Focuses on basic functionality without complex scenarios.
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { useKnowledgeHooks } from "../../src/composables/use-knowledge-hooks.mjs";
import { EVENTS } from "../../src/engines/event-bus.mjs";
import { initStore } from "../../src/context/index.mjs";

describe('Knowledge Hooks Simple', () => {
  let runApp;

  beforeEach(() => {
    runApp = initStore();
  });

  describe('Basic Functionality', () => {
    it('should register and call a knowledge hook', () => {
      const hookEvents = [];

      runApp(() => {
        const hooks = useKnowledgeHooks();

        // Register a simple hook that always fires (no predicates)
        const unregister = hooks.on(
          'test:simple',
          [EVENTS.AFTER_ADD_QUAD],
          (payload, ok) => {
            hookEvents.push({
              hookId: 'test:simple',
              fired: ok,
              quad: payload.quad?.id || payload.quad?.value || payload.quad?.toString()
            });
          },
          [] // No predicates = always fires
        );

        // Add a quad to trigger the hook
        const subject = hooks.engine.namedNode('http://test/subject');
        const predicate = hooks.engine.namedNode('http://test/predicate');
        const object = hooks.engine.literal('test object');

        hooks.engine.store.addQuad(subject, predicate, object);

        // Cleanup
        unregister();
      });

      // Verify knowledge hook fired
      expect(hookEvents).toHaveLength(1);
      expect(hookEvents[0].hookId).toBe('test:simple');
      expect(hookEvents[0].fired).toBe(true);
      expect(hookEvents[0].quad).toContain('http://test/subject');
    });

    it('should handle multiple knowledge hooks', () => {
      const hookEvents = [];

      runApp(() => {
        const hooks = useKnowledgeHooks();

        // Register two hooks
        const unregister1 = hooks.on(
          'test:hook1',
          [EVENTS.AFTER_ADD_QUAD],
          (payload, ok) => {
            hookEvents.push({ hookId: 'hook1', fired: ok });
          },
          [] // No predicates = always fires
        );

        const unregister2 = hooks.on(
          'test:hook2',
          [EVENTS.AFTER_ADD_QUAD],
          (payload, ok) => {
            hookEvents.push({ hookId: 'hook2', fired: ok });
          },
          [] // No predicates = always fires
        );

        // Add a quad to trigger both hooks
        const subject = hooks.engine.namedNode('http://test/subject');
        const predicate = hooks.engine.namedNode('http://test/predicate');
        const object = hooks.engine.literal('test object');

        hooks.engine.store.addQuad(subject, predicate, object);

        // Cleanup
        unregister1();
        unregister2();
      });

      // Verify both hooks fired
      expect(hookEvents).toHaveLength(2);
      expect(hookEvents.map(e => e.hookId)).toEqual(expect.arrayContaining(['hook1', 'hook2']));
      expect(hookEvents.every(e => e.fired)).toBe(true);
    });

    it('should not call hooks when predicates fail', () => {
      const hookEvents = [];

      runApp(() => {
        const hooks = useKnowledgeHooks();

        // Register a hook with a predicate that will fail
        const unregister = hooks.on(
          'test:predicate',
          [EVENTS.AFTER_ADD_QUAD],
          (payload, ok) => {
            hookEvents.push({ hookId: 'test:predicate', fired: ok });
          },
          [
            {
              kind: 'ASK',
              spec: {
                query: 'ASK WHERE { ?s <http://nonexistent/property> "nonexistent" }'
              }
            }
          ]
        );

        // Add a quad that doesn't match the predicate
        const subject = hooks.engine.namedNode('http://test/subject');
        const predicate = hooks.engine.namedNode('http://test/predicate');
        const object = hooks.engine.literal('test object');

        hooks.engine.store.addQuad(subject, predicate, object);

        // Cleanup
        unregister();
      });

      // Verify hook was called but predicate failed
      expect(hookEvents).toHaveLength(1);
      expect(hookEvents[0].hookId).toBe('test:predicate');
      expect(hookEvents[0].fired).toBe(false);
    });
  });
});
