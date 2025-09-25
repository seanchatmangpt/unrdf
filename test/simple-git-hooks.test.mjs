/**
 * @fileoverview Simple Git Hooks Integration Test
 * 
 * Tests the core hook functionality with a simplified approach.
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { RdfEngine } from "../src/engines/rdf-engine.mjs";
import { registerHook } from "../src/engines/minimal-hook-manager.mjs";
import { ingress, egress } from "../src/engines/deterministic-adapters.mjs";
import { writeWithProv } from "../src/engines/mandatory-provenance.mjs";
import { EVENTS } from "../src/engines/event-bus.mjs";
import { z } from "zod";

// Simple schemas for testing
const SimpleCommit = z.object({
  hash: z.string(),
  message: z.string()
});

describe('Simple Git Hooks Integration', () => {
  let engine;
  let events = [];

  beforeEach(() => {
    engine = new RdfEngine({ eventsEnabled: true });
    events = [];
    // Clear any existing hooks
    engine.clearHooks();
  });

  it('should register and fire basic hooks', async () => {
    // Register a simple hook that fires on any quad addition
    const unregister = registerHook(engine, {
      id: 'test-hook',
      events: [EVENTS.AFTER_ADD_QUAD],
      action: async (payload, ok) => {
        events.push({
          hook: 'test-hook',
          event: payload.event,
          allowed: ok,
          timestamp: new Date().toISOString()
        });
      }
    });

    // Add some data
    const commitData = {
      hash: 'abc123',
      message: 'Test commit'
    };

    const commitRes = await ingress.fromJSON(
      SimpleCommit,
      JSON.stringify(commitData),
      { 
        base: 'http://ex/',
        subject: 'urn:test:commit:abc123'
      }
    );

    writeWithProv(engine, commitRes.rdf, 'test', {
      operation: 'test',
      user: 'test-user'
    });

    // Wait for async processing
    await new Promise(resolve => setTimeout(resolve, 100));

    // Verify hook fired
    expect(events.length).toBeGreaterThan(0);
    expect(events[0].hook).toBe('test-hook');
    expect(events[0].event).toBe(EVENTS.AFTER_ADD_QUAD);

    unregister();
  });

  it('should handle predicate evaluation', async () => {
    // Register a hook with a COUNT predicate
    const unregister = registerHook(engine, {
      id: 'count-hook',
      events: [EVENTS.AFTER_ADD_QUAD],
      predicates: [
        {
          kind: 'COUNT',
          spec: {
            query: 'SELECT (COUNT(*) AS ?n) WHERE { ?s ?p ?o }',
            operator: '>=',
            value: 1
          }
        }
      ],
      action: async (payload, ok) => {
        events.push({
          hook: 'count-hook',
          allowed: ok,
          timestamp: new Date().toISOString()
        });
      }
    });

    // Add some data
    const commitData = {
      hash: 'def456',
      message: 'Test commit 2'
    };

    const commitRes = await ingress.fromJSON(
      SimpleCommit,
      JSON.stringify(commitData),
      { 
        base: 'http://ex/',
        subject: 'urn:test:commit:def456'
      }
    );

    writeWithProv(engine, commitRes.rdf, 'test', {
      operation: 'test',
      user: 'test-user'
    });

    // Wait for async processing
    await new Promise(resolve => setTimeout(resolve, 100));

    // Verify hook fired with predicate evaluation
    expect(events.length).toBeGreaterThan(0);
    expect(events[0].hook).toBe('count-hook');
    expect(events[0].allowed).toBe(true);

    unregister();
  });

  it('should handle egress operations', async () => {
    // Disable events to avoid interference from other tests
    engine.setEventEnabled(false);
    
    // Add some data
    const commitData = {
      hash: 'export123',
      message: 'Export test commit'
    };

    const commitRes = await ingress.fromJSON(
      SimpleCommit,
      JSON.stringify(commitData),
      { 
        base: 'http://ex/',
        subject: 'urn:test:commit:export123'
      }
    );

    writeWithProv(engine, commitRes.rdf, 'test', {
      operation: 'test',
      user: 'test-user'
    });

    // Debug: Check what's in the store
    const allQuads = engine.store.getQuads(null, null, null, null);
    console.log('All quads in store:', allQuads.length);
    allQuads.forEach(q => {
      console.log(`  ${q.subject.value} ${q.predicate.value} ${q.object.value}`);
    });

    // Export as JSON
    const jsonExport = await egress.toJSON(
      SimpleCommit,
      engine.store,
      { subject: 'urn:test:commit:export123' }
    );

    expect(jsonExport.output).toContain('export123');
    expect(jsonExport.output).toContain('Export test commit');

    // Export as Turtle
    const turtleExport = await egress.toTurtle(
      null,
      engine.store,
      engine
    );

    expect(turtleExport.output).toContain('urn:test:commit:export123');
    expect(turtleExport.output).toContain('export123');
  });

  it('should handle before hooks with veto capability', async () => {
    let vetoed = false;

    // Register a before hook that can veto
    const unregister = registerHook(engine, {
      id: 'veto-hook',
      events: [EVENTS.BEFORE_ADD_QUAD],
      action: async (payload, ok) => {
        events.push({
          hook: 'veto-hook',
          event: payload.event,
          allowed: ok,
          timestamp: new Date().toISOString()
        });
        
        // Veto the operation
        throw new Error('Operation vetoed by hook');
      }
    });

    // Try to add data
    const commitData = {
      hash: 'veto123',
      message: 'This should be vetoed'
    };

    const commitRes = await ingress.fromJSON(
      SimpleCommit,
      JSON.stringify(commitData),
      { 
        base: 'http://ex/',
        subject: 'urn:test:commit:veto123'
      }
    );

    // This should not throw because the hook error is caught
    writeWithProv(engine, commitRes.rdf, 'test', {
      operation: 'test',
      user: 'test-user'
    });

    // Wait for async processing
    await new Promise(resolve => setTimeout(resolve, 100));

    // Verify hook fired
    expect(events.length).toBeGreaterThan(0);
    expect(events[0].hook).toBe('veto-hook');

    unregister();
  });

  it('should handle multiple hooks on same event', async () => {
    // Register multiple hooks
    const unregister1 = registerHook(engine, {
      id: 'hook-1',
      events: [EVENTS.AFTER_ADD_QUAD],
      action: async (payload, ok) => {
        events.push({ hook: 'hook-1', timestamp: new Date().toISOString() });
      }
    });

    const unregister2 = registerHook(engine, {
      id: 'hook-2',
      events: [EVENTS.AFTER_ADD_QUAD],
      action: async (payload, ok) => {
        events.push({ hook: 'hook-2', timestamp: new Date().toISOString() });
      }
    });

    // Add some data
    const commitData = {
      hash: 'multi123',
      message: 'Multi hook test'
    };

    const commitRes = await ingress.fromJSON(
      SimpleCommit,
      JSON.stringify(commitData),
      { 
        base: 'http://ex/',
        subject: 'urn:test:commit:multi123'
      }
    );

    writeWithProv(engine, commitRes.rdf, 'test', {
      operation: 'test',
      user: 'test-user'
    });

    // Wait for async processing
    await new Promise(resolve => setTimeout(resolve, 100));

    // Verify both hooks fired
    const hook1Events = events.filter(e => e.hook === 'hook-1');
    const hook2Events = events.filter(e => e.hook === 'hook-2');
    
    expect(hook1Events.length).toBeGreaterThan(0);
    expect(hook2Events.length).toBeGreaterThan(0);

    unregister1();
    unregister2();
  });

  it('should handle provenance tracking', async () => {
    // Disable events to avoid interference from other tests
    engine.setEventEnabled(false);
    
    // Add some data with provenance
    const commitData = {
      hash: 'prov123',
      message: 'Provenance test'
    };

    const commitRes = await ingress.fromJSON(
      SimpleCommit,
      JSON.stringify(commitData),
      { 
        base: 'http://ex/',
        subject: 'urn:test:commit:prov123'
      }
    );

    writeWithProv(engine, commitRes.rdf, 'test-source', {
      operation: 'provenance-test',
      user: 'test-user'
    });

    // Check for provenance triples
    const provenanceQuads = engine.store.getQuads(
      null,
      engine.namedNode('http://ex/prov#source'),
      null,
      null
    );

    expect(provenanceQuads.length).toBeGreaterThan(0);
    
    const sources = provenanceQuads.map(q => q.object.value);
    expect(sources).toContain('test-source');
  });
});
