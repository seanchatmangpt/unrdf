/**
 * @fileoverview Minimal Core Tests - Happy Path
 * 
 * Tests for the minimal, AI-free core system:
 * - Events fire in order: beforeAddQuad → afterAddQuad; beforeSerialize → afterSerialize
 * - Hook can block serialize/update
 * - Ingress JSON→RDF then egress RDF→JSON round-trips under schema
 * - Provenance triples appear for every subject written
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
import { z } from "zod";

describe('Minimal Core System', () => {
  let engine;

  beforeEach(() => {
    engine = new RdfEngine({ eventsEnabled: true });
  });

  describe('Event Order', () => {
    it('should fire events in correct order: beforeAddQuad → afterAddQuad', async () => {
      const events = [];
      
      const unregister = registerHook(engine, {
        id: 'event-order-test',
        events: ['beforeAddQuad', 'afterAddQuad'],
        action: (payload) => {
          events.push(payload.event);
        }
      });

      engine.store.addQuad(
        engine.namedNode('http://example.org/test'),
        engine.namedNode('http://example.org/type'),
        engine.literal('Test')
      );

      // Wait for async events
      await new Promise(resolve => setTimeout(resolve, 100));

      expect(events).toEqual(['beforeAddQuad', 'afterAddQuad']);
      
      unregister();
    });

    it('should fire events in correct order: beforeSerialize → afterSerialize', async () => {
      const events = [];
      
      const unregister = registerHook(engine, {
        id: 'serialize-order-test',
        events: ['beforeSerialize', 'afterSerialize'],
        action: (payload) => {
          events.push(payload.event);
        }
      });

      // Add some data first
      engine.store.addQuad(
        engine.namedNode('http://example.org/test'),
        engine.namedNode('http://example.org/type'),
        engine.literal('Test')
      );

      await engine.serializeTurtle();

      // Wait for async events
      await new Promise(resolve => setTimeout(resolve, 100));

      expect(events).toEqual(['beforeSerialize', 'afterSerialize']);
      
      unregister();
    });
  });

  describe('Hook Blocking', () => {
    it('should block serialize when hook condition fails', async () => {
      const unregister = registerHook(engine, {
        id: 'block-serialize',
        events: ['beforeSerialize'],
        predicates: [{ 
          kind: 'COUNT', 
          spec: { 
            query: 'SELECT (COUNT(*) AS ?n) WHERE { ?s ?p ?o }', 
            operator: '>=', 
            value: 1 
          } 
        }],
        action: (_, ok) => { 
          if (!ok) throw new Error('Export blocked: empty graph'); 
        }
      });

      // Try to serialize empty store
      await expect(engine.serializeTurtle()).rejects.toThrow('Export blocked: empty graph');
      
      unregister();
    });

    it('should allow serialize when hook condition passes', async () => {
      const unregister = registerHook(engine, {
        id: 'allow-serialize',
        events: ['beforeSerialize'],
        predicates: [{ 
          kind: 'COUNT', 
          spec: { 
            query: 'SELECT (COUNT(*) AS ?n) WHERE { ?s ?p ?o }', 
            operator: '>=', 
            value: 1 
          } 
        }],
        action: (_, ok) => { 
          if (!ok) throw new Error('Export blocked: empty graph'); 
        }
      });

      // Add some data
      engine.store.addQuad(
        engine.namedNode('http://example.org/test'),
        engine.namedNode('http://example.org/type'),
        engine.literal('Test')
      );

      // Should not throw
      await expect(engine.serializeTurtle()).resolves.toBeDefined();
      
      unregister();
    });

    it('should block SPARQL UPDATE when hook condition fails', async () => {
      const unregister = registerHook(engine, {
        id: 'block-update',
        events: ['beforeUpdate'],
        predicates: [{ 
          kind: 'ASK', 
          spec: { 
            query: 'ASK WHERE { ?s ?p ?o }' 
          } 
        }],
        action: (_, ok) => { 
          if (!ok) throw new Error('Update blocked: no existing data'); 
        }
      });

      // Try to update empty store
      await expect(engine.query(`
        INSERT { <http://example.org/test> <http://example.org/type> "Test" }
        WHERE { }
      `)).rejects.toThrow('Update blocked: no existing data');
      
      unregister();
    });
  });

  describe('Ingress/Egress Round-trip', () => {
    it('should round-trip JSON → RDF → JSON under schema', async () => {
      const User = z.object({ 
        name: z.string(), 
        age: z.number() 
      });
      
      const originalJson = '{"name":"Ada","age":36}';
      
      // Ingress: JSON → RDF
      const inRes = await ingress.fromJSON(
        User, 
        originalJson,
        { subject: 'urn:ingress:item' }
      );
      
      writeWithProv(engine, inRes.rdf, 'json');
      
      // Egress: RDF → JSON
      const outRes = await egress.toJSON(
        User, 
        engine.store, 
        { subject: 'urn:ingress:item' }
      );
      
      // Parse and compare
      const original = JSON.parse(originalJson);
      const roundtripped = JSON.parse(outRes.output);
      
      expect(roundtripped).toEqual(original);
    });

    it('should round-trip Turtle → RDF → Turtle', async () => {
      const originalTurtle = `
        @prefix ex: <http://example.org/> .
        ex:test ex:type "Test" .
        ex:test ex:value 42 .
      `;
      
      // Ingress: Turtle → RDF
      const inRes = await ingress.fromTurtle(
        null, // No schema for Turtle
        originalTurtle,
        { engine }
      );
      
      writeWithProv(engine, inRes.rdf, 'turtle');
      
      // Egress: RDF → Turtle
      const outRes = await egress.toTurtle(
        null, // No schema for Turtle
        engine.store, 
        engine
      );
      
      // Both should contain the same triples
      expect(outRes.output).toContain('ex:test');
      expect(outRes.output).toContain('ex:type');
      expect(outRes.output).toContain('ex:value');
    });
  });

  describe('Mandatory Provenance', () => {
    it('should add provenance triples for every subject written', async () => {
      const User = z.object({ 
        name: z.string(), 
        age: z.number() 
      });
      
      const inRes = await ingress.fromJSON(
        User, 
        '{"name":"Ada","age":36}',
        { subject: 'urn:ingress:item' }
      );
      
      writeWithProv(engine, inRes.rdf, 'json');
      
      // Check for provenance triples
      const sourceQuads = engine.store.getQuads(
        engine.namedNode('urn:ingress:item'), 
        engine.namedNode('http://ex/prov#source'), 
        null, 
        null
      );
      
      const timestampQuads = engine.store.getQuads(
        engine.namedNode('urn:ingress:item'), 
        engine.namedNode('http://ex/prov#ts'), 
        null, 
        null
      );
      
      expect(sourceQuads).toHaveLength(1);
      expect(sourceQuads[0].object.value).toBe('json');
      
      expect(timestampQuads).toHaveLength(1);
      expect(timestampQuads[0].object.value).toMatch(/^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}/);
    });

    it('should add provenance for multiple subjects', async () => {
      const quads = [
        engine.quad(
          engine.namedNode('urn:subject1'),
          engine.namedNode('http://example.org/type'),
          engine.literal('Type1')
        ),
        engine.quad(
          engine.namedNode('urn:subject2'),
          engine.namedNode('http://example.org/type'),
          engine.literal('Type2')
        )
      ];
      
      writeWithProv(engine, quads, 'test');
      
      // Check provenance for both subjects
      const prov1 = engine.store.getQuads(
        engine.namedNode('urn:subject1'), 
        engine.namedNode('http://ex/prov#source'), 
        null, 
        null
      );
      
      const prov2 = engine.store.getQuads(
        engine.namedNode('urn:subject2'), 
        engine.namedNode('http://ex/prov#source'), 
        null, 
        null
      );
      
      expect(prov1).toHaveLength(1);
      expect(prov2).toHaveLength(1);
      expect(prov1[0].object.value).toBe('test');
      expect(prov2[0].object.value).toBe('test');
    });

    it('should add metadata provenance when provided', async () => {
      const quads = [
        engine.quad(
          engine.namedNode('urn:test'),
          engine.namedNode('http://example.org/type'),
          engine.literal('Test')
        )
      ];
      
      writeWithProv(engine, quads, 'test', {
        operation: 'import',
        user: 'test-user',
        session: 'session-123'
      });
      
      // Check for metadata provenance
      const operationQuads = engine.store.getQuads(
        engine.namedNode('urn:test'), 
        engine.namedNode('http://ex/prov#operation'), 
        null, 
        null
      );
      
      const userQuads = engine.store.getQuads(
        engine.namedNode('urn:test'), 
        engine.namedNode('http://ex/prov#user'), 
        null, 
        null
      );
      
      const sessionQuads = engine.store.getQuads(
        engine.namedNode('urn:test'), 
        engine.namedNode('http://ex/prov#session'), 
        null, 
        null
      );
      
      expect(operationQuads).toHaveLength(1);
      expect(operationQuads[0].object.value).toBe('import');
      
      expect(userQuads).toHaveLength(1);
      expect(userQuads[0].object.value).toBe('test-user');
      
      expect(sessionQuads).toHaveLength(1);
      expect(sessionQuads[0].object.value).toBe('session-123');
    });
  });

  describe('Hook Predicates', () => {
    it('should evaluate COUNT predicate correctly', async () => {
      let hookFired = false;
      
      const unregister = registerHook(engine, {
        id: 'count-test',
        events: ['afterAddQuad'],
        predicates: [{ 
          kind: 'COUNT', 
          spec: { 
            query: 'SELECT (COUNT(*) AS ?n) WHERE { ?s ?p ?o }', 
            operator: '>=', 
            value: 2 
          } 
        }],
        action: (_, ok) => { 
          hookFired = ok;
        }
      });

      // Add first quad (count = 1, should not fire)
      engine.store.addQuad(
        engine.namedNode('http://example.org/test1'),
        engine.namedNode('http://example.org/type'),
        engine.literal('Test1')
      );

      await new Promise(resolve => setTimeout(resolve, 100));
      expect(hookFired).toBe(false);

      // Add second quad (count = 2, should fire)
      engine.store.addQuad(
        engine.namedNode('http://example.org/test2'),
        engine.namedNode('http://example.org/type'),
        engine.literal('Test2')
      );

      await new Promise(resolve => setTimeout(resolve, 100));
      expect(hookFired).toBe(true);
      
      unregister();
    });

    it('should evaluate ASK predicate correctly', async () => {
      let hookFired = false;
      
      const unregister = registerHook(engine, {
        id: 'ask-test',
        events: ['afterAddQuad'],
        predicates: [{ 
          kind: 'ASK', 
          spec: { 
            query: 'ASK WHERE { ?s <http://example.org/type> "Test" }' 
          } 
        }],
        action: (_, ok) => { 
          hookFired = ok;
        }
      });

      // Add quad that matches ASK query
      engine.store.addQuad(
        engine.namedNode('http://example.org/test'),
        engine.namedNode('http://example.org/type'),
        engine.literal('Test')
      );

      await new Promise(resolve => setTimeout(resolve, 100));
      expect(hookFired).toBe(true);
      
      unregister();
    });
  });
});
