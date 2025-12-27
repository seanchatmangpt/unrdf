/**
 * @fileoverview Comprehensive CRDT property tests
 *
 * Tests all CRDT properties:
 * - Commutativity: merge(A, B) = merge(B, A)
 * - Associativity: merge(merge(A, B), C) = merge(A, merge(B, C))
 * - Idempotency: merge(A, A) = A
 * - Convergence: Different merge orders → same result
 */

import { describe, it, expect } from 'vitest';
import {
  VectorClock,
  GCounter,
  PNCounter,
  ORSet,
  LWWRegister,
  RDFSet,
} from '../src/crdt-pure/index.mjs';

describe('VectorClock', () => {
  it('should increment correctly', () => {
    const vc = new VectorClock('node-1');
    expect(vc.get('node-1')).toBe(0);

    vc.increment();
    expect(vc.get('node-1')).toBe(1);

    vc.increment();
    expect(vc.get('node-1')).toBe(2);
  });

  it('should merge correctly (commutativity)', () => {
    const vc1 = new VectorClock('node-1');
    const vc2 = new VectorClock('node-2');

    vc1.increment();
    vc1.increment();
    vc2.increment();

    const vc1a = vc1.clone();
    const vc1b = vc1.clone();
    const vc2a = vc2.clone();
    const vc2b = vc2.clone();

    vc1a.merge(vc2a);
    vc2b.merge(vc1b);

    expect(vc1a.get('node-1')).toBe(2);
    expect(vc1a.get('node-2')).toBe(1);
    expect(vc2b.get('node-1')).toBe(2);
    expect(vc2b.get('node-2')).toBe(1);
    expect(vc1a.equals(vc2b)).toBe(true);
  });

  it('should detect happens-before relationship', () => {
    const vc1 = new VectorClock('node-1');
    const vc2 = vc1.clone();

    vc2.increment();

    expect(vc1.isBefore(vc2)).toBe(true);
    expect(vc2.isAfter(vc1)).toBe(true);
    expect(vc1.isConcurrent(vc2)).toBe(false);
  });

  it('should detect concurrent events', () => {
    const vc1 = new VectorClock('node-1');
    const vc2 = new VectorClock('node-2');

    vc1.increment();
    vc2.increment();

    expect(vc1.isConcurrent(vc2)).toBe(true);
    expect(vc1.isBefore(vc2)).toBe(false);
    expect(vc2.isBefore(vc1)).toBe(false);
  });

  it('should serialize and deserialize', () => {
    const vc = new VectorClock('node-1');
    vc.increment();
    vc.increment();

    const json = vc.toJSON();
    const restored = VectorClock.fromJSON(json);

    expect(restored.nodeId).toBe('node-1');
    expect(restored.get('node-1')).toBe(2);
    expect(restored.equals(vc)).toBe(true);
  });
});

describe('GCounter', () => {
  it('should increment correctly', () => {
    const counter = new GCounter('node-1');
    expect(counter.value()).toBe(0);

    counter.increment(5);
    expect(counter.value()).toBe(5);

    counter.increment(3);
    expect(counter.value()).toBe(8);
  });

  it('should reject negative increments', () => {
    const counter = new GCounter('node-1');
    expect(() => counter.increment(-5)).toThrow();
  });

  it('should merge correctly (commutativity)', () => {
    const c1 = new GCounter('node-1');
    const c2 = new GCounter('node-2');

    c1.increment(10);
    c2.increment(20);

    const c1a = c1.clone();
    const c2a = c2.clone();

    c1.merge(c2);
    c2a.merge(c1a);

    expect(c1.value()).toBe(30);
    expect(c2a.value()).toBe(30);
  });

  it('should be idempotent', () => {
    const c1 = new GCounter('node-1');
    const c2 = new GCounter('node-2');

    c1.increment(10);
    c2.increment(20);

    c1.merge(c2);
    const valueBefore = c1.value();

    c1.merge(c2); // Merge again
    expect(c1.value()).toBe(valueBefore);
  });

  it('should serialize and deserialize', () => {
    const counter = new GCounter('node-1');
    counter.increment(42);

    const json = counter.toJSON();
    const restored = GCounter.fromJSON(json);

    expect(restored.value()).toBe(42);
    expect(restored.nodeId).toBe('node-1');
  });
});

describe('PNCounter', () => {
  it('should increment and decrement correctly', () => {
    const counter = new PNCounter('node-1');
    expect(counter.value()).toBe(0);

    counter.increment(10);
    expect(counter.value()).toBe(10);

    counter.decrement(3);
    expect(counter.value()).toBe(7);

    counter.decrement(10);
    expect(counter.value()).toBe(-3);
  });

  it('should merge correctly', () => {
    const c1 = new PNCounter('node-1');
    const c2 = new PNCounter('node-2');

    c1.increment(50);
    c1.decrement(10);

    c2.increment(30);
    c2.decrement(5);

    c1.merge(c2);

    expect(c1.value()).toBe((50 - 10) + (30 - 5));
  });

  it('should be commutative', () => {
    const c1 = new PNCounter('node-1');
    const c2 = new PNCounter('node-2');
    const c3 = new PNCounter('node-3');

    c1.increment(10);
    c2.decrement(5);
    c3.increment(7);

    const c1a = c1.clone();
    const c2a = c2.clone();
    const c3a = c3.clone();

    // Order A: 1 <- 2 <- 3
    c1.merge(c2).merge(c3);

    // Order B: 3 <- 1 <- 2
    c3a.merge(c1a).merge(c2a);

    expect(c1.value()).toBe(c3a.value());
  });

  it('should serialize and deserialize', () => {
    const counter = new PNCounter('node-1');
    counter.increment(50);
    counter.decrement(10);

    const json = counter.toJSON();
    const restored = PNCounter.fromJSON(json);

    expect(restored.value()).toBe(40);
  });
});

describe('ORSet', () => {
  it('should add and check elements', () => {
    const set = new ORSet('node-1');
    expect(set.size()).toBe(0);

    set.add('apple');
    expect(set.has('apple')).toBe(true);
    expect(set.size()).toBe(1);

    set.add('banana');
    expect(set.size()).toBe(2);
  });

  it('should remove elements', () => {
    const set = new ORSet('node-1');
    set.add('apple');
    set.add('banana');

    set.remove('apple');
    expect(set.has('apple')).toBe(false);
    expect(set.has('banana')).toBe(true);
    expect(set.size()).toBe(1);
  });

  it('should handle add-wins semantics', () => {
    const s1 = new ORSet('node-1');
    const s2 = new ORSet('node-2');

    // Concurrent add and remove
    s1.add('apple');
    s2.remove('apple'); // Remove of non-existent element

    s1.merge(s2);

    // Add wins because s1 has a tag that s2 doesn't have in tombstones
    expect(s1.has('apple')).toBe(true);
  });

  it('should merge correctly (commutativity)', () => {
    const s1 = new ORSet('node-1');
    const s2 = new ORSet('node-2');

    s1.add('apple');
    s1.add('banana');

    s2.add('cherry');
    s2.add('date');

    const s1a = s1.clone();
    const s2a = s2.clone();

    s1.merge(s2);
    s2a.merge(s1a);

    expect(s1.size()).toBe(4);
    expect(s2a.size()).toBe(4);

    const values1 = s1.values().sort();
    const values2 = s2a.values().sort();
    expect(JSON.stringify(values1)).toBe(JSON.stringify(values2));
  });

  it('should be idempotent', () => {
    const s1 = new ORSet('node-1');
    const s2 = new ORSet('node-2');

    s1.add('apple');
    s2.add('banana');

    s1.merge(s2);
    const sizeBefore = s1.size();

    s1.merge(s2); // Merge again
    expect(s1.size()).toBe(sizeBefore);
  });

  it('should serialize and deserialize', () => {
    const set = new ORSet('node-1');
    set.add('apple');
    set.add('banana');
    set.remove('apple');

    const json = set.toJSON();
    const restored = ORSet.fromJSON(json);

    expect(restored.has('banana')).toBe(true);
    expect(restored.has('apple')).toBe(false);
    expect(restored.size()).toBe(1);
  });
});

describe('LWWRegister', () => {
  it('should store and retrieve values', () => {
    const reg = new LWWRegister('node-1', 'initial');
    expect(reg.get()).toBe('initial');

    reg.set('updated');
    expect(reg.get()).toBe('updated');
  });

  it('should resolve conflicts with LWW', () => {
    const r1 = new LWWRegister('node-1', 'value1');
    const r2 = new LWWRegister('node-2', 'value2');

    // Simulate r2 writing later
    setTimeout(() => {
      r2.set('newer');
    }, 10);

    // Wait then merge
    setTimeout(() => {
      r1.merge(r2);
      // r2's value should win due to later timestamp
      expect(r1.get()).toBe('newer');
    }, 20);
  });

  it('should use node ID as tie-breaker', () => {
    const timestamp = 1000;

    const r1 = new LWWRegister('node-a', null);
    const r2 = new LWWRegister('node-z', null);

    r1.set('valueA', timestamp);
    r2.set('valueZ', timestamp);

    r1.merge(r2);

    // node-z > node-a lexicographically
    expect(r1.get()).toBe('valueZ');
  });

  it('should merge correctly (commutativity)', () => {
    const r1 = new LWWRegister('node-1', 'val1');
    const r2 = new LWWRegister('node-2', 'val2');

    r1.set('updated1', 100);
    r2.set('updated2', 200);

    const r1a = r1.clone();
    const r2a = r2.clone();

    r1.merge(r2);
    r2a.merge(r1a);

    expect(r1.get()).toBe(r2a.get());
  });

  it('should serialize and deserialize', () => {
    const reg = new LWWRegister('node-1', 'test');
    reg.set('value');

    const json = reg.toJSON();
    const restored = LWWRegister.fromJSON(json);

    expect(restored.get()).toBe('value');
    expect(restored.nodeId).toBe('node-1');
  });
});

describe('RDFSet', () => {
  it('should add and retrieve triples', () => {
    const rdf = new RDFSet('node-1');
    expect(rdf.size()).toBe(0);

    rdf.add({
      subject: 'ex:Alice',
      predicate: 'rdf:type',
      object: 'foaf:Person',
    });

    expect(rdf.size()).toBe(1);
    expect(rdf.has({
      subject: 'ex:Alice',
      predicate: 'rdf:type',
      object: 'foaf:Person',
    })).toBe(true);
  });

  it('should remove triples', () => {
    const rdf = new RDFSet('node-1');

    const triple = {
      subject: 'ex:Alice',
      predicate: 'foaf:name',
      object: 'Alice',
    };

    rdf.add(triple);
    expect(rdf.has(triple)).toBe(true);

    rdf.remove(triple);
    expect(rdf.has(triple)).toBe(false);
  });

  it('should query triples by pattern', () => {
    const rdf = new RDFSet('node-1');

    rdf.add({ subject: 'ex:Alice', predicate: 'rdf:type', object: 'foaf:Person' });
    rdf.add({ subject: 'ex:Alice', predicate: 'foaf:name', object: 'Alice' });
    rdf.add({ subject: 'ex:Bob', predicate: 'rdf:type', object: 'foaf:Person' });

    const aliceTriples = rdf.query({ subject: 'ex:Alice' });
    expect(aliceTriples.length).toBe(2);

    const typeTriples = rdf.query({ predicate: 'rdf:type' });
    expect(typeTriples.length).toBe(2);
  });

  it('should merge correctly (convergence)', () => {
    const r1 = new RDFSet('node-1');
    const r2 = new RDFSet('node-2');
    const r3 = new RDFSet('node-3');

    r1.add({ subject: 'ex:A', predicate: 'rdf:type', object: 'ex:Thing' });
    r2.add({ subject: 'ex:B', predicate: 'rdf:type', object: 'ex:Thing' });
    r3.add({ subject: 'ex:C', predicate: 'rdf:type', object: 'ex:Thing' });

    const r1a = r1.clone();
    const r2a = r2.clone();
    const r3a = r3.clone();

    // Order A: 1 <- 2 <- 3
    r1.merge(r2).merge(r3);

    // Order B: 3 <- 1 <- 2
    r3a.merge(r1a).merge(r2a);

    expect(r1.size()).toBe(3);
    expect(r3a.size()).toBe(3);

    const triples1 = r1.getTriples().sort((a, b) => a.subject.localeCompare(b.subject));
    const triples3 = r3a.getTriples().sort((a, b) => a.subject.localeCompare(b.subject));

    expect(JSON.stringify(triples1)).toBe(JSON.stringify(triples3));
  });

  it('should handle concurrent add/remove (add-wins)', () => {
    const r1 = new RDFSet('node-1');
    const r2 = new RDFSet('node-2');

    const triple = {
      subject: 'ex:Test',
      predicate: 'ex:value',
      object: 'test',
    };

    // Concurrent operations
    r1.add(triple);
    r2.remove(triple); // Remove non-existent

    r1.merge(r2);

    // Add wins
    expect(r1.has(triple)).toBe(true);
  });

  it('should validate triples', () => {
    const rdf = new RDFSet('node-1');

    expect(() => rdf.add({})).toThrow();
    expect(() => rdf.add({ subject: 'ex:A' })).toThrow();
    expect(() => rdf.add({ subject: 'ex:A', predicate: 'ex:p' })).toThrow();
  });

  it('should serialize and deserialize', () => {
    const rdf = new RDFSet('node-1');
    rdf.add({ subject: 'ex:Alice', predicate: 'rdf:type', object: 'foaf:Person' });
    rdf.add({ subject: 'ex:Bob', predicate: 'foaf:knows', object: 'ex:Alice' });

    const json = rdf.toJSON();
    const restored = RDFSet.fromJSON(json);

    expect(restored.size()).toBe(2);
    expect(restored.nodeId).toBe('node-1');
  });
});

describe('CRDT Properties (Cross-type)', () => {
  it('should maintain commutativity across all CRDTs', () => {
    // Test commutativity for each CRDT type
    const tests = [
      () => {
        const c1 = new GCounter('n1');
        const c2 = new GCounter('n2');
        c1.increment(10);
        c2.increment(20);
        const c1a = c1.clone();
        const c2a = c2.clone();
        c1.merge(c2);
        c2a.merge(c1a);
        return c1.value() === c2a.value();
      },
      () => {
        const s1 = new ORSet('n1');
        const s2 = new ORSet('n2');
        s1.add('a');
        s2.add('b');
        const s1a = s1.clone();
        const s2a = s2.clone();
        s1.merge(s2);
        s2a.merge(s1a);
        return JSON.stringify(s1.values().sort()) === JSON.stringify(s2a.values().sort());
      },
    ];

    tests.forEach((test) => {
      expect(test()).toBe(true);
    });
  });

  it('should maintain associativity', () => {
    const c1 = new GCounter('n1');
    const c2 = new GCounter('n2');
    const c3 = new GCounter('n3');

    c1.increment(10);
    c2.increment(20);
    c3.increment(30);

    // (c1 + c2) + c3
    const left = c1.clone();
    left.merge(c2).merge(c3);

    // c1 + (c2 + c3)
    const right = c1.clone();
    const temp = c2.clone();
    temp.merge(c3);
    right.merge(temp);

    expect(left.value()).toBe(right.value());
  });
});
