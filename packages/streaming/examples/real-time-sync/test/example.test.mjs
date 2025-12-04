// @ts-check
// @vitest-environment node

import { describe, it, expect } from 'vitest';
import { Store, DataFactory } from 'n3';
import { createSubscriptionManager, createChangeFeed } from '@unrdf/streaming';
import {
  basicSubscription,
  realTimeMonitoring,
  concurrentUpdates,
  multiPeerSync,
  subscriptionPatterns
} from '../src/index.mjs';

const { namedNode, literal, quad } = DataFactory;

describe('Real-Time Sync Examples', () => {
  it('should manage subscriptions', async () => {
    const store = new Store();
    const manager = createSubscriptionManager(store);

    const notifications = [];
    const alice = namedNode('http://example.org/alice');
    const name = namedNode('http://xmlns.com/foaf/0.1/name');

    const id = manager.subscribe({
      subject: alice,
      predicate: name
    }, (quads) => {
      notifications.push(quads);
    });

    store.addQuad(quad(alice, name, literal('Alice')));
    await new Promise(resolve => setTimeout(resolve, 50));

    expect(notifications.length).toBeGreaterThan(0);
    expect(typeof id).toBe('string');

    manager.unsubscribe(id);
  });

  it('should monitor graph changes in real-time', async () => {
    const store = new Store();
    const manager = createSubscriptionManager(store);

    const changes = [];
    manager.subscribe({}, (quads) => {
      changes.push(quads);
    });

    const alice = namedNode('http://example.org/alice');
    const name = namedNode('http://xmlns.com/foaf/0.1/name');

    store.addQuad(quad(alice, name, literal('Alice')));
    store.addQuad(quad(alice, name, literal('Alice Smith')));

    await new Promise(resolve => setTimeout(resolve, 50));

    expect(changes.length).toBeGreaterThan(0);
  });

  it('should handle concurrent updates', async () => {
    const store = new Store();
    const manager = createSubscriptionManager(store);

    const alice = namedNode('http://example.org/alice');
    const age = namedNode('http://xmlns.com/foaf/0.1/age');

    let conflictDetected = false;

    manager.subscribe({
      subject: alice,
      predicate: age
    }, (quads) => {
      if (quads.length > 1) {
        conflictDetected = true;
      }
    });

    store.addQuad(quad(alice, age, literal('30', namedNode('http://www.w3.org/2001/XMLSchema#integer'))));
    store.addQuad(quad(alice, age, literal('31', namedNode('http://www.w3.org/2001/XMLSchema#integer'))));

    await new Promise(resolve => setTimeout(resolve, 50));

    const ageQuads = store.getQuads(alice, age, null, null);
    expect(ageQuads.length).toBeGreaterThan(1);
  });

  it('should sync across multiple peers', async () => {
    const peer1 = new Store();
    const peer2 = new Store();

    const feed1 = createChangeFeed(peer1);

    feed1.subscribe((change) => {
      if (change.type === 'add') {
        peer2.addQuad(change.quad);
      }
    });

    const alice = namedNode('http://example.org/alice');
    const name = namedNode('http://xmlns.com/foaf/0.1/name');

    peer1.addQuad(quad(alice, name, literal('Alice')));
    await new Promise(resolve => setTimeout(resolve, 100));

    expect(peer2.size).toBe(peer1.size);
  });

  it('should support subscription patterns', async () => {
    const store = new Store();
    const manager = createSubscriptionManager(store);

    const alice = namedNode('http://example.org/alice');
    const name = namedNode('http://xmlns.com/foaf/0.1/name');

    const aliceUpdates = [];
    const nameUpdates = [];

    manager.subscribe({ subject: alice }, (quads) => {
      aliceUpdates.push(quads);
    });

    manager.subscribe({ predicate: name }, (quads) => {
      nameUpdates.push(quads);
    });

    store.addQuad(quad(alice, name, literal('Alice')));
    await new Promise(resolve => setTimeout(resolve, 50));

    expect(aliceUpdates.length).toBeGreaterThan(0);
    expect(nameUpdates.length).toBeGreaterThan(0);
  });

  it('should prevent circular updates in peer sync', async () => {
    const peer1 = new Store();
    const peer2 = new Store();

    const feed1 = createChangeFeed(peer1);
    const feed2 = createChangeFeed(peer2);

    // Bidirectional sync
    feed1.subscribe((change) => {
      if (change.type === 'add' && !peer2.has(change.quad)) {
        peer2.addQuad(change.quad);
      }
    });

    feed2.subscribe((change) => {
      if (change.type === 'add' && !peer1.has(change.quad)) {
        peer1.addQuad(change.quad);
      }
    });

    const alice = namedNode('http://example.org/alice');
    const name = namedNode('http://xmlns.com/foaf/0.1/name');

    peer1.addQuad(quad(alice, name, literal('Alice')));
    await new Promise(resolve => setTimeout(resolve, 100));

    expect(peer1.size).toBe(1);
    expect(peer2.size).toBe(1);
  });

  it('should run basicSubscription example', async () => {
    await expect(basicSubscription()).resolves.toBeUndefined();
  });

  it('should run realTimeMonitoring example', async () => {
    await expect(realTimeMonitoring()).resolves.toBeUndefined();
  });

  it('should run concurrentUpdates example', async () => {
    await expect(concurrentUpdates()).resolves.toBeUndefined();
  });

  it('should run multiPeerSync example', async () => {
    await expect(multiPeerSync()).resolves.toBeUndefined();
  });

  it('should run subscriptionPatterns example', async () => {
    await expect(subscriptionPatterns()).resolves.toBeUndefined();
  });
});
