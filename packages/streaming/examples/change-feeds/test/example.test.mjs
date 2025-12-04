// @ts-check
// @vitest-environment node

import { describe, it, expect } from 'vitest';
import { Store, DataFactory } from 'n3';
import { createChangeFeed } from '@unrdf/streaming';
import {
  basicChangeFeed,
  changeHistory,
  filteredSubscription,
  timeBasedQueries
} from '../src/index.mjs';

const { namedNode, literal, quad } = DataFactory;

describe('Change Feeds Examples', () => {
  it('should create and subscribe to change feed', async () => {
    const store = new Store();
    const feed = createChangeFeed(store);

    const changes = [];
    const unsubscribe = feed.subscribe((change) => {
      changes.push(change);
    });

    const alice = namedNode('http://example.org/alice');
    const name = namedNode('http://xmlns.com/foaf/0.1/name');

    store.addQuad(quad(alice, name, literal('Alice')));
    await new Promise(resolve => setTimeout(resolve, 50));

    expect(changes.length).toBeGreaterThan(0);
    expect(changes[0].type).toBe('add');
    expect(changes[0].quad.subject.value).toBe('http://example.org/alice');

    unsubscribe();
  });

  it('should track change history', async () => {
    const store = new Store();
    const feed = createChangeFeed(store);

    const alice = namedNode('http://example.org/alice');
    const name = namedNode('http://xmlns.com/foaf/0.1/name');

    store.addQuad(quad(alice, name, literal('Alice')));
    store.addQuad(quad(alice, name, literal('Alice Smith')));

    await new Promise(resolve => setTimeout(resolve, 50));

    const history = feed.getHistory();
    expect(history.length).toBeGreaterThan(0);
    expect(history[0].timestamp).toBeTypeOf('number');
  });

  it('should replay changes to new store', async () => {
    const store = new Store();
    const feed = createChangeFeed(store);

    const alice = namedNode('http://example.org/alice');
    const name = namedNode('http://xmlns.com/foaf/0.1/name');

    store.addQuad(quad(alice, name, literal('Alice')));
    await new Promise(resolve => setTimeout(resolve, 50));

    const newStore = new Store();
    feed.replay((change) => {
      if (change.type === 'add') {
        newStore.addQuad(change.quad);
      }
    });

    expect(newStore.size).toBe(store.size);
  });

  it('should support filtered subscriptions', async () => {
    const store = new Store();
    const feed = createChangeFeed(store);

    const additions = [];
    const unsubscribe = feed.subscribe((change) => {
      if (change.type === 'add') {
        additions.push(change);
      }
    });

    const alice = namedNode('http://example.org/alice');
    const name = namedNode('http://xmlns.com/foaf/0.1/name');

    store.addQuad(quad(alice, name, literal('Alice')));
    store.removeQuad(quad(alice, name, literal('Alice')));

    await new Promise(resolve => setTimeout(resolve, 50));

    expect(additions.length).toBe(1);
    expect(additions[0].type).toBe('add');

    unsubscribe();
  });

  it('should support time-based queries', async () => {
    const store = new Store();
    const feed = createChangeFeed(store);

    const alice = namedNode('http://example.org/alice');
    const name = namedNode('http://xmlns.com/foaf/0.1/name');

    store.addQuad(quad(alice, name, literal('Alice')));
    await new Promise(resolve => setTimeout(resolve, 50));

    const midpoint = Date.now();
    await new Promise(resolve => setTimeout(resolve, 50));

    store.addQuad(quad(alice, name, literal('Alice Smith')));
    await new Promise(resolve => setTimeout(resolve, 50));

    const allChanges = feed.getHistory();
    const recentChanges = feed.getHistory({ since: midpoint });

    expect(allChanges.length).toBeGreaterThan(recentChanges.length);
  });

  it('should run basicChangeFeed example', async () => {
    await expect(basicChangeFeed()).resolves.toBeUndefined();
  });

  it('should run changeHistory example', async () => {
    await expect(changeHistory()).resolves.toBeUndefined();
  });

  it('should run filteredSubscription example', async () => {
    await expect(filteredSubscription()).resolves.toBeUndefined();
  });

  it('should run timeBasedQueries example', async () => {
    await expect(timeBasedQueries()).resolves.toBeUndefined();
  });
});
