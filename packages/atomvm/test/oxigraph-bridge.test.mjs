import { describe, it, expect, beforeEach } from 'vitest';
import { OxigraphBridge } from '../src/oxigraph-bridge.mjs';
import { Store, dataFactory } from '@unrdf/oxigraph';

describe('OxigraphBridge', () => {
  let store, bridge;

  beforeEach(() => {
    store = new Store();
    bridge = new OxigraphBridge(store);
  });

  it('initializes in Ready state', () => {
    expect(bridge.isReady()).toBe(true);
  });

  it('adds triples', async () => {
    const triple = dataFactory.quad(
      dataFactory.namedNode('http://example.org/s'),
      dataFactory.namedNode('http://example.org/p'),
      dataFactory.literal('o')
    );
    const res = await bridge.addTriples([triple]);
    expect(res.success).toBe(true);
    
    const count = [...store.match(null, null, null)].length;
    expect(count).toBe(1);
  });

  it('queries by pattern', async () => {
    const triple = dataFactory.quad(
      dataFactory.namedNode('http://example.org/s'),
      dataFactory.namedNode('http://example.org/p'),
      dataFactory.literal('o')
    );
    await bridge.addTriples([triple]);
    
    const res = await bridge.queryPattern(dataFactory.namedNode('http://example.org/s'), null, null);
    expect(res).toHaveLength(1);
    expect(res[0].subject.value).toBe('http://example.org/s');
  });

  it('removes triples', async () => {
    const triple = dataFactory.quad(
      dataFactory.namedNode('http://example.org/s'),
      dataFactory.namedNode('http://example.org/p'),
      dataFactory.literal('o')
    );
    await bridge.addTriples([triple]);
    await bridge.removeTriples([triple]);
    
    const count = [...store.match(null, null, null)].length;
    expect(count).toBe(0);
  });
});
