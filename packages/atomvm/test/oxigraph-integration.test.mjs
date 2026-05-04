import { describe, it, expect, beforeEach } from 'vitest';
import {
  createOxigraphBridge,
  dataFactory,
} from '../src/oxigraph-integration.mjs';

const { namedNode, literal } = dataFactory;

describe('Oxigraph Integration', () => {
  let bridge;

  beforeEach(() => {
    bridge = createOxigraphBridge();
  });

  it('completes triple add/query roundtrip', async () => {
    const triple = {
      subject: namedNode('http://s'),
      predicate: namedNode('http://p'),
      object: literal('o'),
    };

    await bridge.addTriples([triple]);
    const res = await bridge.queryPattern(triple.subject, null, null);

    expect(res).toHaveLength(1);
    expect(res[0].object.value).toBe('o');
  });

  it('supports deletion', async () => {
    const triple = {
      subject: namedNode('http://s'),
      predicate: namedNode('http://p'),
      object: literal('o'),
    };

    await bridge.addTriples([triple]);
    await bridge.removeTriples([triple]);
    const res = await bridge.getAllTriples();
    expect(res).toHaveLength(0);
  });
});
