/**
 * @file Tests for thing module
 */

import { describe, it, expect } from 'vitest';
import { createThing } from '../src/thing.mjs';

describe('createThing', () => {
  it('creates thing with default options', () => {
    const thing = createThing();
    expect(thing).toBeDefined();
    expect(thing.option).toBe('default');
  });

  it('creates thing with custom options', () => {
    const thing = createThing({ option: 'custom' });
    expect(thing.option).toBe('custom');
  });

  it('throws on invalid options', () => {
    expect(() => createThing({ option: 123 })).toThrow(TypeError);
  });
});

describe('thing.doSomething', () => {
  it('does something', () => {
    const thing = createThing();
    expect(() => thing.doSomething()).not.toThrow();
  });
});
