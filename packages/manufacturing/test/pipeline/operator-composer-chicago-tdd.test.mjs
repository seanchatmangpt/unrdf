/**
 * @file Operator Composer Tests — Chicago TDD
 * @description DAG verification for operator composition
 *
 * Core Chicago TDD tests focused on DAG lie detection:
 * - Circular dependency detection
 * - Parallel group detection
 * - Dependency ordering verification
 */

import { describe, it, expect } from 'vitest';
import { OperatorComposer } from '../../src/pipeline/operator-composer.mjs';

describe('OperatorComposer — DAG Lie Detection', () => {

  it('detects circular dependency and throws', () => {
    const composer = new OperatorComposer();
    composer.add('a', {}, { id: 'a' });
    composer.add('b', {}, { id: 'b', dependsOn: ['a'] });
    composer.add('c', {}, { id: 'c', dependsOn: ['b'] });
    composer.add('d', {}, { id: 'd', dependsOn: ['c', 'a'] });  // OK - diamond dependency

    // Now create a cycle: add a step that depends on 'd' and make 'd' depend on it
    composer.add('e', {}, { id: 'e', dependsOn: ['d'] });
    // Manually modify to create cycle for testing
    const stepD = composer.getStep('d');
    stepD.dependsOn = ['e'];  // Now d depends on e, e depends on d → cycle!

    expect(() => composer.getExecutionOrder()).toThrow('Circular');
  });

  it('detects parallel groups correctly', () => {
    const composer = new OperatorComposer();
    composer.add('validate', {}, { id: 'v' });
    composer.add('filter', {}, { id: 'f', dependsOn: ['v'], parallel: true });
    composer.add('enrich', {}, { id: 'e', dependsOn: ['v'], parallel: true });
    composer.add('transform', {}, { id: 't', dependsOn: ['f', 'e'] });
    const order = composer.getExecutionOrder();
    expect(order).toEqual([['v'], ['f', 'e'], ['t']]);
  });

  it('handles diamond dependency correctly', () => {
    const composer = new OperatorComposer();
    composer.add('a', {}, { id: 'a' });
    composer.add('b', {}, { id: 'b', dependsOn: ['a'] });
    composer.add('c', {}, { id: 'c', dependsOn: ['a'] });
    composer.add('d', {}, { id: 'd', dependsOn: ['b', 'c'] });
    const order = composer.getExecutionOrder();
    // a must come first, then b and c (in any order), then d
    const flat = order.flat();
    expect(flat.indexOf('a')).toBeLessThan(flat.indexOf('b'));
    expect(flat.indexOf('a')).toBeLessThan(flat.indexOf('c'));
    expect(flat.indexOf('b')).toBeLessThan(flat.indexOf('d'));
    expect(flat.indexOf('c')).toBeLessThan(flat.indexOf('d'));
  });
});
