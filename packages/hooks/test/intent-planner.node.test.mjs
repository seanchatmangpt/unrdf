import assert from 'node:assert/strict';
import { test } from 'node:test';
import { createIntentPlanner, evaluateCondition } from '../src/hooks/intent-planner.mjs';

test('evaluateCondition supports comparisons and nested paths', () => {
  const context = { event: { score: 7, tags: ['ready', 'verified'] } };
  assert.equal(evaluateCondition({ path: 'event.score', op: 'gte', value: 7 }, context), true);
  assert.equal(evaluateCondition({ path: 'event.tags', op: 'contains', value: 'ready' }, context), true);
  assert.equal(evaluateCondition({ path: 'event.score', op: 'lt', value: 7 }, context), false);
});

test('evaluateCondition supports all, any, and not', () => {
  const context = { state: 'ready', failures: 0 };
  assert.equal(evaluateCondition({
    all: [
      { path: 'state', op: 'eq', value: 'ready' },
      { not: { path: 'failures', op: 'gt', value: 0 } },
    ],
  }, context), true);
  assert.equal(evaluateCondition({
    any: [
      { path: 'state', op: 'eq', value: 'blocked' },
      { path: 'failures', op: 'eq', value: 0 },
    ],
  }, context), true);
});

test('evaluateCondition supports existence, membership, and regex matching', () => {
  const context = { actor: { id: 'agent-42' }, mode: 'verify' };
  assert.equal(evaluateCondition({ path: 'actor.id', op: 'exists' }, context), true);
  assert.equal(evaluateCondition({ path: 'mode', op: 'in', value: ['verify', 'repair'] }, context), true);
  assert.equal(evaluateCondition({ path: 'actor.id', op: 'matches', value: '^agent-\\d+$' }, context), true);
});

test('evaluateCondition refuses unsafe regexes and unsupported operators', () => {
  assert.throws(
    () => evaluateCondition({ path: 'x', op: 'matches', value: '(a+)+$' }, { x: 'a' }),
    /catastrophic/,
  );
  assert.throws(
    () => evaluateCondition({ path: 'x', op: 'execute', value: true }, { x: true }),
    /Unsupported/,
  );
});

const baseRules = [
  {
    id: 'build',
    priority: 10,
    when: { path: 'event.type', op: 'eq', value: 'source.changed' },
    intent: {
      id: 'build-intent',
      type: 'BuildPackage',
      authority: 'ci/build',
      target: { $context: 'event.package' },
      parameters: { sha: { $context: 'event.sha' } },
      resources: ['workspace'],
      idempotencyKey: { $context: 'event.sha' },
    },
  },
  {
    id: 'notify',
    priority: 5,
    when: { path: 'event.type', op: 'eq', value: 'source.changed' },
    intent: {
      id: 'notify-intent',
      type: 'NotifyOwner',
      authority: 'notifications/write',
      parameters: { package: { $context: 'event.package' } },
      dependsOn: ['build-intent'],
    },
  },
];

test('planner manufactures intents from matching rules', () => {
  const plan = createIntentPlanner({ rules: baseRules }).plan({
    event: { type: 'source.changed', package: '@unrdf/core', sha: 'abc' },
  });
  assert.equal(plan.matchedRules.length, 2);
  assert.deepEqual(plan.intents.map(intent => intent.id), ['build-intent', 'notify-intent']);
  assert.equal(plan.intents[0].target, '@unrdf/core');
  assert.equal(plan.intents[0].parameters.sha, 'abc');
});

test('planner does not expose an execution or actuation method', () => {
  const planner = createIntentPlanner({ rules: baseRules });
  assert.equal(planner.execute, undefined);
  assert.equal(planner.actuate, undefined);
  assert.equal(typeof planner.plan, 'function');
});

test('planner omits unmatched rules', () => {
  const plan = createIntentPlanner({ rules: baseRules }).plan({ event: { type: 'timer' } });
  assert.equal(plan.intents.length, 0);
  assert.equal(plan.matchedRules.length, 0);
});

test('planner deduplicates idempotent intents', () => {
  const rules = [
    baseRules[0],
    { ...baseRules[0], id: 'build-again', priority: 9, intent: { ...baseRules[0].intent, id: 'build-2' } },
  ];
  const plan = createIntentPlanner({ rules }).plan({
    event: { type: 'source.changed', package: '@unrdf/core', sha: 'abc' },
  });
  assert.equal(plan.intents.length, 1);
  assert.equal(plan.deduplicated.length, 1);
  assert.equal(plan.deduplicated[0].duplicateOf, 'build-intent');
});

test('planner refuses resource conflicts by default', () => {
  const rules = [
    {
      id: 'a', priority: 5, intent: {
        id: 'a', type: 'Write', authority: 'fs/write', target: 'left', resources: ['file:x'],
      },
    },
    {
      id: 'b', priority: 4, intent: {
        id: 'b', type: 'Write', authority: 'fs/write', target: 'right', resources: ['file:x'],
      },
    },
  ];
  const plan = createIntentPlanner({ rules }).plan({});
  assert.deepEqual(plan.intents.map(intent => intent.id), ['a']);
  assert.deepEqual(plan.refused.map(item => item.reason), ['RESOURCE_CONFLICT']);
});

test('priority strategy preserves strongest conflicting intent', () => {
  const rules = [
    {
      id: 'low', priority: 1, intent: {
        id: 'low', type: 'Write', authority: 'fs/write', target: 'left', resources: ['file:x'],
      },
    },
    {
      id: 'high', priority: 10, intent: {
        id: 'high', type: 'Write', authority: 'fs/write', target: 'right', resources: ['file:x'],
      },
    },
  ];
  const plan = createIntentPlanner({ rules, conflictStrategy: 'priority' }).plan({});
  assert.deepEqual(plan.intents.map(intent => intent.id), ['high']);
  assert.equal(plan.refused[0].intent.id, 'low');
});

test('merge strategy combines compatible conflicting parameters', () => {
  const rules = [
    {
      id: 'a', intent: {
        id: 'a', type: 'Configure', authority: 'config/write', target: 'same',
        resources: ['config:x'], parameters: { alpha: 1, items: ['a'] },
      },
    },
    {
      id: 'b', intent: {
        id: 'b', type: 'Configure', authority: 'config/write', target: 'different',
        resources: ['config:x'], parameters: { beta: 2, items: ['b'] },
      },
    },
  ];
  const plan = createIntentPlanner({ rules, conflictStrategy: 'merge' }).plan({});
  assert.equal(plan.intents.length, 1);
  assert.deepEqual(plan.intents[0].parameters, { alpha: 1, beta: 2, items: ['a', 'b'] });
});

test('merge strategy refuses incompatible scalar values', () => {
  const rules = [
    {
      id: 'a', intent: {
        id: 'a', type: 'Configure', authority: 'config/write', target: 'left',
        resources: ['config:x'], parameters: { mode: 'a' },
      },
    },
    {
      id: 'b', intent: {
        id: 'b', type: 'Configure', authority: 'config/write', target: 'right',
        resources: ['config:x'], parameters: { mode: 'b' },
      },
    },
  ];
  const plan = createIntentPlanner({ rules, conflictStrategy: 'merge' }).plan({});
  assert.equal(plan.intents.length, 1);
  assert.deepEqual(plan.refused.map(item => item.reason), ['UNMERGEABLE_CONFLICT']);
});

test('planner orders dependency graph before priority', () => {
  const plan = createIntentPlanner({ rules: baseRules }).plan({
    event: { type: 'source.changed', package: '@unrdf/core', sha: 'abc' },
  });
  assert.deepEqual(plan.intents.map(intent => intent.id), ['build-intent', 'notify-intent']);
});

test('planner refuses missing dependencies', () => {
  assert.throws(() => createIntentPlanner({
    rules: [{
      id: 'x', intent: { id: 'x', type: 'X', authority: 'x', dependsOn: ['missing'] },
    }],
  }).plan({}), /Missing intent dependency/);
});

test('planner refuses dependency cycles', () => {
  assert.throws(() => createIntentPlanner({
    rules: [
      { id: 'a', intent: { id: 'a', type: 'A', authority: 'a', dependsOn: ['b'] } },
      { id: 'b', intent: { id: 'b', type: 'B', authority: 'b', dependsOn: ['a'] } },
    ],
  }).plan({}), /cycle/);
});

test('rules require explicit authority', () => {
  assert.throws(() => createIntentPlanner({
    rules: [{ id: 'unsafe', intent: { type: 'Actuate' } }],
  }), /authority/);
});

test('equivalent plans have equivalent receipt digests', () => {
  const context = { event: { type: 'source.changed', package: 'x', sha: 'abc' } };
  const left = createIntentPlanner({ rules: baseRules }).plan(context);
  const right = createIntentPlanner({ rules: [...baseRules].reverse() }).plan(context);
  assert.equal(left.receipt.digest, right.receipt.digest);
});
