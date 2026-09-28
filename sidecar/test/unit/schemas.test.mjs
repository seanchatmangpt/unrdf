import { describe, it, expect } from 'vitest';
import {
  HookPhaseSchema,
  SeveritySchema,
  VersionSchema
} from '../../app/schemas/common.mjs';
import {
  PredicateSchema,
  BatchHookEvaluationSchema,
  KnowledgeHookSchema,
  CreateHookSchema,
  UpdateHookSchema
} from '../../app/schemas/hooks.mjs';

describe('app schemas (Zod v4)', () => {
  it('enum custom messages are reported', () => {
    const r = HookPhaseSchema.safeParse('bogus');
    expect(r.success).toBe(false);
    expect(r.error.issues[0].message).toBe('Hook phase must be one of: pre, post, invariant');

    const s = SeveritySchema.safeParse('nope');
    expect(s.error.issues[0].message).toBe(
      'Severity must be one of: info, warning, error, critical'
    );
  });

  it('predicate enum message and z.record(key, value) accept valid input', () => {
    const bad = PredicateSchema.safeParse({ type: 'x', params: {} });
    expect(bad.success).toBe(false);
    expect(bad.error.issues[0].message).toBe('Predicate type must be either "sparql" or "custom"');

    const ok = PredicateSchema.safeParse({
      type: 'sparql',
      query: 'ASK { ?s ?p ?o }',
      params: { a: 1, b: 'x' }
    });
    expect(ok.error?.issues ?? []).toEqual([]);
    expect(ok.success).toBe(true);
  });

  it('batch evaluation schema keeps record contexts', () => {
    const r = BatchHookEvaluationSchema.safeParse({
      hookIds: ['a-b'],
      context: { k: 'v' }
    });
    expect(r.error?.issues ?? []).toEqual([]);
  });

  it('semver schema', () => {
    expect(VersionSchema.safeParse('1.2.3').success).toBe(true);
    expect(VersionSchema.safeParse('x').success).toBe(false);
  });

  it('hook schema variants derive from the refined base (create/update/full)', () => {
    const hook = {
      id: 'my-hook',
      name: 'My hook',
      phase: 'pre',
      predicates: [{ type: 'custom', name: 'p' }]
    };
    expect(KnowledgeHookSchema.safeParse(hook).success).toBe(true);
    expect(CreateHookSchema.safeParse(hook).success).toBe(true);
    expect(UpdateHookSchema.safeParse({ id: 'my-hook', name: 'renamed' }).success).toBe(true);
    expect(UpdateHookSchema.safeParse({ name: 'no id' }).success).toBe(false);

    const orphanHash = { ...hook, selectQuerySha256: 'a'.repeat(64) };
    for (const schema of [KnowledgeHookSchema, CreateHookSchema]) {
      const r = schema.safeParse(orphanHash);
      expect(r.success).toBe(false);
      expect(r.error.issues[0].message).toBe('selectQuerySha256 requires selectQuery to be present');
    }
  });
});
