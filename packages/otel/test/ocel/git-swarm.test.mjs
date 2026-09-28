import { describe, expect, it } from 'vitest';
import { createGitSwarmOcel, validateOcel2Document, gitSwarmOcelToQuads } from '../../src/ocel/git-swarm.mjs';

describe('git swarm OCEL 2.0', () => {
  const input = {
    runId: 'run-1',
    lane: 'runtime',
    repository: 'seanchatmangpt/gitvan',
    branch: 'feat/swarm',
    baseCommit: 'a'.repeat(40),
    commit: 'b'.repeat(40),
    task: 'T001',
    tool: 'github.create_tree',
    time: '2026-09-26T19:00:00.000Z',
    surfaces: ['src/swarm/receipt-service.mjs'],
    events: [
      { type: 'task_started', sequence: 1, tool: 'scheduler' },
      { type: 'write_succeeded', sequence: 2, surface: 'src/swarm/receipt-service.mjs', tool: 'github.create_file', outcome: 'created' },
      { type: 'commit_created', sequence: 3, tool: 'github.create_commit', outcome: 'ok' },
    ],
  };

  it('creates a referentially valid OCEL 2.0 document', () => {
    const doc = createGitSwarmOcel(input);
    expect(validateOcel2Document(doc)).toEqual({ valid: true, errors: [] });
    expect(doc.events).toHaveLength(3);
    expect(doc.objects.some(o => o.type === 'Commit' && o.id === `commit:${input.commit}`)).toBe(true);
  });

  it('refuses construction before OCEL materialization when exact task/tool identity is missing', () => {
    expect(() => createGitSwarmOcel({ ...input, task: '' })).toThrow('task must be a non-empty string');
    expect(() => createGitSwarmOcel({ ...input, tool: '' })).toThrow('tool must be a non-empty string');
    expect(() => createGitSwarmOcel({ ...input, commit: 'abc123' })).toThrow('commit_not_full_sha');
  });

  it('binds task and admitted tool into every event', () => {
    const doc = createGitSwarmOcel(input);
    expect(doc.objects.some(o => o.type === 'Task' && o.id === 'task:T001')).toBe(true);
    expect(doc.events.every(e => e.relationships.some(r => r.qualifier === 'task'))).toBe(true);
    expect(doc.events.every(e => e.relationships.some(r => r.qualifier === 'admitted-tool'))).toBe(true);
  });

  it('projects the same evidence into RDF quads', () => {
    const quads = gitSwarmOcelToQuads(createGitSwarmOcel(input));
    expect(quads.length).toBeGreaterThan(20);
    expect(quads.some(q => q.object.value === 'write_succeeded')).toBe(true);
  });

  it('rejects relationships to unknown objects', () => {
    const doc = createGitSwarmOcel(input);
    doc.events[0].relationships.push({ objectId: 'missing', qualifier: 'target' });
    const result = validateOcel2Document(doc);
    expect(result.valid).toBe(false);
    expect(result.errors.join(' ')).toContain('unknown object');
  });
});
