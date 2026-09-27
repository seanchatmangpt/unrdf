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
