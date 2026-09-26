import { describe, expect, it } from 'vitest';
import {
  accountRepositoryFacts,
  compareSemanticCompression,
  serializeRepositoryFactReceipt,
} from '../src/repository-fact-accounting.mjs';

const subject = {
  repository: 'seanchatmangpt/unrdf',
  commit: '0123456789abcdef0123456789abcdef01234567',
};

const digest = (c) => 'sha256:' + c.repeat(64);

describe('ERRC repository-fact accounting', () => {
  it('charges unknown and unproven ownership as handwritten', () => {
    const receipt = accountRepositoryFacts({
      subject,
      facts: [
        { path: 'src/a.mjs', loc: 10, ownership: 'unknown' },
        { path: 'gen/b.mjs', loc: 20, ownership: 'generated' },
        {
          path: 'gen/c.mjs',
          loc: 30,
          ownership: 'generated',
          generator: 'ggen',
          generatorDigest: digest('a'),
        },
        { path: 'reuse/d.mjs', loc: 40, ownership: 'reused', sourceOwner: 'upstream' },
        {
          path: 'reuse/e.mjs',
          loc: 50,
          ownership: 'reused',
          sourceOwner: 'upstream',
          sourceDigest: digest('b'),
        },
      ],
    });

    expect(receipt.totals).toMatchObject({
      totalLoc: 150,
      netMaintainedLoc: 70,
      handwrittenLoc: 70,
      generatedLoc: 30,
      reusedLoc: 50,
      materializedLoc: 0,
    });
    expect(receipt.facts.find((x) => x.path === 'gen/b.mjs').ownershipDowngraded).toBe(true);
    expect(receipt.facts.find((x) => x.path === 'reuse/d.mjs').ownershipDowngraded).toBe(true);
  });

  it('requires immutable exact subjects', () => {
    expect(() => accountRepositoryFacts({
      subject: { repository: 'seanchatmangpt/unrdf', commit: 'main' },
      facts: [],
    })).toThrow();
  });

  it('refuses duplicate path facts', () => {
    expect(() => accountRepositoryFacts({
      subject,
      facts: [
        { path: 'same.mjs', loc: 1, ownership: 'handwritten' },
        { path: 'same.mjs', loc: 1, ownership: 'handwritten' },
      ],
    })).toThrow('REFUSED:DUPLICATE_REPOSITORY_FACT:same.mjs');
  });

  it('produces byte-identical second-run receipts', () => {
    const input = {
      subject,
      facts: [
        { path: 'z.mjs', loc: 3, ownership: 'handwritten' },
        {
          path: 'a.mjs',
          loc: 9,
          ownership: 'materialized',
          sourceOwner: 'ietf',
          sourceDigest: digest('c'),
          materializationReceipt: digest('d'),
        },
      ],
    };
    const first = serializeRepositoryFactReceipt(accountRepositoryFacts(input));
    const second = serializeRepositoryFactReceipt(accountRepositoryFacts(input));
    expect(first).toBe(second);
  });

  it('measures maintained-LOC deletion independently of total generated output', () => {
    const before = accountRepositoryFacts({
      subject,
      facts: [{ path: 'manual.mjs', loc: 100, ownership: 'handwritten' }],
    });
    const after = accountRepositoryFacts({
      subject: { ...subject, commit: '89abcdef0123456789abcdef0123456789abcdef' },
      facts: [
        { path: 'generator.mjs', loc: 20, ownership: 'handwritten' },
        {
          path: 'generated-a.mjs',
          loc: 100,
          ownership: 'generated',
          generator: 'generator.mjs',
          generatorDigest: digest('e'),
        },
        {
          path: 'generated-b.mjs',
          loc: 100,
          ownership: 'generated',
          generator: 'generator.mjs',
          generatorDigest: digest('e'),
        },
      ],
    });

    const delta = compareSemanticCompression(before, after);
    expect(delta.maintainedLocDelta).toBe(-80);
    expect(delta.totalLocDelta).toBe(120);
    expect(after.compressionRatio).toBe(11);
  });
});
