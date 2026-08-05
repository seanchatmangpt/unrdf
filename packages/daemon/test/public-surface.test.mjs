import { describe, expect, it } from 'vitest';

describe('@unrdf/daemon KGC-4D public surface', () => {
  it('executes the declared event-store and Merkle subpaths', async () => {
    const sourcing = await import('@unrdf/daemon/integrations/kgc-4d-sourcing');
    const merkle = await import('@unrdf/daemon/integrations/kgc-4d-merkle');

    expect(typeof sourcing.DaemonEventStore).toBe('function');
    expect(typeof merkle.buildMerkleTree).toBe('function');
    expect(typeof merkle.getMerkleProofPath).toBe('function');
    expect(typeof merkle.verifyMerkleProof).toBe('function');
  });
});
