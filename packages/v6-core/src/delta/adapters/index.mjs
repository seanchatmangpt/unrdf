/**
 * Delta Adapters - Storage and transmission adapters
 *
 * @module @unrdf/v6-core/delta/adapters
 */

/**
 * Base adapter interface
 */
export class DeltaAdapter {
  /**
   * Store a delta proposal
   * @param {Object} proposal - Delta proposal
   * @returns {Promise<string>} Storage ID
   */
  async store(proposal) {
    throw new Error('DeltaAdapter.store() must be implemented');
  }

  /**
   * Retrieve a delta proposal
   * @param {string} id - Proposal ID
   * @returns {Promise<Object>} Delta proposal
   */
  async retrieve(id) {
    throw new Error('DeltaAdapter.retrieve() must be implemented');
  }
}

/**
 * Memory adapter (for testing)
 */
export class MemoryAdapter extends DeltaAdapter {
  constructor() {
    super();
    this.storage = new Map();
  }

  async store(proposal, context = {}) {
    const timestamp = context.t_ns ? Number(context.t_ns / 1_000_000n) : Date.now();
    const id = proposal.id || `mem-${timestamp}`;
    this.storage.set(id, proposal);
    return id;
  }

  async retrieve(id) {
    const proposal = this.storage.get(id);
    if (!proposal) {
      throw new Error(`Proposal ${id} not found`);
    }
    return proposal;
  }
}

export default {
  DeltaAdapter,
  MemoryAdapter,
};
