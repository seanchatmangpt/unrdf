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
    store(proposal: any): Promise<string>;
    /**
     * Retrieve a delta proposal
     * @param {string} id - Proposal ID
     * @returns {Promise<Object>} Delta proposal
     */
    retrieve(id: string): Promise<any>;
}
/**
 * Memory adapter (for testing)
 */
export class MemoryAdapter extends DeltaAdapter {
    storage: Map<any, any>;
    store(proposal: any, context?: {}): Promise<any>;
    retrieve(id: any): Promise<any>;
}
declare namespace _default {
    export { DeltaAdapter };
    export { MemoryAdapter };
}
export default _default;
