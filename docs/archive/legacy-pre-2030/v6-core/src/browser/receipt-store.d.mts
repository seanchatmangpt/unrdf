/**
 * Create browser receipt store
 *
 * @param {Object} [options] - Store options
 * @returns {BrowserReceiptStore}
 */
export function createBrowserReceiptStore(options?: any): BrowserReceiptStore;
/**
 * Browser Receipt Store
 *
 * IndexedDB-backed storage for v6 receipts.
 *
 * @class
 * @example
 * const store = new BrowserReceiptStore({ dbName: 'my-app-receipts' });
 * await store.init();
 *
 * await store.saveReceipt(receipt);
 * const receipt = await store.getReceipt(receiptId);
 * const allReceipts = await store.getAllReceipts();
 */
export class BrowserReceiptStore {
    /**
     * @param {Object} [options] - Store options
     * @param {string} [options.dbName] - Database name
     */
    constructor(options?: {
        dbName?: string;
    });
    dbName: string;
    db: IDBDatabase;
    /**
     * Initialize store (open database)
     *
     * @returns {Promise<void>}
     */
    init(): Promise<void>;
    /**
     * Close store
     *
     * @returns {Promise<void>}
     */
    close(): Promise<void>;
    /**
     * Save receipt
     *
     * @param {Object} receipt - Receipt to save
     * @returns {Promise<void>}
     */
    saveReceipt(receipt: any): Promise<void>;
    /**
     * Get receipt by ID
     *
     * @param {string} id - Receipt ID
     * @returns {Promise<Object|null>}
     */
    getReceipt(id: string): Promise<any | null>;
    /**
     * Get all receipts
     *
     * @param {Object} [options] - Query options
     * @param {string} [options.receiptType] - Filter by receipt type
     * @returns {Promise<Array>}
     */
    getAllReceipts(options?: {
        receiptType?: string;
    }): Promise<any[]>;
    /**
     * Get receipt by hash
     *
     * @param {string} hash - Receipt hash
     * @returns {Promise<Object|null>}
     */
    getReceiptByHash(hash: string): Promise<any | null>;
    /**
     * Build and save merkle tree
     *
     * @param {Array<Object>} receipts - Receipts to include
     * @param {Object} [metadata] - Additional metadata
     * @returns {Promise<Object>} Tree structure
     */
    buildAndSaveMerkleTree(receipts: Array<any>, metadata?: any): Promise<any>;
    /**
     * Get merkle tree by root
     *
     * @param {string} root - Merkle root
     * @returns {Promise<Object|null>}
     */
    getMerkleTreeByRoot(root: string): Promise<any | null>;
    /**
     * Generate proof for receipt
     *
     * @param {string} receiptId - Receipt ID
     * @param {Object} tree - Merkle tree structure
     * @returns {Promise<Object>} Merkle proof
     */
    generateProof(receiptId: string, tree: any): Promise<any>;
    /**
     * Verify receipt inclusion in merkle tree
     *
     * @param {string} root - Merkle root
     * @param {Object} receipt - Receipt to verify
     * @param {Object} proof - Merkle proof
     * @returns {Promise<boolean>}
     */
    verifyReceiptInclusion(root: string, receipt: any, proof: any): Promise<boolean>;
    /**
     * Get storage statistics
     *
     * @returns {Promise<Object>}
     */
    getStats(): Promise<any>;
    /**
     * Clear all data
     *
     * @returns {Promise<void>}
     */
    clear(): Promise<void>;
}
