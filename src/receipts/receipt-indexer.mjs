/**
 * @fileoverview Receipt Indexer - Fast query interface for receipts
 *
 * **Purpose**: Efficient querying across receipt ledgers
 * - Package-scoped queries
 * - Time-range filtering
 * - Decision and type filtering
 * - Full-text search in reasons
 * - Field-based filtering
 *
 * **Use Cases**:
 * - "All receipts for package X in date range Y"
 * - "All receipts with decision = DENY"
 * - "All receipts mentioning invariant Q_version_consistency"
 * - "All test receipts with failures"
 *
 * @module receipts/receipt-indexer
 */

import { ReceiptLedger } from './receipt-ledger.mjs';

// ============================================================================
// Index Structures
// ============================================================================

/**
 * In-memory index for fast receipt queries
 */
class ReceiptIndex {
  constructor() {
    /** @type {Map<string, Set<string>>} Decision -> receipt IDs */
    this._byDecision = new Map();

    /** @type {Map<string, Set<string>>} Type -> receipt IDs */
    this._byType = new Map();

    /** @type {Map<string, Set<string>>} Package -> receipt IDs */
    this._byPackage = new Map();

    /** @type {Map<string, object>} Receipt ID -> receipt */
    this._receipts = new Map();

    /** @type {Array<{ epoch: string, id: string }>} Sorted by epoch */
    this._chronological = [];
  }

  /**
   * Add receipt to index
   * @param {object} receipt
   */
  add(receipt) {
    const id = receipt.id;

    // Store receipt
    this._receipts.set(id, receipt);

    // Index by decision
    if (!this._byDecision.has(receipt.decision)) {
      this._byDecision.set(receipt.decision, new Set());
    }
    this._byDecision.get(receipt.decision).add(id);

    // Index by type
    if (!this._byType.has(receipt.type)) {
      this._byType.set(receipt.type, new Set());
    }
    this._byType.get(receipt.type).add(id);

    // Index by package
    if (!this._byPackage.has(receipt.package)) {
      this._byPackage.set(receipt.package, new Set());
    }
    this._byPackage.get(receipt.package).add(id);

    // Add to chronological list (maintain sorted order)
    this._chronological.push({ epoch: receipt.epoch, id });
    this._chronological.sort((a, b) => a.epoch.localeCompare(b.epoch));
  }

  /**
   * Get receipts by decision
   * @param {string} decision
   * @returns {object[]}
   */
  getByDecision(decision) {
    const ids = this._byDecision.get(decision) || new Set();
    return Array.from(ids).map(id => this._receipts.get(id));
  }

  /**
   * Get receipts by type
   * @param {string} type
   * @returns {object[]}
   */
  getByType(type) {
    const ids = this._byType.get(type) || new Set();
    return Array.from(ids).map(id => this._receipts.get(id));
  }

  /**
   * Get receipts by package
   * @param {string} pkg
   * @returns {object[]}
   */
  getByPackage(pkg) {
    const ids = this._byPackage.get(pkg) || new Set();
    return Array.from(ids).map(id => this._receipts.get(id));
  }

  /**
   * Get receipts in time range
   * @param {string} startEpoch
   * @param {string} endEpoch
   * @returns {object[]}
   */
  getByTimeRange(startEpoch, endEpoch) {
    return this._chronological
      .filter(e => e.epoch >= startEpoch && e.epoch <= endEpoch)
      .map(e => this._receipts.get(e.id));
  }

  /**
   * Get all receipts
   * @returns {object[]}
   */
  getAll() {
    return this._chronological.map(e => this._receipts.get(e.id));
  }

  /**
   * Get receipt by ID
   * @param {string} id
   * @returns {object|undefined}
   */
  getById(id) {
    return this._receipts.get(id);
  }

  /**
   * Get index statistics
   * @returns {object}
   */
  getStats() {
    return {
      totalReceipts: this._receipts.size,
      byDecision: Object.fromEntries(
        Array.from(this._byDecision.entries()).map(([k, v]) => [k, v.size])
      ),
      byType: Object.fromEntries(
        Array.from(this._byType.entries()).map(([k, v]) => [k, v.size])
      ),
      byPackage: Object.fromEntries(
        Array.from(this._byPackage.entries()).map(([k, v]) => [k, v.size])
      ),
    };
  }

  /**
   * Clear index
   */
  clear() {
    this._byDecision.clear();
    this._byType.clear();
    this._byPackage.clear();
    this._receipts.clear();
    this._chronological = [];
  }
}

// ============================================================================
// Query Builder
// ============================================================================

/**
 * Query result with metadata
 * @typedef {object} QueryResult
 * @property {object[]} receipts - Matching receipts
 * @property {number} total - Total matches
 * @property {number} offset - Query offset
 * @property {number} limit - Query limit
 * @property {object} filters - Applied filters
 */

/**
 * Receipt Query Builder - fluent API for building queries
 *
 * @example
 * const results = await indexer.query()
 *   .package('@unrdf/core')
 *   .decision('DENY')
 *   .inTimeRange('tau_2025_01_01_0000_000', 'tau_2025_12_31_2359_999')
 *   .limit(100)
 *   .execute();
 */
export class ReceiptQuery {
  /**
   * @param {ReceiptIndexer} indexer
   */
  constructor(indexer) {
    this._indexer = indexer;
    this._filters = {};
    this._limit = 100;
    this._offset = 0;
    this._sortBy = 'epoch';
    this._sortOrder = 'desc';
  }

  /**
   * Filter by package
   * @param {string} pkg
   * @returns {ReceiptQuery}
   */
  package(pkg) {
    this._filters.package = pkg;
    return this;
  }

  /**
   * Filter by decision
   * @param {'ALLOW'|'DENY'|'WARN'|'SKIP'|'PENDING'} decision
   * @returns {ReceiptQuery}
   */
  decision(decision) {
    this._filters.decision = decision;
    return this;
  }

  /**
   * Filter by type
   * @param {string} type
   * @returns {ReceiptQuery}
   */
  type(type) {
    this._filters.type = type;
    return this;
  }

  /**
   * Filter by time range
   * @param {string} startEpoch
   * @param {string} endEpoch
   * @returns {ReceiptQuery}
   */
  inTimeRange(startEpoch, endEpoch) {
    this._filters.startEpoch = startEpoch;
    this._filters.endEpoch = endEpoch;
    return this;
  }

  /**
   * Filter by text in reason
   * @param {string} text
   * @returns {ReceiptQuery}
   */
  reasonContains(text) {
    this._filters.reasonText = text.toLowerCase();
    return this;
  }

  /**
   * Filter by field value
   * @param {string} path - Dot-separated path (e.g., 'extension.data.failed')
   * @param {any} value - Expected value
   * @returns {ReceiptQuery}
   */
  where(path, value) {
    if (!this._filters.where) {
      this._filters.where = [];
    }
    this._filters.where.push({ path, value });
    return this;
  }

  /**
   * Filter by hash
   * @param {string} hash
   * @returns {ReceiptQuery}
   */
  hash(hash) {
    this._filters.hash = hash;
    return this;
  }

  /**
   * Set result limit
   * @param {number} limit
   * @returns {ReceiptQuery}
   */
  limit(limit) {
    this._limit = limit;
    return this;
  }

  /**
   * Set result offset
   * @param {number} offset
   * @returns {ReceiptQuery}
   */
  offset(offset) {
    this._offset = offset;
    return this;
  }

  /**
   * Set sort order
   * @param {'epoch'|'decision'|'type'} field
   * @param {'asc'|'desc'} order
   * @returns {ReceiptQuery}
   */
  sort(field, order = 'desc') {
    this._sortBy = field;
    this._sortOrder = order;
    return this;
  }

  /**
   * Execute query
   * @returns {QueryResult}
   */
  execute() {
    return this._indexer._executeQuery(this._filters, {
      limit: this._limit,
      offset: this._offset,
      sortBy: this._sortBy,
      sortOrder: this._sortOrder,
    });
  }
}

// ============================================================================
// Receipt Indexer
// ============================================================================

/**
 * Receipt Indexer - builds and queries receipt indices
 *
 * @example
 * const indexer = new ReceiptIndexer(ledger);
 * await indexer.buildIndex();
 *
 * const deniedReceipts = indexer.query()
 *   .package('@unrdf/core')
 *   .decision('DENY')
 *   .execute();
 */
export class ReceiptIndexer {
  /**
   * @param {ReceiptLedger} ledger
   */
  constructor(ledger) {
    this._ledger = ledger;
    this._index = new ReceiptIndex();
    this._indexed = false;
  }

  /**
   * Build index from ledger
   * @returns {Promise<{ indexed: number, packages: number }>}
   */
  async buildIndex() {
    this._index.clear();

    let indexed = 0;
    const packages = this._ledger.getPackages();

    for (const pkg of packages) {
      const chain = this._ledger.getChain(pkg);
      for (const receipt of chain) {
        this._index.add(receipt);
        indexed++;
      }
    }

    this._indexed = true;

    return {
      indexed,
      packages: packages.length,
    };
  }

  /**
   * Add receipt to index (incremental update)
   * @param {object} receipt
   */
  addToIndex(receipt) {
    this._index.add(receipt);
  }

  /**
   * Create a query builder
   * @returns {ReceiptQuery}
   */
  query() {
    return new ReceiptQuery(this);
  }

  /**
   * Execute query (internal method)
   * @param {object} filters
   * @param {object} options
   * @returns {QueryResult}
   * @private
   */
  _executeQuery(filters, options) {
    let results = this._index.getAll();

    // Apply filters
    if (filters.package) {
      const pkgReceipts = new Set(this._index.getByPackage(filters.package).map(r => r.id));
      results = results.filter(r => pkgReceipts.has(r.id));
    }

    if (filters.decision) {
      results = results.filter(r => r.decision === filters.decision);
    }

    if (filters.type) {
      results = results.filter(r => r.type === filters.type);
    }

    if (filters.startEpoch || filters.endEpoch) {
      const start = filters.startEpoch || '';
      const end = filters.endEpoch || 'z';
      results = results.filter(r => r.epoch >= start && r.epoch <= end);
    }

    if (filters.reasonText) {
      results = results.filter(r => r.reason.toLowerCase().includes(filters.reasonText));
    }

    if (filters.hash) {
      results = results.filter(r => r.receiptHash === filters.hash);
    }

    if (filters.where) {
      for (const { path, value } of filters.where) {
        results = results.filter(r => getNestedValue(r, path) === value);
      }
    }

    // Sort
    const sortField = options.sortBy || 'epoch';
    const sortOrder = options.sortOrder === 'asc' ? 1 : -1;

    results.sort((a, b) => {
      const aVal = a[sortField] || '';
      const bVal = b[sortField] || '';
      return sortOrder * aVal.localeCompare(bVal);
    });

    // Paginate
    const total = results.length;
    const offset = options.offset || 0;
    const limit = options.limit || 100;

    results = results.slice(offset, offset + limit);

    return {
      receipts: results,
      total,
      offset,
      limit,
      filters,
    };
  }

  /**
   * Get all denied receipts
   * @param {string} [pkg] - Optional package filter
   * @returns {object[]}
   */
  getDenied(pkg) {
    let query = this.query().decision('DENY');
    if (pkg) {
      query = query.package(pkg);
    }
    return query.execute().receipts;
  }

  /**
   * Get all receipts mentioning an invariant
   * @param {string} invariant - Invariant name
   * @returns {object[]}
   */
  getByInvariant(invariant) {
    return this._index.getAll().filter(r => {
      // Check in reason
      if (r.reason.includes(invariant)) return true;

      // Check in extension data for admission receipts
      if (r.extension?.type === 'admission' && r.extension.data?.invariants) {
        return r.extension.data.invariants.some(inv => inv.name === invariant);
      }

      return false;
    });
  }

  /**
   * Get receipts with test failures
   * @returns {object[]}
   */
  getTestFailures() {
    return this.query()
      .type('test')
      .decision('DENY')
      .execute()
      .receipts;
  }

  /**
   * Get index statistics
   * @returns {object}
   */
  getStats() {
    return this._index.getStats();
  }

  /**
   * Check if index is built
   * @returns {boolean}
   */
  isIndexed() {
    return this._indexed;
  }
}

// ============================================================================
// Utilities
// ============================================================================

/**
 * Get nested value from object using dot path
 * @param {object} obj
 * @param {string} path
 * @returns {any}
 */
function getNestedValue(obj, path) {
  return path.split('.').reduce((current, key) => {
    return current && current[key] !== undefined ? current[key] : undefined;
  }, obj);
}

/**
 * Create an indexer and build index
 *
 * @param {ReceiptLedger} ledger
 * @returns {Promise<ReceiptIndexer>}
 */
export async function createIndexer(ledger) {
  const indexer = new ReceiptIndexer(ledger);
  await indexer.buildIndex();
  return indexer;
}
