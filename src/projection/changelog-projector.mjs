/**
 * @fileoverview Changelog Projector - Generate changelogs from receipts
 *
 * **Purpose**: Deterministic projection from receipt chains to changelog documentation.
 *
 * **Projection Formula**:
 *   M_changelog = Pi_changelog(receipts)
 *   Same receipts -> Same Markdown (deterministic, reproducible)
 *
 * **Features**:
 * - Groups changes by epoch/date
 * - Categorizes by decision type (allow/deny)
 * - Links to receipts for audit trail
 * - Supports multiple output formats
 *
 * @module projection/changelog-projector
 */

import { blake3 } from 'hash-wasm';
import { z } from 'zod';

/**
 * Schema for receipt input
 */
const ReceiptInputSchema = z.object({
  receiptHash: z.string(),
  decision: z.enum(['allow', 'deny']),
  epoch: z.string(),
  generatedAtTime: z.string(),
  inputHashes: z.object({
    ontologyReleases: z.array(z.string()),
    deltaCapsule: z.string(),
  }),
  outputHash: z.string(),
  beforeHash: z.string().nullable(),
  merkleRoot: z.string().nullable().optional(),
  toolchainVersion: z.object({
    node: z.string(),
    packages: z.record(z.string(), z.string()).optional(),
  }),
});

/**
 * Schema for changelog entry
 */
const ChangelogEntrySchema = z.object({
  date: z.string(),
  epoch: z.string(),
  decision: z.enum(['allow', 'deny']),
  description: z.string(),
  receiptHash: z.string(),
  deltaCapsule: z.string(),
  ontologyCount: z.number(),
  breaking: z.boolean().default(false),
});

/**
 * Changelog Projector - Transforms receipts to changelog documentation
 *
 * @class ChangelogProjector
 *
 * @example
 * const projector = new ChangelogProjector();
 * const result = await projector.project(receiptChain);
 */
export class ChangelogProjector {
  /**
   * Create a new Changelog projector
   *
   * @param {Object} [options] - Projector options
   * @param {string} [options.format='markdown'] - Output format (markdown, json)
   * @param {boolean} [options.includeHashes=true] - Include receipt hashes
   * @param {boolean} [options.groupByDate=true] - Group entries by date
   * @param {string} [options.headingPrefix='##'] - Markdown heading prefix
   */
  constructor(options = {}) {
    this.format = options.format || 'markdown';
    this.includeHashes = options.includeHashes !== false;
    this.groupByDate = options.groupByDate !== false;
    this.headingPrefix = options.headingPrefix || '##';
  }

  /**
   * Parse receipt to changelog entry
   *
   * @param {Object} receipt - Receipt object
   * @returns {Object} Changelog entry
   */
  parseReceipt(receipt) {
    const validated = ReceiptInputSchema.parse(receipt);

    // Extract date from ISO timestamp
    const date = validated.generatedAtTime.split('T')[0];

    // Generate description
    const description = this._generateDescription(validated);

    return {
      date,
      epoch: validated.epoch,
      decision: validated.decision,
      description,
      receiptHash: validated.receiptHash,
      deltaCapsule: validated.inputHashes.deltaCapsule,
      ontologyCount: validated.inputHashes.ontologyReleases.length,
      breaking: validated.decision === 'deny',
    };
  }

  /**
   * Generate description from receipt
   *
   * @param {Object} receipt - Validated receipt
   * @returns {string} Description
   * @private
   */
  _generateDescription(receipt) {
    const action = receipt.decision === 'allow' ? 'Admitted' : 'Rejected';
    const ontologyText = receipt.inputHashes.ontologyReleases.length === 1
      ? '1 ontology'
      : `${receipt.inputHashes.ontologyReleases.length} ontologies`;

    return `${action} delta capsule with ${ontologyText}`;
  }

  /**
   * Group entries by date
   *
   * @param {Array<Object>} entries - Changelog entries
   * @returns {Map<string, Array<Object>>} Grouped entries
   */
  groupEntriesByDate(entries) {
    const groups = new Map();

    for (const entry of entries) {
      if (!groups.has(entry.date)) {
        groups.set(entry.date, []);
      }
      groups.get(entry.date).push(entry);
    }

    return groups;
  }

  /**
   * Generate Markdown changelog
   *
   * @param {Array<Object>} entries - Changelog entries
   * @param {Object} metadata - Changelog metadata
   * @returns {string} Markdown content
   */
  toMarkdown(entries, metadata = {}) {
    const lines = [];

    // Header
    lines.push('# Changelog');
    lines.push('');
    if (metadata.projectName) {
      lines.push(`Governance changelog for **${metadata.projectName}**`);
      lines.push('');
    }
    lines.push('All admissibility decisions are recorded with cryptographic receipts.');
    lines.push('');
    lines.push('---');
    lines.push('');

    // Sort entries by date (newest first)
    const sortedEntries = [...entries].sort((a, b) => {
      const dateCmp = b.date.localeCompare(a.date);
      if (dateCmp !== 0) return dateCmp;
      return b.epoch.localeCompare(a.epoch);
    });

    if (this.groupByDate) {
      // Group by date
      const groups = this.groupEntriesByDate(sortedEntries);
      const sortedDates = Array.from(groups.keys()).sort().reverse();

      for (const date of sortedDates) {
        lines.push(`${this.headingPrefix} ${date}`);
        lines.push('');

        const dateEntries = groups.get(date);
        for (const entry of dateEntries) {
          lines.push(...this._entryToMarkdownLines(entry));
        }
        lines.push('');
      }
    } else {
      // Flat list
      for (const entry of sortedEntries) {
        lines.push(...this._entryToMarkdownLines(entry));
        lines.push('');
      }
    }

    // Footer
    lines.push('---');
    lines.push('');
    lines.push('*Generated from governance receipt chain*');

    return lines.join('\n');
  }

  /**
   * Convert entry to Markdown lines
   *
   * @param {Object} entry - Changelog entry
   * @returns {Array<string>} Markdown lines
   * @private
   */
  _entryToMarkdownLines(entry) {
    const lines = [];

    // Decision icon
    const icon = entry.decision === 'allow' ? '+' : '-';
    const decisionLabel = entry.decision === 'allow' ? 'ALLOW' : 'DENY';

    // Main line
    lines.push(`- **[${decisionLabel}]** ${entry.description}`);

    // Details
    if (this.includeHashes) {
      lines.push(`  - Epoch: \`${entry.epoch}\``);
      lines.push(`  - Receipt: \`${entry.receiptHash.substring(0, 16)}...\``);
      lines.push(`  - Delta: \`${entry.deltaCapsule.substring(0, 16)}...\``);
    }

    // Breaking change notice
    if (entry.breaking) {
      lines.push(`  - **Breaking**: This change was rejected`);
    }

    return lines;
  }

  /**
   * Generate JSON changelog
   *
   * @param {Array<Object>} entries - Changelog entries
   * @param {Object} metadata - Changelog metadata
   * @returns {Object} JSON changelog
   */
  toJSON(entries, metadata = {}) {
    const sortedEntries = [...entries].sort((a, b) => {
      const dateCmp = b.date.localeCompare(a.date);
      if (dateCmp !== 0) return dateCmp;
      return b.epoch.localeCompare(a.epoch);
    });

    return {
      projectName: metadata.projectName || 'UNRDF',
      version: metadata.version || '1.0.0',
      generatedAt: new Date().toISOString(),
      entries: sortedEntries,
      summary: {
        totalEntries: entries.length,
        allowCount: entries.filter(e => e.decision === 'allow').length,
        denyCount: entries.filter(e => e.decision === 'deny').length,
        dateRange: {
          earliest: sortedEntries[sortedEntries.length - 1]?.date,
          latest: sortedEntries[0]?.date,
        },
      },
    };
  }

  /**
   * Project receipts to changelog documentation
   *
   * @param {Array<Object>} receipts - Receipt objects
   * @param {Object} [metadata] - Changelog metadata
   * @returns {Promise<{markdown: string, hash: string, entries: Array}>} Projection result
   *
   * @example
   * const result = await projector.project(receiptChain.getAll(), {
   *   projectName: 'MyProject'
   * });
   */
  async project(receipts, metadata = {}) {
    // Parse receipts to entries
    const entries = receipts.map(r => this.parseReceipt(r));

    // Generate output based on format
    let output;
    if (this.format === 'json') {
      output = JSON.stringify(this.toJSON(entries, metadata), null, 2);
    } else {
      output = this.toMarkdown(entries, metadata);
    }

    const hash = await blake3(output);

    return {
      markdown: this.format === 'markdown' ? output : undefined,
      json: this.format === 'json' ? output : undefined,
      hash,
      entries,
      summary: {
        totalEntries: entries.length,
        allowCount: entries.filter(e => e.decision === 'allow').length,
        denyCount: entries.filter(e => e.decision === 'deny').length,
      },
    };
  }

  /**
   * Append new receipt to existing changelog
   *
   * @param {Object} existingChangelog - Existing changelog result
   * @param {Object} newReceipt - New receipt to append
   * @param {Object} [metadata] - Changelog metadata
   * @returns {Promise<Object>} Updated projection result
   */
  async appendReceipt(existingChangelog, newReceipt, metadata = {}) {
    const newEntry = this.parseReceipt(newReceipt);
    const entries = [...existingChangelog.entries, newEntry];

    // Re-project with new entry
    let output;
    if (this.format === 'json') {
      output = JSON.stringify(this.toJSON(entries, metadata), null, 2);
    } else {
      output = this.toMarkdown(entries, metadata);
    }

    const hash = await blake3(output);

    return {
      markdown: this.format === 'markdown' ? output : undefined,
      json: this.format === 'json' ? output : undefined,
      hash,
      entries,
      summary: {
        totalEntries: entries.length,
        allowCount: entries.filter(e => e.decision === 'allow').length,
        denyCount: entries.filter(e => e.decision === 'deny').length,
      },
    };
  }

  /**
   * Generate summary statistics
   *
   * @param {Array<Object>} entries - Changelog entries
   * @returns {Object} Summary statistics
   */
  generateSummary(entries) {
    const byDate = new Map();
    const byDecision = { allow: 0, deny: 0 };

    for (const entry of entries) {
      byDecision[entry.decision]++;

      if (!byDate.has(entry.date)) {
        byDate.set(entry.date, { allow: 0, deny: 0 });
      }
      byDate.get(entry.date)[entry.decision]++;
    }

    return {
      total: entries.length,
      byDecision,
      byDate: Object.fromEntries(byDate),
      approvalRate: entries.length > 0
        ? ((byDecision.allow / entries.length) * 100).toFixed(1) + '%'
        : 'N/A',
    };
  }

  /**
   * Verify projection determinism
   *
   * @param {Array<Object>} receipts - Receipts
   * @param {Object} metadata - Metadata
   * @param {string} expectedHash - Expected hash
   * @returns {Promise<boolean>} True if hash matches
   */
  async verifyDeterminism(receipts, metadata, expectedHash) {
    const result = await this.project(receipts, metadata);
    return result.hash === expectedHash;
  }
}

export default ChangelogProjector;
