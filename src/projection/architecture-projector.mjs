/**
 * @fileoverview Architecture Projector - Generate architecture docs from RDF partition structure
 *
 * **Purpose**: Deterministic projection from RDF partition metadata to architecture documentation.
 *
 * **Projection Formula**:
 *   M_arch = Pi_arch(partitions, dependencies, namespaces)
 *   Same structure -> Same Markdown (deterministic, reproducible)
 *
 * **Features**:
 * - Documents partition hierarchy and relationships
 * - Generates dependency graphs (Mermaid format)
 * - Documents namespace ownership
 * - Produces deterministic output sorted alphabetically
 *
 * @module projection/architecture-projector
 */

import { blake3 } from 'hash-wasm';
import { z } from 'zod';

/**
 * Schema for partition metadata
 */
const PartitionMetadataSchema = z.object({
  name: z.string(),
  description: z.string(),
  readOnly: z.boolean(),
  namespaceIris: z.array(z.string()),
  protectedNamespaces: z.array(z.string()),
  size: z.number(),
  priority: z.number().optional(),
});

/**
 * Schema for dependency edge
 */
const DependencyEdgeSchema = z.object({
  from: z.string(),
  to: z.string(),
  type: z.enum(['imports', 'extends', 'uses', 'depends-on']),
  weight: z.number().default(1),
});

/**
 * Schema for architecture input
 */
const ArchitectureInputSchema = z.object({
  partitions: z.array(PartitionMetadataSchema),
  dependencies: z.array(DependencyEdgeSchema).default([]),
  namespaces: z.record(z.array(z.string())).default({}),
  metadata: z.object({
    name: z.string(),
    version: z.string(),
    description: z.string(),
  }).optional(),
});

/**
 * Architecture Projector - Transforms RDF structure to architecture documentation
 *
 * @class ArchitectureProjector
 *
 * @example
 * const projector = new ArchitectureProjector();
 * const result = await projector.project({
 *   partitions: universe.getAllPartitions().map(p => p.toJSON()),
 *   dependencies: [],
 * });
 */
export class ArchitectureProjector {
  /**
   * Create a new Architecture projector
   *
   * @param {Object} [options] - Projector options
   * @param {boolean} [options.includeMermaid=true] - Generate Mermaid diagrams
   * @param {boolean} [options.includeMetrics=true] - Include size metrics
   * @param {string} [options.headingPrefix='##'] - Markdown heading prefix
   */
  constructor(options = {}) {
    this.includeMermaid = options.includeMermaid !== false;
    this.includeMetrics = options.includeMetrics !== false;
    this.headingPrefix = options.headingPrefix || '##';
  }

  /**
   * Validate and normalize architecture input
   *
   * @param {Object} input - Raw architecture input
   * @returns {Object} Validated input
   */
  validateInput(input) {
    return ArchitectureInputSchema.parse(input);
  }

  /**
   * Generate partition hierarchy documentation
   *
   * @param {Array<Object>} partitions - Partition metadata array
   * @returns {string} Markdown documentation
   */
  generatePartitionDocs(partitions) {
    const lines = [];

    lines.push(`${this.headingPrefix} Partition Hierarchy`);
    lines.push('');
    lines.push('The RDF Universe is organized into ordered partitions with specific responsibilities:');
    lines.push('');

    // Sort by priority or name for determinism
    const sorted = [...partitions].sort((a, b) => {
      if (a.priority !== undefined && b.priority !== undefined) {
        return a.priority - b.priority;
      }
      return a.name.localeCompare(b.name);
    });

    // Overview table
    lines.push('| Priority | Partition | Description | Read-Only | Quads |');
    lines.push('|----------|-----------|-------------|-----------|-------|');

    for (let i = 0; i < sorted.length; i++) {
      const p = sorted[i];
      const priority = p.priority !== undefined ? p.priority : i + 1;
      const readOnly = p.readOnly ? 'Yes' : 'No';
      lines.push(`| ${priority} | **${p.name}** | ${p.description} | ${readOnly} | ${p.size} |`);
    }
    lines.push('');

    // Detailed sections
    for (const partition of sorted) {
      lines.push(`### ${partition.name}`);
      lines.push('');
      lines.push(partition.description);
      lines.push('');

      // Properties
      lines.push('**Properties:**');
      lines.push('');
      lines.push(`- **Read-only**: ${partition.readOnly ? 'Yes' : 'No'}`);
      lines.push(`- **Size**: ${partition.size} quads`);
      lines.push('');

      // Namespaces
      if (partition.namespaceIris.length > 0) {
        lines.push('**Namespace IRIs:**');
        lines.push('');
        for (const ns of partition.namespaceIris.sort()) {
          lines.push(`- \`${ns}\``);
        }
        lines.push('');
      }

      // Protected namespaces
      if (partition.protectedNamespaces.length > 0) {
        lines.push('**Protected Namespaces:**');
        lines.push('');
        for (const ns of partition.protectedNamespaces.sort()) {
          lines.push(`- \`${ns}\``);
        }
        lines.push('');
      }
    }

    return lines.join('\n');
  }

  /**
   * Generate Mermaid dependency diagram
   *
   * @param {Array<Object>} partitions - Partition metadata
   * @param {Array<Object>} dependencies - Dependency edges
   * @returns {string} Mermaid diagram code
   */
  generateMermaidDiagram(partitions, dependencies) {
    const lines = [];

    lines.push('```mermaid');
    lines.push('graph TD');
    lines.push('');

    // Add partition nodes
    for (const partition of partitions) {
      const shape = partition.readOnly ? '[[' : '[';
      const closeShape = partition.readOnly ? ']]' : ']';
      lines.push(`    ${partition.name}${shape}"${partition.name}<br/>${partition.size} quads"${closeShape}`);
    }
    lines.push('');

    // Add edges
    const sortedDeps = [...dependencies].sort((a, b) => {
      const fromCmp = a.from.localeCompare(b.from);
      if (fromCmp !== 0) return fromCmp;
      return a.to.localeCompare(b.to);
    });

    for (const dep of sortedDeps) {
      const arrow = dep.type === 'extends' ? '==>' :
                    dep.type === 'uses' ? '-..->' : '-->';
      lines.push(`    ${dep.from} ${arrow} ${dep.to}`);
    }
    lines.push('');

    // Add styling
    lines.push('    classDef readOnly fill:#f9f,stroke:#333');
    lines.push('    classDef writable fill:#9f9,stroke:#333');
    lines.push('');

    const readOnlyPartitions = partitions.filter(p => p.readOnly).map(p => p.name);
    const writablePartitions = partitions.filter(p => !p.readOnly).map(p => p.name);

    if (readOnlyPartitions.length > 0) {
      lines.push(`    class ${readOnlyPartitions.join(',')} readOnly`);
    }
    if (writablePartitions.length > 0) {
      lines.push(`    class ${writablePartitions.join(',')} writable`);
    }

    lines.push('```');

    return lines.join('\n');
  }

  /**
   * Generate namespace ownership documentation
   *
   * @param {Object} namespaces - Map of namespace to owning partitions
   * @returns {string} Markdown documentation
   */
  generateNamespaceDocs(namespaces) {
    const lines = [];

    lines.push(`${this.headingPrefix} Namespace Ownership`);
    lines.push('');
    lines.push('Namespace IRIs and their owning partitions:');
    lines.push('');
    lines.push('| Namespace | Owner Partition(s) |');
    lines.push('|-----------|-------------------|');

    // Sort namespaces for determinism
    const sortedNamespaces = Object.keys(namespaces).sort();

    for (const ns of sortedNamespaces) {
      const owners = namespaces[ns].sort().join(', ');
      lines.push(`| \`${ns}\` | ${owners} |`);
    }
    lines.push('');

    return lines.join('\n');
  }

  /**
   * Generate metrics documentation
   *
   * @param {Array<Object>} partitions - Partition metadata
   * @returns {string} Markdown documentation
   */
  generateMetrics(partitions) {
    const lines = [];

    lines.push(`${this.headingPrefix} Metrics`);
    lines.push('');

    const totalQuads = partitions.reduce((sum, p) => sum + p.size, 0);
    const readOnlyCount = partitions.filter(p => p.readOnly).length;
    const writableCount = partitions.filter(p => !p.readOnly).length;
    const totalNamespaces = partitions.reduce((sum, p) => sum + p.namespaceIris.length, 0);

    lines.push('| Metric | Value |');
    lines.push('|--------|-------|');
    lines.push(`| Total Partitions | ${partitions.length} |`);
    lines.push(`| Read-only Partitions | ${readOnlyCount} |`);
    lines.push(`| Writable Partitions | ${writableCount} |`);
    lines.push(`| Total Quads | ${totalQuads} |`);
    lines.push(`| Total Namespaces | ${totalNamespaces} |`);
    lines.push('');

    // Size distribution
    lines.push('**Size Distribution:**');
    lines.push('');

    const maxSize = Math.max(...partitions.map(p => p.size), 1);
    for (const p of partitions.sort((a, b) => b.size - a.size)) {
      const barLength = Math.max(1, Math.round((p.size / maxSize) * 40));
      const bar = '='.repeat(barLength);
      const pct = totalQuads > 0 ? ((p.size / totalQuads) * 100).toFixed(1) : '0.0';
      lines.push(`- **${p.name}**: ${bar} ${p.size} (${pct}%)`);
    }
    lines.push('');

    return lines.join('\n');
  }

  /**
   * Project architecture input to Markdown documentation
   *
   * @param {Object} input - Architecture input
   * @returns {Promise<{markdown: string, hash: string}>} Projection result
   *
   * @example
   * const result = await projector.project({
   *   partitions: [...],
   *   dependencies: [...],
   *   metadata: { name: 'MyProject', version: '1.0.0', description: 'Desc' }
   * });
   */
  async project(input) {
    const validated = this.validateInput(input);
    const lines = [];

    // Header
    const title = validated.metadata?.name || 'Architecture';
    const version = validated.metadata?.version || '';
    const description = validated.metadata?.description || '';

    lines.push(`# ${title} Architecture`);
    lines.push('');
    if (version) {
      lines.push(`*Version: ${version}*`);
      lines.push('');
    }
    if (description) {
      lines.push(description);
      lines.push('');
    }
    lines.push('---');
    lines.push('');

    // Partition hierarchy
    lines.push(this.generatePartitionDocs(validated.partitions));
    lines.push('');

    // Dependency diagram
    if (this.includeMermaid && validated.dependencies.length > 0) {
      lines.push(`${this.headingPrefix} Dependency Graph`);
      lines.push('');
      lines.push(this.generateMermaidDiagram(validated.partitions, validated.dependencies));
      lines.push('');
    } else if (this.includeMermaid && validated.partitions.length > 0) {
      // Generate partition structure diagram even without dependencies
      lines.push(`${this.headingPrefix} Structure Diagram`);
      lines.push('');
      lines.push(this.generateMermaidDiagram(validated.partitions, []));
      lines.push('');
    }

    // Namespace ownership
    if (Object.keys(validated.namespaces).length > 0) {
      lines.push(this.generateNamespaceDocs(validated.namespaces));
    }

    // Metrics
    if (this.includeMetrics) {
      lines.push(this.generateMetrics(validated.partitions));
    }

    const markdown = lines.join('\n');
    const hash = await blake3(markdown);

    return {
      markdown,
      hash,
      partitionCount: validated.partitions.length,
      dependencyCount: validated.dependencies.length,
    };
  }

  /**
   * Extract architecture input from Universe
   *
   * @param {Object} universe - Universe instance
   * @returns {Object} Architecture input
   */
  extractFromUniverse(universe) {
    const partitions = universe.getAllPartitions().map((p, idx) => ({
      ...p.toJSON(),
      priority: idx + 1,
    }));

    // Build namespace map
    const namespaces = {};
    for (const p of partitions) {
      for (const ns of p.namespaceIris) {
        if (!namespaces[ns]) {
          namespaces[ns] = [];
        }
        namespaces[ns].push(p.name);
      }
    }

    return {
      partitions,
      dependencies: [],
      namespaces,
      metadata: {
        name: 'UNRDF Universe',
        version: '1.0.0',
        description: 'RDF Universe partition architecture',
      },
    };
  }

  /**
   * Verify projection determinism
   *
   * @param {Object} input - Architecture input
   * @param {string} expectedHash - Expected hash
   * @returns {Promise<boolean>} True if hash matches
   */
  async verifyDeterminism(input, expectedHash) {
    const result = await this.project(input);
    return result.hash === expectedHash;
  }
}

export default ArchitectureProjector;
