/**
 * @fileoverview Projection Pipeline - Unified documentation generation from RDF universe
 *
 * **Purpose**: Orchestrate all projectors to generate complete, deterministic documentation.
 *
 * **Pipeline Formula**:
 *   M_t = Pi_doc(mu(O_t))
 *   Where:
 *   - O_t = Monorepo universe at time t
 *   - mu(O) = Operation producing artifacts
 *   - Pi_doc = Documentation projection
 *   - M_t = Markdown documentation
 *
 * **Guarantee**: Same O -> Same M (deterministic, reproducible)
 *
 * @module projection/projection-pipeline
 */

import { blake3 } from 'hash-wasm';
import { z } from 'zod';

import { JsDocProjector } from './jsdoc-projector.mjs';
import { ArchitectureProjector } from './architecture-projector.mjs';
import { ExampleProjector } from './example-projector.mjs';
import { GuideProjector } from './guide-projector.mjs';
import { ChangelogProjector } from './changelog-projector.mjs';
import { DiataxisRenderer } from './diataxis-renderer.mjs';

/**
 * Schema for pipeline configuration
 */
const PipelineConfigSchema = z.object({
  projectName: z.string(),
  version: z.string().optional(),
  description: z.string().optional(),
  outputDir: z.string().optional(),
  audiences: z.array(z.enum(['user', 'contributor', 'operator', 'auditor'])).default(['user']),
  projectors: z.object({
    jsdoc: z.boolean().default(true),
    architecture: z.boolean().default(true),
    examples: z.boolean().default(true),
    guides: z.boolean().default(true),
    changelog: z.boolean().default(true),
  }).default({}),
});

/**
 * Schema for pipeline input
 */
const PipelineInputSchema = z.object({
  sources: z.array(z.object({
    name: z.string(),
    content: z.string(),
    type: z.enum(['source', 'test']),
  })).default([]),
  universe: z.any().optional(),
  receipts: z.array(z.any()).default([]),
  guides: z.array(z.any()).default([]),
  customDocs: z.array(z.object({
    id: z.string(),
    title: z.string(),
    content: z.string(),
    quadrant: z.enum(['tutorial', 'how-to', 'reference', 'explanation']).optional(),
    audience: z.enum(['user', 'contributor', 'operator', 'auditor']).optional(),
  })).default([]),
});

/**
 * Schema for projection receipt
 */
const ProjectionReceiptSchema = z.object({
  projectorType: z.string(),
  inputHash: z.string(),
  outputHash: z.string(),
  timestamp: z.string(),
  documentCount: z.number(),
  deterministic: z.boolean(),
});

/**
 * Projection Pipeline - Unified documentation generation
 *
 * @class ProjectionPipeline
 *
 * @example
 * const pipeline = new ProjectionPipeline({
 *   projectName: 'UNRDF',
 *   version: '1.0.0'
 * });
 *
 * const result = await pipeline.run({
 *   sources: [...],
 *   universe: universe,
 *   receipts: receiptChain.getAll()
 * });
 */
export class ProjectionPipeline {
  /**
   * Create a new projection pipeline
   *
   * @param {Object} config - Pipeline configuration
   * @param {string} config.projectName - Project name
   * @param {string} [config.version] - Project version
   * @param {Array<string>} [config.audiences] - Target audiences
   */
  constructor(config) {
    this.config = PipelineConfigSchema.parse(config);

    // Initialize projectors
    this.jsDocProjector = new JsDocProjector();
    this.architectureProjector = new ArchitectureProjector();
    this.exampleProjector = new ExampleProjector();
    this.guideProjector = new GuideProjector();
    this.changelogProjector = new ChangelogProjector();
    this.diataxisRenderer = new DiataxisRenderer({
      projectName: this.config.projectName,
      version: this.config.version,
      description: this.config.description,
      audiences: this.config.audiences,
    });

    // Receipt storage
    this.receipts = [];
  }

  /**
   * Validate pipeline input
   *
   * @param {Object} input - Raw input
   * @returns {Object} Validated input
   */
  validateInput(input) {
    return PipelineInputSchema.parse(input);
  }

  /**
   * Compute input hash for determinism verification
   *
   * @param {Object} input - Pipeline input
   * @returns {Promise<string>} Input hash
   */
  async computeInputHash(input) {
    // Canonical serialization
    const sourceHashes = await Promise.all(
      input.sources.map(s => blake3(s.content))
    );

    const canonical = JSON.stringify({
      sources: sourceHashes.sort(),
      universeSize: input.universe?.totalSize || 0,
      receiptCount: input.receipts.length,
      guideCount: input.guides.length,
      customDocCount: input.customDocs.length,
    });

    return blake3(canonical);
  }

  /**
   * Emit projection receipt
   *
   * @param {string} projectorType - Type of projector
   * @param {string} inputHash - Input hash
   * @param {string} outputHash - Output hash
   * @param {number} documentCount - Number of documents generated
   * @returns {Object} Receipt
   */
  emitReceipt(projectorType, inputHash, outputHash, documentCount) {
    const receipt = {
      projectorType,
      inputHash,
      outputHash,
      timestamp: new Date().toISOString(),
      documentCount,
      deterministic: true,
    };

    this.receipts.push(receipt);
    return receipt;
  }

  /**
   * Run JSDoc projection
   *
   * @param {Array<Object>} sources - Source files
   * @returns {Promise<Object>} Projection result
   */
  async projectJsDocs(sources) {
    const sourcesOnly = sources.filter(s => s.type === 'source');
    if (sourcesOnly.length === 0) {
      return { modules: [], combinedHash: '', totalEntries: 0 };
    }

    const result = await this.jsDocProjector.projectMultiple(
      sourcesOnly.map(s => ({ content: s.content, name: s.name }))
    );

    const inputHash = await blake3(sourcesOnly.map(s => s.content).join(''));
    this.emitReceipt('jsdoc', inputHash, result.combinedHash, result.modules.length);

    return result;
  }

  /**
   * Run architecture projection
   *
   * @param {Object} universe - Universe instance
   * @returns {Promise<Object>} Projection result
   */
  async projectArchitecture(universe) {
    if (!universe) {
      return { markdown: '', hash: '', partitionCount: 0 };
    }

    const input = this.architectureProjector.extractFromUniverse(universe);
    input.metadata = {
      name: this.config.projectName,
      version: this.config.version || '1.0.0',
      description: this.config.description || 'RDF Universe Architecture',
    };

    const result = await this.architectureProjector.project(input);

    const inputHash = await blake3(JSON.stringify(input));
    this.emitReceipt('architecture', inputHash, result.hash, 1);

    return result;
  }

  /**
   * Run example projection
   *
   * @param {Array<Object>} sources - Source/test files
   * @returns {Promise<Object>} Projection result
   */
  async projectExamples(sources) {
    const testFiles = sources.filter(s => s.type === 'test');
    if (testFiles.length === 0) {
      return { files: [], combinedHash: '', totalExamples: 0 };
    }

    const result = await this.exampleProjector.projectMultiple(
      testFiles.map(s => ({ content: s.content, name: s.name }))
    );

    const inputHash = await blake3(testFiles.map(s => s.content).join(''));
    this.emitReceipt('examples', inputHash, result.combinedHash, result.files.length);

    return result;
  }

  /**
   * Run guide projection
   *
   * @param {Array<Object>} guides - Guide definitions
   * @returns {Promise<Object>} Projection result
   */
  async projectGuides(guides) {
    if (guides.length === 0) {
      // Generate from templates if no guides provided
      const templateSpecs = this.guideProjector.getAvailableTemplates().map(pattern => ({
        pattern,
        context: {},
      }));
      return this.guideProjector.projectFromTemplates(templateSpecs);
    }

    const result = await this.guideProjector.project(guides);

    const inputHash = await blake3(JSON.stringify(guides));
    this.emitReceipt('guides', inputHash, result.combinedHash, result.guideCount);

    return result;
  }

  /**
   * Run changelog projection
   *
   * @param {Array<Object>} receipts - Receipt chain
   * @returns {Promise<Object>} Projection result
   */
  async projectChangelog(receipts) {
    if (receipts.length === 0) {
      return { markdown: '', hash: '', entries: [], summary: { totalEntries: 0 } };
    }

    const result = await this.changelogProjector.project(receipts, {
      projectName: this.config.projectName,
      version: this.config.version,
    });

    const inputHash = await blake3(JSON.stringify(receipts.map(r => r.receiptHash || r)));
    this.emitReceipt('changelog', inputHash, result.hash, result.entries.length);

    return result;
  }

  /**
   * Run complete pipeline
   *
   * @param {Object} input - Pipeline input
   * @returns {Promise<Object>} Complete documentation output
   *
   * @example
   * const result = await pipeline.run({
   *   sources: [
   *     { name: 'universe.mjs', content: '...', type: 'source' },
   *     { name: 'universe.test.mjs', content: '...', type: 'test' }
   *   ],
   *   universe: universe,
   *   receipts: []
   * });
   */
  async run(input) {
    const validated = this.validateInput(input);
    const inputHash = await this.computeInputHash(validated);

    // Reset receipts
    this.receipts = [];

    const results = {};

    // Run enabled projectors in parallel where possible
    const projections = [];

    if (this.config.projectors.jsdoc) {
      projections.push(
        this.projectJsDocs(validated.sources).then(r => { results.jsdoc = r; })
      );
    }

    if (this.config.projectors.architecture && validated.universe) {
      projections.push(
        this.projectArchitecture(validated.universe).then(r => { results.architecture = r; })
      );
    }

    if (this.config.projectors.examples) {
      projections.push(
        this.projectExamples(validated.sources).then(r => { results.examples = r; })
      );
    }

    if (this.config.projectors.guides) {
      projections.push(
        this.projectGuides(validated.guides).then(r => { results.guides = r; })
      );
    }

    if (this.config.projectors.changelog) {
      projections.push(
        this.projectChangelog(validated.receipts).then(r => { results.changelog = r; })
      );
    }

    await Promise.all(projections);

    // Collect all documents for Diataxis rendering
    const allDocs = this._collectDocuments(results, validated.customDocs);

    // Render with Diataxis
    const diataxisResult = await this.diataxisRenderer.render(allDocs);

    // Compute combined output hash
    const outputHashes = this.receipts.map(r => r.outputHash).sort();
    outputHashes.push(diataxisResult.hash);
    const combinedHash = await blake3(outputHashes.join(':'));

    // Emit pipeline receipt
    this.emitReceipt('pipeline', inputHash, combinedHash, allDocs.length);

    return {
      inputHash,
      outputHash: combinedHash,
      projections: results,
      diataxis: diataxisResult,
      receipts: this.receipts,
      summary: {
        sourceCount: validated.sources.filter(s => s.type === 'source').length,
        testCount: validated.sources.filter(s => s.type === 'test').length,
        documentCount: allDocs.length,
        guideCount: results.guides?.guideCount || 0,
        exampleCount: results.examples?.totalExamples || 0,
      },
      timestamp: new Date().toISOString(),
      deterministic: this._verifyDeterminism(),
    };
  }

  /**
   * Collect all documents from projection results
   *
   * @param {Object} results - Projection results
   * @param {Array<Object>} customDocs - Custom documents
   * @returns {Array<Object>} All documents
   * @private
   */
  _collectDocuments(results, customDocs) {
    const docs = [];

    // API reference docs (from JSDoc)
    if (results.jsdoc?.modules) {
      for (const module of results.jsdoc.modules) {
        docs.push({
          id: `api-${module.moduleName.replace(/[^a-z0-9]/gi, '-')}`,
          title: `API: ${module.moduleName}`,
          content: module.markdown,
          quadrant: 'reference',
          audience: 'user',
          tags: ['api', 'reference'],
        });
      }
    }

    // Architecture docs
    if (results.architecture?.markdown) {
      docs.push({
        id: 'architecture',
        title: 'Architecture',
        content: results.architecture.markdown,
        quadrant: 'explanation',
        audience: 'contributor',
        tags: ['architecture', 'design'],
      });
    }

    // Example docs
    if (results.examples?.files) {
      for (const file of results.examples.files) {
        docs.push({
          id: `examples-${file.fileName.replace(/[^a-z0-9]/gi, '-')}`,
          title: `Examples: ${file.fileName}`,
          content: file.markdown,
          quadrant: 'tutorial',
          audience: 'user',
          tags: ['examples', 'tutorial'],
        });
      }
    }

    // Guide docs
    if (results.guides?.guides) {
      for (const guide of results.guides.guides) {
        docs.push({
          id: guide.id,
          title: guide.title,
          content: guide.markdown,
          quadrant: 'how-to',
          audience: 'user',
          tags: ['guide', 'how-to'],
        });
      }
    }

    // Changelog
    if (results.changelog?.markdown) {
      docs.push({
        id: 'changelog',
        title: 'Changelog',
        content: results.changelog.markdown,
        quadrant: 'reference',
        audience: 'auditor',
        tags: ['changelog', 'governance'],
      });
    }

    // Custom docs
    for (const doc of customDocs) {
      docs.push({
        ...doc,
        quadrant: doc.quadrant || 'explanation',
        audience: doc.audience || 'user',
        tags: doc.tags || [],
      });
    }

    return docs;
  }

  /**
   * Verify all projections are deterministic
   *
   * @returns {boolean} True if all deterministic
   * @private
   */
  _verifyDeterminism() {
    return this.receipts.every(r => r.deterministic);
  }

  /**
   * Get all projection receipts
   *
   * @returns {Array<Object>} Receipts
   */
  getReceipts() {
    return [...this.receipts];
  }

  /**
   * Verify pipeline determinism against expected hash
   *
   * @param {Object} input - Pipeline input
   * @param {string} expectedHash - Expected output hash
   * @returns {Promise<boolean>} True if deterministic
   */
  async verifyDeterminism(input, expectedHash) {
    const result = await this.run(input);
    return result.outputHash === expectedHash;
  }

  /**
   * Generate pipeline summary report
   *
   * @param {Object} result - Pipeline result
   * @returns {string} Markdown summary
   */
  generateReport(result) {
    const lines = [];

    lines.push('# Documentation Projection Report');
    lines.push('');
    lines.push(`**Project**: ${this.config.projectName}`);
    lines.push(`**Version**: ${this.config.version || 'N/A'}`);
    lines.push(`**Generated**: ${result.timestamp}`);
    lines.push('');
    lines.push('---');
    lines.push('');

    lines.push('## Summary');
    lines.push('');
    lines.push('| Metric | Value |');
    lines.push('|--------|-------|');
    lines.push(`| Source Files | ${result.summary.sourceCount} |`);
    lines.push(`| Test Files | ${result.summary.testCount} |`);
    lines.push(`| Documents Generated | ${result.summary.documentCount} |`);
    lines.push(`| Guides | ${result.summary.guideCount} |`);
    lines.push(`| Examples | ${result.summary.exampleCount} |`);
    lines.push(`| Deterministic | ${result.deterministic ? 'Yes' : 'No'} |`);
    lines.push('');

    lines.push('## Hashes');
    lines.push('');
    lines.push(`- **Input Hash**: \`${result.inputHash}\``);
    lines.push(`- **Output Hash**: \`${result.outputHash}\``);
    lines.push('');

    lines.push('## Projection Receipts');
    lines.push('');
    lines.push('| Projector | Input Hash | Output Hash | Documents |');
    lines.push('|-----------|------------|-------------|-----------|');

    for (const receipt of result.receipts) {
      lines.push(
        `| ${receipt.projectorType} | \`${receipt.inputHash.substring(0, 8)}...\` | ` +
        `\`${receipt.outputHash.substring(0, 8)}...\` | ${receipt.documentCount} |`
      );
    }
    lines.push('');

    return lines.join('\n');
  }
}

export default ProjectionPipeline;
