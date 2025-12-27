/**
 * @fileoverview Guide Projector - Generate how-to guides from architecture and patterns
 *
 * **Purpose**: Deterministic projection from architecture + patterns to how-to guides.
 *
 * **Projection Formula**:
 *   M_guides = Pi_guide(architecture, patterns, queries)
 *   Same inputs -> Same Markdown (deterministic, reproducible)
 *
 * **Features**:
 * - Generates step-by-step guides
 * - Includes code snippets and examples
 * - Cross-references architecture docs
 * - Supports multiple audience levels
 *
 * @module projection/guide-projector
 */

import { blake3 } from 'hash-wasm';
import { z } from 'zod';

/**
 * Schema for guide step
 */
const GuideStepSchema = z.object({
  title: z.string(),
  description: z.string(),
  code: z.string().optional(),
  notes: z.array(z.string()).default([]),
  warnings: z.array(z.string()).default([]),
});

/**
 * Schema for guide definition
 */
const GuideDefinitionSchema = z.object({
  id: z.string(),
  title: z.string(),
  description: z.string(),
  audience: z.enum(['beginner', 'intermediate', 'advanced']).default('intermediate'),
  category: z.string(),
  prerequisites: z.array(z.string()).default([]),
  steps: z.array(GuideStepSchema),
  relatedGuides: z.array(z.string()).default([]),
  tags: z.array(z.string()).default([]),
});

/**
 * Schema for guide templates
 */
const GuideTemplateSchema = z.object({
  pattern: z.string(),
  title: z.string(),
  category: z.string(),
  steps: z.array(z.object({
    title: z.string(),
    template: z.string(),
  })),
});

/**
 * Guide Projector - Transforms patterns to how-to documentation
 *
 * @class GuideProjector
 *
 * @example
 * const projector = new GuideProjector();
 * const result = await projector.project(guides);
 */
export class GuideProjector {
  /**
   * Create a new Guide projector
   *
   * @param {Object} [options] - Projector options
   * @param {string} [options.audience='all'] - Target audience filter
   * @param {boolean} [options.includePrerequisites=true] - Include prerequisites
   * @param {string} [options.headingPrefix='##'] - Markdown heading prefix
   */
  constructor(options = {}) {
    this.audience = options.audience || 'all';
    this.includePrerequisites = options.includePrerequisites !== false;
    this.headingPrefix = options.headingPrefix || '##';

    // Standard guide templates
    this.templates = this._initializeTemplates();
  }

  /**
   * Initialize standard guide templates
   *
   * @returns {Array<Object>} Templates
   * @private
   */
  _initializeTemplates() {
    return [
      {
        pattern: 'create-partition',
        title: 'Creating a Custom Partition',
        category: 'RDF Management',
        steps: [
          { title: 'Define Partition Class', template: 'Extend the base Partition class' },
          { title: 'Configure Properties', template: 'Set name, namespaces, and read-only status' },
          { title: 'Register with Universe', template: 'Add to universe partition array' },
          { title: 'Verify Registration', template: 'Check partition appears in getAllPartitions()' },
        ],
      },
      {
        pattern: 'add-ontology',
        title: 'Adding an Ontology to IndustrialSubstrate',
        category: 'RDF Management',
        steps: [
          { title: 'Obtain Ontology', template: 'Download or create TTL content' },
          { title: 'Add to Registry', template: 'Register namespace in OntologyRegistry' },
          { title: 'Load Content', template: 'Use loadIndustrialSubstrate() method' },
          { title: 'Validate', template: 'Run validateIndustrialSubstrate()' },
        ],
      },
      {
        pattern: 'create-receipt',
        title: 'Generating Admissibility Receipts',
        category: 'Governance',
        steps: [
          { title: 'Create Generator', template: 'Instantiate ReceiptGenerator' },
          { title: 'Prepare Input', template: 'Gather ontology hashes and delta capsule' },
          { title: 'Emit Receipt', template: 'Call emitAdmissibilityReceipt()' },
          { title: 'Verify Chain', template: 'Validate receipt chain integrity' },
        ],
      },
      {
        pattern: 'query-universe',
        title: 'Querying Across Partitions',
        category: 'RDF Management',
        steps: [
          { title: 'Create Universe', template: 'Instantiate Universe with registry' },
          { title: 'Write SPARQL Query', template: 'Define your query with proper prefixes' },
          { title: 'Execute Query', template: 'Call universe.query()' },
          { title: 'Process Results', template: 'Iterate over result bindings' },
        ],
      },
      {
        pattern: 'project-documentation',
        title: 'Generating Documentation',
        category: 'Documentation',
        steps: [
          { title: 'Choose Projector', template: 'Select appropriate projector type' },
          { title: 'Prepare Input', template: 'Gather source content' },
          { title: 'Execute Projection', template: 'Call project() method' },
          { title: 'Verify Hash', template: 'Check determinism with verifyDeterminism()' },
        ],
      },
    ];
  }

  /**
   * Generate guide from template
   *
   * @param {string} pattern - Template pattern name
   * @param {Object} context - Context for template rendering
   * @returns {Object} Generated guide definition
   */
  generateFromTemplate(pattern, context = {}) {
    const template = this.templates.find(t => t.pattern === pattern);
    if (!template) {
      throw new Error(`Unknown guide template: ${pattern}`);
    }

    const steps = template.steps.map((step, idx) => ({
      title: step.title,
      description: step.template,
      code: context.code?.[idx] || undefined,
      notes: context.notes?.[idx] || [],
      warnings: context.warnings?.[idx] || [],
    }));

    return {
      id: `guide-${pattern}`,
      title: template.title,
      description: context.description || `How to ${template.title.toLowerCase()}`,
      audience: context.audience || 'intermediate',
      category: template.category,
      prerequisites: context.prerequisites || [],
      steps,
      relatedGuides: context.relatedGuides || [],
      tags: [pattern, template.category.toLowerCase().replace(/\s+/g, '-')],
    };
  }

  /**
   * Validate guide definition
   *
   * @param {Object} guide - Guide definition
   * @returns {Object} Validated guide
   */
  validateGuide(guide) {
    return GuideDefinitionSchema.parse(guide);
  }

  /**
   * Generate Markdown for a single guide
   *
   * @param {Object} guide - Guide definition
   * @returns {string} Markdown content
   */
  guideToMarkdown(guide) {
    const lines = [];

    // Header
    lines.push(`# ${guide.title}`);
    lines.push('');

    // Metadata badges
    const badges = [];
    badges.push(`\`${guide.audience}\``);
    badges.push(`\`${guide.category}\``);
    for (const tag of guide.tags) {
      badges.push(`\`${tag}\``);
    }
    lines.push(badges.join(' '));
    lines.push('');

    // Description
    lines.push(guide.description);
    lines.push('');
    lines.push('---');
    lines.push('');

    // Prerequisites
    if (this.includePrerequisites && guide.prerequisites.length > 0) {
      lines.push(`${this.headingPrefix} Prerequisites`);
      lines.push('');
      for (const prereq of guide.prerequisites) {
        lines.push(`- ${prereq}`);
      }
      lines.push('');
    }

    // Steps
    lines.push(`${this.headingPrefix} Steps`);
    lines.push('');

    for (let i = 0; i < guide.steps.length; i++) {
      const step = guide.steps[i];
      lines.push(`### Step ${i + 1}: ${step.title}`);
      lines.push('');
      lines.push(step.description);
      lines.push('');

      // Warnings first
      for (const warning of step.warnings) {
        lines.push(`> **Warning:** ${warning}`);
        lines.push('');
      }

      // Code
      if (step.code) {
        lines.push('```javascript');
        lines.push(step.code);
        lines.push('```');
        lines.push('');
      }

      // Notes
      for (const note of step.notes) {
        lines.push(`> **Note:** ${note}`);
        lines.push('');
      }
    }

    // Related guides
    if (guide.relatedGuides.length > 0) {
      lines.push(`${this.headingPrefix} Related Guides`);
      lines.push('');
      for (const related of guide.relatedGuides) {
        lines.push(`- [${related}](${related.toLowerCase().replace(/\s+/g, '-')}.md)`);
      }
      lines.push('');
    }

    return lines.join('\n');
  }

  /**
   * Generate index page for all guides
   *
   * @param {Array<Object>} guides - All guides
   * @returns {string} Markdown index
   */
  generateIndex(guides) {
    const lines = [];

    lines.push('# How-To Guides');
    lines.push('');
    lines.push('Step-by-step guides for common tasks.');
    lines.push('');
    lines.push('---');
    lines.push('');

    // Group by category
    const categories = new Map();
    for (const guide of guides) {
      if (!categories.has(guide.category)) {
        categories.set(guide.category, []);
      }
      categories.get(guide.category).push(guide);
    }

    // Sort categories
    const sortedCategories = Array.from(categories.keys()).sort();

    for (const category of sortedCategories) {
      lines.push(`${this.headingPrefix} ${category}`);
      lines.push('');

      const categoryGuides = categories.get(category).sort((a, b) =>
        a.title.localeCompare(b.title)
      );

      for (const guide of categoryGuides) {
        const fileName = guide.id.toLowerCase().replace(/\s+/g, '-');
        const audienceIcon = guide.audience === 'beginner' ? '(Beginner)' :
                            guide.audience === 'advanced' ? '(Advanced)' : '';
        lines.push(`- [${guide.title}](${fileName}.md) ${audienceIcon}`);
      }
      lines.push('');
    }

    return lines.join('\n');
  }

  /**
   * Project guides to Markdown documentation
   *
   * @param {Array<Object>} guides - Guide definitions
   * @returns {Promise<{index: {markdown: string, hash: string}, guides: Array}>} Projection result
   *
   * @example
   * const result = await projector.project([
   *   { id: 'guide-1', title: 'Getting Started', ... }
   * ]);
   */
  async project(guides) {
    // Validate and filter guides
    const validatedGuides = guides
      .map(g => this.validateGuide(g))
      .filter(g => this.audience === 'all' || g.audience === this.audience);

    // Sort for determinism
    validatedGuides.sort((a, b) => a.id.localeCompare(b.id));

    // Generate individual guide pages
    const projectedGuides = [];
    const hashes = [];

    for (const guide of validatedGuides) {
      const markdown = this.guideToMarkdown(guide);
      const hash = await blake3(markdown);

      projectedGuides.push({
        id: guide.id,
        title: guide.title,
        category: guide.category,
        audience: guide.audience,
        markdown,
        hash,
        stepCount: guide.steps.length,
      });

      hashes.push(hash);
    }

    // Generate index
    const indexMarkdown = this.generateIndex(validatedGuides);
    const indexHash = await blake3(indexMarkdown);

    // Combined hash
    hashes.push(indexHash);
    hashes.sort();
    const combinedHash = await blake3(hashes.join(':'));

    return {
      index: {
        markdown: indexMarkdown,
        hash: indexHash,
      },
      guides: projectedGuides,
      combinedHash,
      guideCount: projectedGuides.length,
    };
  }

  /**
   * Generate guides from templates with context
   *
   * @param {Array<{pattern: string, context: Object}>} specs - Template specifications
   * @returns {Promise<Object>} Projection result
   */
  async projectFromTemplates(specs) {
    const guides = specs.map(spec =>
      this.generateFromTemplate(spec.pattern, spec.context)
    );
    return this.project(guides);
  }

  /**
   * Get available template patterns
   *
   * @returns {Array<string>} Pattern names
   */
  getAvailableTemplates() {
    return this.templates.map(t => t.pattern);
  }

  /**
   * Verify projection determinism
   *
   * @param {Array<Object>} guides - Guide definitions
   * @param {string} expectedHash - Expected combined hash
   * @returns {Promise<boolean>} True if hash matches
   */
  async verifyDeterminism(guides, expectedHash) {
    const result = await this.project(guides);
    return result.combinedHash === expectedHash;
  }
}

export default GuideProjector;
