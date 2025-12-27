/**
 * @fileoverview Diataxis Renderer - Route documentation to appropriate Diataxis quadrants
 *
 * **Purpose**: Unified renderer that applies Diataxis framework to all documentation.
 *
 * **Diataxis Framework**:
 * - Tutorials: Learning-oriented, step-by-step
 * - How-to Guides: Goal-oriented, problem-solving
 * - Reference: Information-oriented, accurate
 * - Explanation: Understanding-oriented, context
 *
 * **Projection Formula**:
 *   M_diataxis = Pi_diataxis(docs, audience)
 *   Same docs + audience -> Same structure (deterministic, reproducible)
 *
 * @module projection/diataxis-renderer
 */

import { blake3 } from 'hash-wasm';
import { z } from 'zod';

/**
 * Schema for Diataxis document
 */
const DiataxisDocSchema = z.object({
  id: z.string(),
  title: z.string(),
  content: z.string(),
  quadrant: z.enum(['tutorial', 'how-to', 'reference', 'explanation']),
  audience: z.enum(['user', 'contributor', 'operator', 'auditor']).default('user'),
  tags: z.array(z.string()).default([]),
  order: z.number().default(0),
});

/**
 * Schema for Diataxis configuration
 */
const DiataxisConfigSchema = z.object({
  projectName: z.string(),
  version: z.string().optional(),
  description: z.string().optional(),
  audiences: z.array(z.enum(['user', 'contributor', 'operator', 'auditor'])).default(['user']),
  customQuadrants: z.record(z.object({
    label: z.string(),
    description: z.string(),
    icon: z.string().optional(),
  })).optional(),
});

/**
 * Diataxis quadrant definitions
 */
const QUADRANT_DEFINITIONS = {
  tutorial: {
    label: 'Tutorials',
    description: 'Learning-oriented guides that take you step-by-step through a process',
    icon: 'graduation-cap',
    purpose: 'To allow the newcomer to get started',
    form: 'A lesson',
  },
  'how-to': {
    label: 'How-to Guides',
    description: 'Goal-oriented directions for solving specific problems',
    icon: 'wrench',
    purpose: 'To show how to solve a specific problem',
    form: 'A series of steps',
  },
  reference: {
    label: 'Reference',
    description: 'Technical descriptions of the machinery and how to operate it',
    icon: 'book',
    purpose: 'To describe the machinery',
    form: 'Dry description',
  },
  explanation: {
    label: 'Explanation',
    description: 'Understanding-oriented discussion that clarifies and illuminates',
    icon: 'lightbulb',
    purpose: 'To explain',
    form: 'Discursive explanation',
  },
};

/**
 * Diataxis Renderer - Routes and renders documentation to Diataxis quadrants
 *
 * @class DiataxisRenderer
 *
 * @example
 * const renderer = new DiataxisRenderer({
 *   projectName: 'UNRDF',
 *   audiences: ['user', 'contributor']
 * });
 * const result = await renderer.render(documents);
 */
export class DiataxisRenderer {
  /**
   * Create a new Diataxis renderer
   *
   * @param {Object} config - Renderer configuration
   * @param {string} config.projectName - Project name
   * @param {string} [config.version] - Project version
   * @param {Array<string>} [config.audiences] - Target audiences
   */
  constructor(config) {
    this.config = DiataxisConfigSchema.parse(config);
    this.quadrants = { ...QUADRANT_DEFINITIONS, ...this.config.customQuadrants };
  }

  /**
   * Classify document to Diataxis quadrant
   *
   * @param {Object} doc - Document to classify
   * @returns {string} Quadrant name
   */
  classifyDocument(doc) {
    // If already classified, return
    if (doc.quadrant) {
      return doc.quadrant;
    }

    const content = doc.content.toLowerCase();
    const title = doc.title.toLowerCase();

    // Tutorial indicators
    if (
      title.includes('tutorial') ||
      title.includes('getting started') ||
      title.includes('introduction') ||
      content.includes('in this tutorial') ||
      content.includes('you will learn')
    ) {
      return 'tutorial';
    }

    // How-to indicators
    if (
      title.includes('how to') ||
      title.includes('guide') ||
      content.includes('step 1') ||
      content.includes('steps:') ||
      content.includes('to accomplish')
    ) {
      return 'how-to';
    }

    // Reference indicators
    if (
      title.includes('api') ||
      title.includes('reference') ||
      title.includes('specification') ||
      content.includes('parameters:') ||
      content.includes('@param') ||
      content.includes('returns:')
    ) {
      return 'reference';
    }

    // Explanation indicators (default)
    return 'explanation';
  }

  /**
   * Validate and normalize documents
   *
   * @param {Array<Object>} docs - Documents to validate
   * @returns {Array<Object>} Validated documents
   */
  validateDocuments(docs) {
    return docs.map((doc, idx) => {
      const quadrant = doc.quadrant || this.classifyDocument(doc);
      return DiataxisDocSchema.parse({
        ...doc,
        quadrant,
        order: doc.order ?? idx,
      });
    });
  }

  /**
   * Group documents by quadrant
   *
   * @param {Array<Object>} docs - Documents
   * @returns {Map<string, Array<Object>>} Grouped documents
   */
  groupByQuadrant(docs) {
    const groups = new Map();

    for (const quadrant of Object.keys(this.quadrants)) {
      groups.set(quadrant, []);
    }

    for (const doc of docs) {
      groups.get(doc.quadrant).push(doc);
    }

    // Sort each group
    for (const [quadrant, items] of groups) {
      items.sort((a, b) => a.order - b.order || a.title.localeCompare(b.title));
    }

    return groups;
  }

  /**
   * Group documents by audience
   *
   * @param {Array<Object>} docs - Documents
   * @returns {Map<string, Array<Object>>} Grouped documents
   */
  groupByAudience(docs) {
    const groups = new Map();

    for (const audience of this.config.audiences) {
      groups.set(audience, []);
    }

    for (const doc of docs) {
      if (groups.has(doc.audience)) {
        groups.get(doc.audience).push(doc);
      }
    }

    return groups;
  }

  /**
   * Generate navigation structure
   *
   * @param {Map<string, Array<Object>>} quadrantGroups - Documents by quadrant
   * @returns {Object} Navigation structure
   */
  generateNavigation(quadrantGroups) {
    const nav = {};

    for (const [quadrant, docs] of quadrantGroups) {
      const quadrantDef = this.quadrants[quadrant];
      nav[quadrant] = {
        label: quadrantDef.label,
        description: quadrantDef.description,
        items: docs.map(doc => ({
          id: doc.id,
          title: doc.title,
          path: `/${quadrant}/${doc.id}.md`,
          tags: doc.tags,
        })),
      };
    }

    return nav;
  }

  /**
   * Generate index page for a quadrant
   *
   * @param {string} quadrant - Quadrant name
   * @param {Array<Object>} docs - Documents in quadrant
   * @returns {string} Markdown index
   */
  generateQuadrantIndex(quadrant, docs) {
    const def = this.quadrants[quadrant];
    const lines = [];

    lines.push(`# ${def.label}`);
    lines.push('');
    lines.push(def.description);
    lines.push('');
    lines.push(`*Purpose: ${def.purpose}*`);
    lines.push('');
    lines.push('---');
    lines.push('');

    if (docs.length === 0) {
      lines.push('*No documentation available in this category yet.*');
    } else {
      for (const doc of docs) {
        const path = `${doc.id}.md`;
        lines.push(`## [${doc.title}](${path})`);
        lines.push('');

        // Extract first paragraph as summary
        const firstPara = doc.content.split('\n\n')[0]?.replace(/^#+\s+.*\n/, '').trim();
        if (firstPara) {
          lines.push(firstPara.substring(0, 200) + (firstPara.length > 200 ? '...' : ''));
          lines.push('');
        }

        if (doc.tags.length > 0) {
          lines.push(`*Tags: ${doc.tags.join(', ')}*`);
          lines.push('');
        }
      }
    }

    return lines.join('\n');
  }

  /**
   * Generate main documentation index
   *
   * @param {Map<string, Array<Object>>} quadrantGroups - Documents by quadrant
   * @returns {string} Markdown index
   */
  generateMainIndex(quadrantGroups) {
    const lines = [];

    lines.push(`# ${this.config.projectName} Documentation`);
    lines.push('');
    if (this.config.version) {
      lines.push(`*Version ${this.config.version}*`);
      lines.push('');
    }
    if (this.config.description) {
      lines.push(this.config.description);
      lines.push('');
    }
    lines.push('---');
    lines.push('');

    // Diataxis quadrant overview
    lines.push('## Documentation Structure');
    lines.push('');
    lines.push('This documentation follows the [Diataxis](https://diataxis.fr/) framework:');
    lines.push('');
    lines.push('| Category | Purpose | For |');
    lines.push('|----------|---------|-----|');

    for (const [quadrant, docs] of quadrantGroups) {
      const def = this.quadrants[quadrant];
      const count = docs.length;
      lines.push(`| [${def.label}](${quadrant}/index.md) | ${def.purpose} | ${count} docs |`);
    }
    lines.push('');

    // Quick links by quadrant
    for (const [quadrant, docs] of quadrantGroups) {
      if (docs.length === 0) continue;

      const def = this.quadrants[quadrant];
      lines.push(`### ${def.label}`);
      lines.push('');

      const topDocs = docs.slice(0, 5);
      for (const doc of topDocs) {
        lines.push(`- [${doc.title}](${quadrant}/${doc.id}.md)`);
      }
      if (docs.length > 5) {
        lines.push(`- [View all ${docs.length} ${def.label.toLowerCase()}...](${quadrant}/index.md)`);
      }
      lines.push('');
    }

    return lines.join('\n');
  }

  /**
   * Generate audience-specific view
   *
   * @param {string} audience - Audience name
   * @param {Array<Object>} docs - Documents for audience
   * @returns {string} Markdown content
   */
  generateAudienceView(audience, docs) {
    const lines = [];

    const audienceLabels = {
      user: 'Users',
      contributor: 'Contributors',
      operator: 'Operators',
      auditor: 'Auditors',
    };

    lines.push(`# Documentation for ${audienceLabels[audience]}`);
    lines.push('');
    lines.push('---');
    lines.push('');

    // Group by quadrant
    const byQuadrant = new Map();
    for (const doc of docs) {
      if (!byQuadrant.has(doc.quadrant)) {
        byQuadrant.set(doc.quadrant, []);
      }
      byQuadrant.get(doc.quadrant).push(doc);
    }

    for (const [quadrant, quadrantDocs] of byQuadrant) {
      const def = this.quadrants[quadrant];
      lines.push(`## ${def.label}`);
      lines.push('');

      for (const doc of quadrantDocs) {
        lines.push(`- [${doc.title}](../${quadrant}/${doc.id}.md)`);
      }
      lines.push('');
    }

    return lines.join('\n');
  }

  /**
   * Render all documentation
   *
   * @param {Array<Object>} documents - Documents to render
   * @returns {Promise<Object>} Rendered documentation structure
   *
   * @example
   * const result = await renderer.render(documents);
   * console.log(result.mainIndex);
   * console.log(result.quadrants.tutorial);
   */
  async render(documents) {
    // Validate documents
    const validated = this.validateDocuments(documents);

    // Group by quadrant
    const quadrantGroups = this.groupByQuadrant(validated);

    // Group by audience
    const audienceGroups = this.groupByAudience(validated);

    // Generate outputs
    const outputs = {
      mainIndex: this.generateMainIndex(quadrantGroups),
      navigation: this.generateNavigation(quadrantGroups),
      quadrants: {},
      audiences: {},
      documents: {},
    };

    // Quadrant indexes
    for (const [quadrant, docs] of quadrantGroups) {
      outputs.quadrants[quadrant] = {
        index: this.generateQuadrantIndex(quadrant, docs),
        documents: docs.map(d => ({ id: d.id, content: d.content })),
      };
    }

    // Audience views
    for (const [audience, docs] of audienceGroups) {
      outputs.audiences[audience] = this.generateAudienceView(audience, docs);
    }

    // Individual documents
    for (const doc of validated) {
      outputs.documents[doc.id] = doc.content;
    }

    // Compute combined hash
    const hashInput = JSON.stringify({
      mainIndex: outputs.mainIndex,
      quadrants: Object.keys(outputs.quadrants).sort(),
      documentIds: Object.keys(outputs.documents).sort(),
    });
    const hash = await blake3(hashInput);

    return {
      ...outputs,
      hash,
      documentCount: validated.length,
      quadrantCounts: Object.fromEntries(
        Array.from(quadrantGroups.entries()).map(([q, docs]) => [q, docs.length])
      ),
    };
  }

  /**
   * Get quadrant definitions
   *
   * @returns {Object} Quadrant definitions
   */
  getQuadrantDefinitions() {
    return this.quadrants;
  }

  /**
   * Verify render determinism
   *
   * @param {Array<Object>} documents - Documents
   * @param {string} expectedHash - Expected hash
   * @returns {Promise<boolean>} True if hash matches
   */
  async verifyDeterminism(documents, expectedHash) {
    const result = await this.render(documents);
    return result.hash === expectedHash;
  }
}

export default DiataxisRenderer;
