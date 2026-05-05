/**
 * @file Capability Documentation Generator
 * @module kgc-claude/capabilities/capability-docs
 *
 * @description
 * Auto-generates Diataxis-style documentation from capability metadata.
 * Produces tutorials, how-to guides, reference docs, and explanations.
 */

import { createCapabilityMap } from './capability-map.mjs';
import { createCapabilityRegistry } from './capability-api.mjs';

/* ========================================================================= */
/* Documentation Generator                                                   */
/* ========================================================================= */

/**
 * CapabilityDocsGenerator - Generate Diataxis documentation
 */
export class CapabilityDocsGenerator {
  constructor() {
    this.map = createCapabilityMap();
    this.registry = createCapabilityRegistry();
  }

  /**
   * Generate tutorial for capability
   * @param {string} capabilityId - Capability ID
   * @returns {string} Markdown tutorial
   */
  generateTutorial(capabilityId) {
    const cap = this.map.getCapability(capabilityId);
    if (!cap) {
      throw new Error(`Capability not found: ${capabilityId}`);
    }

    const implementations = this.registry.getImplementationsForCapability(capabilityId);
    const examples = this.registry.getExamples();

    return `# Tutorial: Getting Started with ${cap.name}

## Overview

${cap.description}

## What You'll Learn

- Understand ${cap.name} fundamentals
- Set up your first ${cap.name.toLowerCase()} workflow
- Execute a simple example
- Verify it works

## Prerequisites

- Claude Code CLI or VS Code extension installed
- Basic understanding of ${cap.category} concepts

## Step-by-Step Guide

### Step 1: Understanding ${cap.name}

${cap.name} provides the following primitives:

${cap.primitives.map(p => `- **${p}**`).join('\n')}

${cap.docRef ? `\nFor complete reference, see: ${cap.docRef}\n` : ''}

### Step 2: Implementation Setup

${implementations.length > 0 ? `
The following implementations are available:

${implementations.map(impl => `- **${impl.name}**: ${impl.description}`).join('\n')}

#### Using the Implementation

\`\`\`javascript
import { createCapabilityRegistry } from '@unrdf/kgc-claude/capabilities';

const registry = createCapabilityRegistry();
const ${capabilityId} = registry.initialize('${implementations[0]?.name}');
\`\`\`
` : `
${capabilityId} is a primitive capability provided by Claude Code.
Configure it via:

- **CLI**: Command-line flags and configuration files
- **VS Code**: Extension settings
- **Project**: \`.claude/\` directory configurations
`}

### Step 3: Hello World Example

${this._getHelloWorldExample(cap, implementations)}

### Step 4: Verify It Works

Run the example and check for:

- [ ] No errors in output
- [ ] Expected behavior matches description
- [ ] Results are reproducible

## Next Steps

- Explore [how-to guides](#) for specific workflows
- Read [reference documentation](#) for all options
- Understand [why ${cap.name} matters](#) in explanations

## Troubleshooting

**Q: The example doesn't work**
A: Check that you have the latest Claude Code version

**Q: How do I customize behavior?**
A: See the reference documentation for configuration options

## Related Capabilities

${this._getRelatedCapabilities(capabilityId)}
`;
  }

  /**
   * Generate how-to guide for common workflow
   * @param {string} capabilityId - Capability ID
   * @param {string} workflow - Workflow name
   * @returns {string} Markdown how-to guide
   */
  generateHowTo(capabilityId, workflow) {
    const cap = this.map.getCapability(capabilityId);
    if (!cap) {
      throw new Error(`Capability not found: ${capabilityId}`);
    }

    const workflows = this._getWorkflowsForCapability(capabilityId);
    const workflowConfig = workflows[workflow] || this._getDefaultWorkflow(cap);

    return `# How To: ${workflowConfig.title}

## Problem

${workflowConfig.problem}

## Solution

${workflowConfig.solution}

## Prerequisites

- ${cap.name} configured
${workflowConfig.prerequisites?.map(p => `- ${p}`).join('\n') || ''}

## Steps

${workflowConfig.steps.map((step, i) => `
### ${i + 1}. ${step.title}

${step.description}

${step.code ? `\`\`\`${step.lang || 'bash'}\n${step.code}\n\`\`\`` : ''}
`).join('\n')}

## Verification

${workflowConfig.verification}

## Common Issues

${workflowConfig.issues?.map(issue => `
**${issue.problem}**
${issue.solution}
`).join('\n') || 'No common issues documented yet.'}

## See Also

${this._getRelatedHowTos(capabilityId)}
`;
  }

  /**
   * Generate reference documentation
   * @param {string} capabilityId - Capability ID
   * @returns {string} Markdown reference
   */
  generateReference(capabilityId) {
    const cap = this.map.getCapability(capabilityId);
    if (!cap) {
      throw new Error(`Capability not found: ${capabilityId}`);
    }

    const implementations = this.registry.getImplementationsForCapability(capabilityId);
    const dependencies = this.map.getDependencies(capabilityId);

    return `# Reference: ${cap.name}

## Description

${cap.description}

**Category:** ${cap.category}

${cap.docRef ? `**Official Docs:** ${cap.docRef}\n` : ''}

## Primitives

${cap.primitives.map(p => `### ${p}\n\n(Description to be added)\n`).join('\n')}

## Implementations

${implementations.length > 0 ? implementations.map(impl => `
### ${impl.name}

${impl.description}

**Status:** ${impl.initialized ? 'Initialized' : 'Available'}

#### Initialization

\`\`\`javascript
import { createCapabilityRegistry } from '@unrdf/kgc-claude/capabilities';

const registry = createCapabilityRegistry();
const instance = registry.initialize('${impl.name}', {
  // configuration options
});
\`\`\`
`).join('\n') : 'No implementations available in this package.'}

## Dependencies

${dependencies.length > 0 ? `
### Outgoing Dependencies (Requires)

${dependencies.filter(d => d.from === capabilityId).map(d => `
- **${this.map.getCapability(d.to)?.name}** (${d.strength})
  ${d.relationship}
`).join('\n')}

### Incoming Dependencies (Enhanced By)

${dependencies.filter(d => d.to === capabilityId).map(d => `
- **${this.map.getCapability(d.from)?.name}** (${d.strength})
  ${d.relationship}
`).join('\n')}
` : 'No documented dependencies.'}

## Compositions

${this._getCompositionsForCapability(capabilityId)}

## Configuration

(To be documented based on implementation)

## API Reference

(To be documented based on implementation)
`;
  }

  /**
   * Generate explanation
   * @param {string} capabilityId - Capability ID
   * @returns {string} Markdown explanation
   */
  generateExplanation(capabilityId) {
    const cap = this.map.getCapability(capabilityId);
    if (!cap) {
      throw new Error(`Capability not found: ${capabilityId}`);
    }

    const compositions = this.map.getCompositionsForCapability(capabilityId);

    return `# Explanation: Why ${cap.name} Matters

## The Problem Space

(Context: What problem does ${cap.name} solve?)

## Design Decisions

### Why ${cap.category}?

${cap.category === 'execution' ? 'Execution capabilities focus on running tasks efficiently and reliably.' : ''}
${cap.category === 'control' ? 'Control capabilities provide governance and policy enforcement.' : ''}
${cap.category === 'safety' ? 'Safety capabilities enable risk-taking with recovery mechanisms.' : ''}
${cap.category === 'automation' ? 'Automation capabilities reduce manual steps and increase reproducibility.' : ''}
${cap.category === 'integration' ? 'Integration capabilities connect to external systems and data sources.' : ''}
${cap.category === 'interface' ? 'Interface capabilities provide rich user experiences and workflows.' : ''}
${cap.category === 'extension' ? 'Extension capabilities enable sharing and reuse of configurations.' : ''}
${cap.category === 'capability' ? 'Capability management enables dynamic, context-aware features.' : ''}

## How It Works

${cap.name} operates through the following primitives:

${cap.primitives.map(p => `
### ${p}

(Detailed explanation of how ${p} works)
`).join('\n')}

## Composition Benefits

${compositions.length > 0 ? `
When combined with other capabilities, ${cap.name} enables:

${compositions.map(comp => `
### ${comp.components.filter(c => c !== capabilityId).join(' + ')}

**Emergent Property:** ${comp.emergentProperty}

**Measurable Impact:**
${comp.measurableMetrics.map(m => `- ${m}`).join('\n')}

**Status:** ${comp.verdict || 'pending'}
`).join('\n')}
` : 'Compositions with this capability are still being explored.'}

## Operator Economics

Using ${cap.name} changes operator workflows by:

1. **Reducing Steps:** (Quantify based on compositions)
2. **Increasing Safety:** (Describe guardrails)
3. **Enabling Automation:** (Show machine-readable patterns)
4. **Improving Reproducibility:** (Explain consistency benefits)

## Trade-offs

(Document known limitations and when NOT to use this capability)

## Future Directions

(Speculate on upcoming features or research areas)
`;
  }

  /**
   * Generate full documentation set
   * @returns {Object} All documentation
   */
  generateAll() {
    const capabilities = this.map.getAllCapabilities();

    const docs = {
      tutorials: {},
      howTos: {},
      reference: {},
      explanations: {},
    };

    for (const cap of capabilities) {
      docs.tutorials[cap.id] = this.generateTutorial(cap.id);
      docs.reference[cap.id] = this.generateReference(cap.id);
      docs.explanations[cap.id] = this.generateExplanation(cap.id);
    }

    return docs;
  }

  /* ======================================================================= */
  /* Private Helpers                                                         */
  /* ======================================================================= */

  /**
   * Get hello world example
   * @private
   */
  _getHelloWorldExample(cap, implementations) {
    const examples = this.registry.getExamples();

    if (cap.id === 'hooks') return examples.hookComposer.code;
    if (cap.id === 'tool_permissions') return examples.governance.code;
    if (cap.id === 'ide_surface') return examples.ideIntegration.code;
    if (cap.id === 'checkpointing') return examples.timeTravel.code;

    return `
\`\`\`javascript
// Example for ${cap.name}
// (To be implemented)
\`\`\`
    `.trim();
  }

  /**
   * Get related capabilities
   * @private
   */
  _getRelatedCapabilities(capabilityId) {
    const deps = this.map.getDependencies(capabilityId);
    const related = new Set([...deps.map(d => d.from), ...deps.map(d => d.to)]);
    related.delete(capabilityId);

    if (related.size === 0) return 'No direct dependencies.';

    return Array.from(related)
      .map(id => {
        const cap = this.map.getCapability(id);
        return cap ? `- [${cap.name}](#)` : '';
      })
      .filter(Boolean)
      .join('\n');
  }

  /**
   * Get workflows for capability
   * @private
   */
  _getWorkflowsForCapability(capabilityId) {
    // Placeholder - would be populated from research
    return {};
  }

  /**
   * Get default workflow
   * @private
   */
  _getDefaultWorkflow(cap) {
    return {
      title: `Using ${cap.name} in Your Workflow`,
      problem: `You need to leverage ${cap.name} for your project.`,
      solution: `Configure and use ${cap.name} effectively.`,
      prerequisites: [],
      steps: [
        {
          title: 'Setup',
          description: `Configure ${cap.name} for your needs.`,
          code: '// Configuration code here',
          lang: 'javascript',
        },
        {
          title: 'Execute',
          description: 'Run the workflow.',
          code: '// Execution code here',
          lang: 'bash',
        },
      ],
      verification: 'Check that the workflow completed successfully.',
      issues: [],
    };
  }

  /**
   * Get related how-tos
   * @private
   */
  _getRelatedHowTos(capabilityId) {
    return '(Related how-to guides to be linked)';
  }

  /**
   * Get compositions for capability
   * @private
   */
  _getCompositionsForCapability(capabilityId) {
    const compositions = this.map.getCompositionsForCapability(capabilityId);

    if (compositions.length === 0) {
      return 'No documented compositions yet.';
    }

    return compositions.map(comp => `
### ${comp.components.join(' + ')}

**Emergent Property:** ${comp.emergentProperty}

**Metrics:** ${comp.measurableMetrics.join(', ')}

**Status:** ${comp.tested ? comp.verdict : 'pending'}
    `).join('\n');
  }
}

/* ========================================================================= */
/* Factory Functions                                                         */
/* ========================================================================= */

/**
 * Create documentation generator
 * @returns {CapabilityDocsGenerator}
 */
export function createDocsGenerator() {
  return new CapabilityDocsGenerator();
}

/**
 * Generate documentation for all capabilities
 * @param {Object} [options] - Generation options
 * @returns {Object} Documentation set
 */
export function generateAllDocs(options = {}) {
  const generator = createDocsGenerator();
  return generator.generateAll();
}

/* ========================================================================= */
/* Exports                                                                   */
/* ========================================================================= */

export default {
  CapabilityDocsGenerator,
  createDocsGenerator,
  generateAllDocs,
};
