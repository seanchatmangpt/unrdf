/**
 * 80/20 Fill Gaps Workflow - Capability Completion Process
 *
 * @fileoverview Comprehensive workflow for identifying and completing incomplete
 * capabilities in a codebase using 80/20 thinking (quality-first approach).
 *
 * The workflow systematically:
 * 1. Scans codebase for incomplete capabilities
 * 2. Prioritizes by 80/20 (value = quality + consistency + maintainability)
 * 3. Completes high-impact capabilities
 * 4. Validates implementations
 * 5. Determines strategic next steps
 *
 * @module workflows/80-20-fill-gaps
 * @version 2.0.0
 * @license MIT
 */

/**
 * Workflow Definition: 80/20 Fill Gaps
 *
 * @typedef {Object} Gap
 * @property {string} id - Unique gap identifier
 * @property {string} name - Gap name
 * @property {string} category - Category (error-handling, type-safety, validation, testing, adoption, documentation)
 * @property {string} file - File location with line numbers
 * @property {string} description - What is missing/incomplete
 * @property {string} impact - HIGH, MEDIUM, LOW
 * @property {string} effort - quick-win, moderate, complex
 * @property {number} priority - 1-10 (higher = more important)
 * @property {string} status - pending, in-progress, completed, deferred
 * @property {string[]} dependencies - Other gaps this depends on
 */

export const workflow = {
  id: '80-20-fill-gaps-workflow',
  name: '80/20 Fill Gaps - Capability Completion',
  version: '2.0.0',
  description: `
    Comprehensive workflow for identifying and completing incomplete capabilities
    in a codebase using 80/20 thinking. Focuses on high-impact, quality-first
    improvements that deliver 80% of value with 20% of effort.

    Quality-First Principle:
    - Quality (prevents defects) = HIGH VALUE
    - Consistency (maintainability) = HIGH VALUE
    - Efficiency (prevents waste) = HIGH VALUE
    - These are not optional, they are part of the 80% value

    DfLSS Alignment:
    - Design for Lean Six Sigma addresses BOTH quality AND efficiency
    - Prevent defects (Six Sigma) AND prevent waste (Lean) from the start
    - Not optional - foundational for sustainable development
  `,

  /**
   * Workflow Steps
   * @type {Array<{id: string, name: string, description: string, action: string, outputs: string[]}>}
   */
  steps: [
    {
      id: 'step-1-scan',
      name: 'Step 1: 80/20 Scan',
      description: 'Rapidly scan codebase to identify incomplete capabilities',
      action: 'explore',
      inputs: {
        codebasePath: '.',
        targetFiles: ['src/**/*.mjs', 'test/**/*.test.mjs'],
        patterns: [
          'TODO',
          'FIXME',
          'unimplemented',
          'incomplete',
          'partial',
          'placeholder',
          'stub',
        ],
      },
      outputs: ['gaps-discovered', 'codebase-snapshot'],
      criteria: {
        success: 'Identified at least 3 incomplete capabilities',
        timeLimit: 300, // seconds
      },
    },

    {
      id: 'step-2-identify',
      name: 'Step 2: Identify Incomplete Capabilities',
      description: 'Categorize gaps and identify patterns by capability',
      action: 'analyze',
      inputs: {
        discoveredGaps: 'step-1-scan.gaps-discovered',
        categories: [
          'error-handling',
          'type-safety',
          'validation',
          'testing',
          'adoption',
          'documentation',
        ],
      },
      outputs: ['capability-inventory', 'gap-categories'],
      criteria: {
        success: 'Documented all gaps with categories and locations',
      },
    },

    {
      id: 'step-3-prioritize',
      name: 'Step 3: 80/20 Prioritization',
      description: 'Prioritize by impact and value (quality-first approach)',
      action: 'prioritize',
      inputs: {
        gaps: 'step-2-identify.capability-inventory',
        valueFramework: {
          highImpactHighValue: 'Complete first',
          highImpactMediumValue: 'Plan carefully',
          lowImpactHighValue: 'Do when convenient',
          lowImpactLowValue: 'Skip',
        },
        valueDimensions: ['quality', 'consistency', 'maintainability', 'prevention'],
      },
      outputs: ['prioritized-gaps', '80-20-matrix'],
      criteria: {
        success: 'Top 20% of gaps selected (target: deliver 80% of value)',
        deliveryTarget: '80%',
      },
    },

    {
      id: 'step-4-plan',
      name: 'Step 4: Implementation Planning',
      description: 'Create detailed plan for completing selected gaps',
      action: 'plan',
      inputs: {
        selectedGaps: 'step-3-prioritize.prioritized-gaps',
        batchStrategy: 'related-capabilities',
      },
      outputs: ['implementation-plan', 'batch-assignments', 'success-criteria'],
      criteria: {
        success: 'Detailed plan with clear success metrics for each gap',
      },
    },

    {
      id: 'step-5-implement',
      name: 'Step 5: Implement Capabilities',
      description: 'Complete gaps in parallel when independent',
      action: 'implement',
      inputs: {
        plan: 'step-4-plan.implementation-plan',
        batches: 'step-4-plan.batch-assignments',
        quality: {
          requireTests: true,
          requireDocs: true,
          requireTypes: true,
          lintRequired: true,
        },
      },
      outputs: ['completed-gaps', 'implementation-log'],
      criteria: {
        success: 'All selected gaps completed with quality standards met',
        qualityChecks: [
          'Type safety: 100% JSDoc/Zod coverage',
          'Testing: All edge cases covered',
          'Linting: 0 errors introduced',
          'Documentation: Clear usage examples',
        ],
      },
    },

    {
      id: 'step-6-validate',
      name: 'Step 6: Validation',
      description: 'Verify implementations are complete and correct',
      action: 'validate',
      inputs: {
        implementations: 'step-5-implement.completed-gaps',
        checks: [
          'functional-test',
          'integration-test',
          'edge-cases',
          'error-paths',
          'type-safety',
          'performance',
        ],
      },
      outputs: ['validation-results', 'quality-report'],
      criteria: {
        success: 'All validations pass, 0 regressions',
        testCoverage: '>=80%',
        typesCovered: '100%',
      },
    },

    {
      id: 'step-7-next-steps',
      name: 'Step 7: Determine Next Steps',
      description: 'Assess completion and plan remaining work',
      action: 'assess',
      inputs: {
        completedGaps: 'step-5-implement.completed-gaps',
        validationResults: 'step-6-validate.validation-results',
        allDiscoveredGaps: 'step-1-scan.gaps-discovered',
      },
      outputs: ['next-steps-plan', 'roadmap', 'completion-report'],
      criteria: {
        success: 'Clear plan for remaining work and strategic direction',
      },
    },
  ],

  /**
   * Decision Points in workflow
   */
  decisions: [
    {
      id: 'all-gaps-found',
      question: 'Were enough gaps identified (>=3)?',
      trueAction: 'proceed-to-identify',
      falseAction: 'expand-search',
    },
    {
      id: 'gaps-categorized',
      question: 'Are all gaps properly categorized?',
      trueAction: 'proceed-to-prioritize',
      falseAction: 'recategorize-gaps',
    },
    {
      id: 'top-20-percent-selected',
      question: 'Is top 20% selected (80% value target)?',
      trueAction: 'proceed-to-plan',
      falseAction: 'reconsider-priorities',
    },
    {
      id: 'plan-complete',
      question: 'Is implementation plan detailed and clear?',
      trueAction: 'proceed-to-implement',
      falseAction: 'refine-plan',
    },
    {
      id: 'all-quality-criteria-met',
      question: 'Do all implementations meet quality standards?',
      trueAction: 'proceed-to-validation',
      falseAction: 'fix-quality-issues',
    },
    {
      id: 'all-validations-pass',
      question: 'Do all validations pass with 0 regressions?',
      trueAction: 'proceed-to-next-steps',
      falseAction: 'fix-failures',
    },
    {
      id: 'continue-gaps',
      question: 'Should we work on remaining gaps now?',
      trueAction: 'prioritize-next-batch',
      falseAction: 'document-and-close',
    },
  ],

  /**
   * Capability Categories
   */
  categories: [
    {
      id: 'error-handling',
      name: 'Error Handling',
      description: 'Incomplete error handling, missing error paths',
      examples: [
        'try/catch without proper error propagation',
        'Missing error handling in async functions',
        'Error paths not fully tested',
      ],
      valueType: 'prevents-defects', // Prevents silent failures
    },
    {
      id: 'type-safety',
      name: 'Type Safety',
      description: 'Incomplete type safety, missing type annotations',
      examples: [
        'Plain objects instead of Zod schemas',
        'Missing JSDoc type annotations',
        'Runtime validation where types could be used',
      ],
      valueType: 'prevents-defects', // Prevents type errors
    },
    {
      id: 'validation',
      name: 'Validation',
      description: 'Incomplete input/output validation',
      examples: [
        'Missing Zod validation schemas',
        'Missing API response validation',
        'Incomplete input validation',
      ],
      valueType: 'prevents-defects', // Prevents bad data
    },
    {
      id: 'testing',
      name: 'Testing',
      description: 'Incomplete test coverage',
      examples: [
        'Code without tests',
        'Missing error path tests',
        'Missing integration tests',
        'Insufficient edge case coverage',
      ],
      valueType: 'prevents-defects', // Prevents regressions
    },
    {
      id: 'adoption',
      name: 'Adoption',
      description: 'Incomplete adoption of project patterns',
      examples: [
        'Zod schemas only in tests (not production)',
        'Inconsistent language use (JS in JS project)',
        'Pattern inconsistency',
      ],
      valueType: 'improves-consistency', // Maintains consistency
    },
    {
      id: 'documentation',
      name: 'Documentation',
      description: 'Missing or incomplete documentation',
      examples: [
        'Missing JSDoc comments',
        'Incomplete parameter documentation',
        'Missing usage examples',
      ],
      valueType: 'improves-maintainability', // Prevents misuse
    },
  ],

  /**
   * Value Framework (Quality-First 80/20)
   *
   * Value = Quality + Consistency + Maintainability (not just features)
   * - Quality: Code that works correctly, handles errors, follows patterns
   * - Consistency: Uses project language/patterns, maintains conventions
   * - Maintainability: Easy to understand, modify, and extend
   * - Prevention: Prevents defects and waste rather than fixing later
   */
  valueFramework: {
    dimensions: [
      {
        id: 'quality',
        name: 'Quality',
        description: 'Prevents defects and silent failures',
        weight: 0.4,
      },
      {
        id: 'consistency',
        name: 'Consistency',
        description: 'Maintains project patterns and conventions',
        weight: 0.3,
      },
      {
        id: 'maintainability',
        name: 'Maintainability',
        description: 'Enables easy understanding and modification',
        weight: 0.2,
      },
      {
        id: 'prevention',
        name: 'Prevention',
        description: 'Prevents waste and future problems',
        weight: 0.1,
      },
    ],

    matrix: {
      highImpactHighValue: {
        label: 'Quality Work (Do First)',
        description: 'High impact + high value (quality, consistency, maintainability)',
        approach: 'Complete immediately with full quality standards',
        examples: 'Type safety, error handling, validation',
      },
      highImpactMediumValue: {
        label: 'Good Work (Plan)',
        description: 'High impact + medium value (good but requires more effort)',
        approach: 'Plan carefully, may require iteration',
        examples: 'Comprehensive schemas, extensive tests',
      },
      lowImpactHighValue: {
        label: 'Foundation Work (Convenient)',
        description: 'Lower impact + high value (quality foundations)',
        approach: 'Do when convenient, prevents future problems',
        examples: 'Documentation, examples, patterns',
      },
      lowImpactLowValue: {
        label: 'Avoid',
        description: 'Low impact + low value',
        approach: 'Skip, not worth the effort',
        examples: 'Cosmetic changes, redundant features',
      },
    },
  },

  /**
   * Success Criteria (Quality-First)
   */
  successCriteria: {
    workflow: {
      '80-percent-value-delivered': 'Top 20% of gaps complete, delivering 80% of value',
      'zero-regressions': 'All previously passing tests still pass',
      'quality-standards-met': 'Type safety, testing, documentation complete',
      'strategic-clarity': 'Clear plan for remaining work',
    },

    implementation: {
      'type-safety-100-percent': '100% JSDoc types or Zod schemas on public APIs',
      'test-coverage-80-plus': 'Minimum 80% test coverage on new code',
      'documentation-complete': 'All public functions have JSDoc with examples',
      'linting-errors-zero': 'No new linting errors introduced',
      'error-paths-covered': 'All error paths tested',
      'edge-cases-covered': 'Known edge cases documented and tested',
    },

    validation: {
      'functional-tests-pass': '100% of functional tests pass',
      'integration-tests-pass': 'All integration tests pass',
      'no-performance-regression': 'No significant performance degradation',
      'manual-verification': 'Manual testing confirms expected behavior',
      'code-review-approval': 'Code review completed and approved',
    },
  },

  /**
   * Metrics and Tracking
   */
  metrics: {
    baseline: {
      gapsIdentified: 0,
      gapsCompleted: 0,
      coverageImprovement: 0,
      qualityScore: 0,
    },

    tracked: [
      {
        name: 'gaps-identified',
        unit: 'count',
        target: '>=3',
      },
      {
        name: 'gaps-completed',
        unit: 'count',
        target: '20% of identified',
      },
      {
        name: 'value-delivered',
        unit: 'percent',
        target: '>=80%',
      },
      {
        name: 'test-coverage',
        unit: 'percent',
        target: '>=80%',
      },
      {
        name: 'type-coverage',
        unit: 'percent',
        target: '100%',
      },
      {
        name: 'regressions',
        unit: 'count',
        target: '0',
      },
    ],
  },

  /**
   * Tools and Scripts
   */
  tools: {
    scanning: {
      description: 'Find incomplete capabilities',
      command: 'grep -r "TODO|FIXME|unimplemented" src/ test/',
      agent: 'explorer',
    },

    analysis: {
      description: 'Analyze discovered gaps',
      command: 'Task("analyze-gaps", "Categorize gaps...", "code-analyzer")',
      agent: 'analyzer',
    },

    planning: {
      description: 'Create implementation plan',
      command: 'Task("plan-implementation", "Plan...", "planner")',
      agent: 'planner',
    },

    implementation: {
      description: 'Implement selected gaps',
      command: 'Task("implement-gaps", "Complete...", "coder")',
      agent: 'coder',
    },

    testing: {
      description: 'Validate implementations',
      command: 'pnpm test && pnpm lint && pnpm format:check',
      agent: 'tester',
    },

    review: {
      description: 'Review completed work',
      command: 'Task("review-work", "Review...", "reviewer")',
      agent: 'reviewer',
    },
  },

  /**
   * Workflow Example Execution
   *
   * EXAMPLE: 80/20 Fill Gaps for UNRDF Monorepo
   *
   * Timeline:
   *   Step 1 (Scan): 15 minutes
   *   Step 2 (Identify): 15 minutes
   *   Step 3 (Prioritize): 10 minutes
   *   Step 4 (Plan): 20 minutes
   *   Step 5 (Implement): 90 minutes (parallel: 4 capabilities)
   *   Step 6 (Validate): 15 minutes
   *   Step 7 (Next Steps): 10 minutes
   * Total: ~175 minutes (3 hours)
   *
   * Results:
   *   - 6 gaps identified
   *   - 4 gaps completed (top 20%)
   *   - 80% of value delivered
   *   - 0 regressions
   *   - 231/231 core tests passing
   */
  example: {
    codebase: 'UNRDF Monorepo',
    sourceFiles: 358,
    testFiles: 112,
    gapsFound: 6,
    gapsCompleted: 4,
    valueDelivered: '80%',
    timeTaken: '3 hours',
    results: {
      typeSafety: 'Enhanced via Zod schemas',
      testing: 'All core tests pass (231/231)',
      documentation: 'Complete JSDoc coverage',
      qualityImprovement: '30% (type safety + validation + documentation)',
    },
  },

  /**
   * Integration Points
   */
  integrations: {
    andonSignals: {
      description: 'Verify Andon signals cleared after completion',
      workflow: '/andon-signals',
      trigger: 'after-implementation',
    },

    rootCauseAnalysis: {
      description: 'Use 5 Whys to understand why gaps exist',
      workflow: '/root-cause-analysis',
      trigger: 'during-identify',
    },

    dmaic: {
      description: 'Use DMAIC for measurement and control',
      workflow: '/dmaic-problem-solving',
      trigger: 'during-validate',
    },

    pokaYoke: {
      description: 'Use error-proofing design principles',
      workflow: '/poka-yoke-design',
      trigger: 'during-implement',
    },
  },

  /**
   * Documentation
   */
  documentation: {
    overview: '/80-20-fill-gaps.md',
    andonSignals: '/docs/ANDON_SIGNALS.md',
    completionReport: '/docs/COMPLETION_REPORT_2025_12_20.md',
    signalChecker: '/scripts/check-andon-signals.mjs',
  },
};

/**
 * Workflow Execution Helper
 *
 * @param {Object} config - Workflow configuration
 * @returns {Promise<Object>} Workflow results
 */
export async function executeWorkflow(config = {}) {
  const {
    codebasePath = '.',
    selectTop = 0.2, // Top 20%
    requireQualityStandards = true,
    parallelImplementation = true,
  } = config;

  return {
    workflow: workflow.id,
    config: {
      codebasePath,
      selectTop: `${selectTop * 100}%`,
      requireQualityStandards,
      parallelImplementation,
    },
    status: 'ready-to-execute',
    nextStep: 'step-1-scan',
    instructions: 'Run /80-20-fill-gaps command to begin workflow',
  };
}

/**
 * Export workflow for CI/CD integration
 *
 * @param {string} format - Export format (yaml, json, mermaid)
 * @returns {string} Formatted workflow definition
 */
export function exportWorkflow(format = 'json') {
  switch (format) {
    case 'yaml':
      return generateYamlWorkflow(workflow);
    case 'mermaid':
      return generateMermaidDiagram(workflow);
    case 'json':
    default:
      return JSON.stringify(workflow, null, 2);
  }
}

function generateYamlWorkflow(wf) {
  return `
name: ${wf.name}
version: ${wf.version}
description: ${wf.description}

steps:
${wf.steps.map((s) => `  - id: ${s.id}
    name: ${s.name}
    action: ${s.action}`).join('\n')}

decisions:
${wf.decisions.map((d) => `  - id: ${d.id}
    question: ${d.question}
    true_action: ${d.trueAction}
    false_action: ${d.falseAction}`).join('\n')}
  `.trim();
}

function generateMermaidDiagram(wf) {
  return `
graph TD
    Start([Start: 80/20 Fill Gaps])
    Scan["Step 1: Scan (Find gaps)"]
    Identify["Step 2: Identify (Categorize)"]
    Prioritize["Step 3: Prioritize (80/20)"]
    Plan["Step 4: Plan"]
    Implement["Step 5: Implement (Parallel)"]
    Validate["Step 6: Validate"]
    NextSteps["Step 7: Next Steps"]
    End([Complete])

    Start --> Scan
    Scan --> Identify
    Identify --> Prioritize
    Prioritize --> Plan
    Plan --> Implement
    Implement --> Validate
    Validate --> NextSteps
    NextSteps --> End

    style Implement fill:#90EE90
    style Validate fill:#FFB6C1
    style Prioritize fill:#87CEEB
  `.trim();
}

export default workflow;
