/**
 * CI Integration - Continuous Integration & Deployment Automation
 *
 * Provides CI/CD integration capabilities for Claude Code automation.
 * Supports GitHub Actions, GitLab CI, test result parsing, and PR automation.
 *
 * @module @unrdf/kgc-claude/capabilities/ci-integration
 */

import { z } from 'zod';
import { createHeadlessRunner } from './headless-runner.mjs';
import { createBatchProcessor, TaskPriority } from './batch-processor.mjs';
import { readFile, writeFile } from 'fs/promises';
import { join } from 'path';

/**
 * CI platform enumeration
 */
export const CIPlatform = {
  GITHUB_ACTIONS: 'github-actions',
  GITLAB_CI: 'gitlab-ci',
  JENKINS: 'jenkins',
  CIRCLECI: 'circleci',
  TRAVIS: 'travis',
};

/**
 * CI event schema
 */
export const CIEventSchema = z.object({
  platform: z.enum(['github-actions', 'gitlab-ci', 'jenkins', 'circleci', 'travis']),
  event: z.string(),
  ref: z.string().optional(),
  sha: z.string().optional(),
  branch: z.string().optional(),
  pr: z.number().int().optional(),
  author: z.string().optional(),
  message: z.string().optional(),
  files: z.array(z.string()).optional(),
  env: z.record(z.string()).optional(),
}).passthrough();

/**
 * Test result schema
 */
export const TestResultSchema = z.object({
  total: z.number(),
  passed: z.number(),
  failed: z.number(),
  skipped: z.number(),
  duration: z.number(),
  passRate: z.number(),
  failures: z.array(z.object({
    name: z.string(),
    error: z.string(),
    stack: z.string().optional(),
  })),
});

/**
 * PR automation config schema
 */
export const PRAutomationConfigSchema = z.object({
  autoReview: z.boolean().default(false),
  autoTest: z.boolean().default(true),
  autoFix: z.boolean().default(false),
  commentOnFailure: z.boolean().default(true),
  requireApproval: z.boolean().default(true),
  labels: z.array(z.string()).default([]),
});

/**
 * CI Integration - Automate Claude Code in CI/CD pipelines
 */
export class CIIntegration {
  /**
   * @param {object} config - CI configuration
   * @param {boolean} [config.debug=false] - Enable debug output
   */
  constructor({ debug = false } = {}) {
    this.debug = debug;
    this.runner = createHeadlessRunner({ debug });
    this.processor = createBatchProcessor({ debug });
  }

  /**
   * Detect CI environment
   *
   * @returns {object|null} CI event details
   */
  detectCIEnvironment() {
    // GitHub Actions
    if (process.env.GITHUB_ACTIONS === 'true') {
      return CIEventSchema.parse({
        platform: CIPlatform.GITHUB_ACTIONS,
        event: process.env.GITHUB_EVENT_NAME || 'unknown',
        ref: process.env.GITHUB_REF,
        sha: process.env.GITHUB_SHA,
        branch: process.env.GITHUB_REF_NAME,
        pr: process.env.GITHUB_EVENT_NAME === 'pull_request'
          ? parseInt(process.env.GITHUB_REF?.split('/')[2] || '0')
          : undefined,
        author: process.env.GITHUB_ACTOR,
        env: {
          repository: process.env.GITHUB_REPOSITORY,
          workflow: process.env.GITHUB_WORKFLOW,
          runId: process.env.GITHUB_RUN_ID,
        },
      });
    }

    // GitLab CI
    if (process.env.GITLAB_CI === 'true') {
      return CIEventSchema.parse({
        platform: CIPlatform.GITLAB_CI,
        event: process.env.CI_PIPELINE_SOURCE || 'unknown',
        ref: process.env.CI_COMMIT_REF_NAME,
        sha: process.env.CI_COMMIT_SHA,
        branch: process.env.CI_COMMIT_BRANCH,
        pr: process.env.CI_MERGE_REQUEST_IID ? parseInt(process.env.CI_MERGE_REQUEST_IID) : undefined,
        author: process.env.GITLAB_USER_LOGIN,
        message: process.env.CI_COMMIT_MESSAGE,
        env: {
          project: process.env.CI_PROJECT_PATH,
          pipeline: process.env.CI_PIPELINE_ID,
        },
      });
    }

    // Jenkins
    if (process.env.JENKINS_URL) {
      return CIEventSchema.parse({
        platform: CIPlatform.JENKINS,
        event: 'build',
        branch: process.env.GIT_BRANCH,
        sha: process.env.GIT_COMMIT,
        env: {
          buildNumber: process.env.BUILD_NUMBER,
          jobName: process.env.JOB_NAME,
        },
      });
    }

    // CircleCI
    if (process.env.CIRCLECI === 'true') {
      return CIEventSchema.parse({
        platform: CIPlatform.CIRCLECI,
        event: 'build',
        branch: process.env.CIRCLE_BRANCH,
        sha: process.env.CIRCLE_SHA1,
        pr: process.env.CIRCLE_PR_NUMBER ? parseInt(process.env.CIRCLE_PR_NUMBER) : undefined,
        author: process.env.CIRCLE_USERNAME,
        env: {
          project: process.env.CIRCLE_PROJECT_REPONAME,
          buildNum: process.env.CIRCLE_BUILD_NUM,
        },
      });
    }

    // Travis CI
    if (process.env.TRAVIS === 'true') {
      return CIEventSchema.parse({
        platform: CIPlatform.TRAVIS,
        event: process.env.TRAVIS_EVENT_TYPE || 'unknown',
        branch: process.env.TRAVIS_BRANCH,
        sha: process.env.TRAVIS_COMMIT,
        pr: process.env.TRAVIS_PULL_REQUEST !== 'false'
          ? parseInt(process.env.TRAVIS_PULL_REQUEST || '0')
          : undefined,
        message: process.env.TRAVIS_COMMIT_MESSAGE,
        env: {
          repo: process.env.TRAVIS_REPO_SLUG,
          buildId: process.env.TRAVIS_BUILD_ID,
        },
      });
    }

    return null;
  }

  /**
   * Run automated code review
   *
   * @param {object} options - Review options
   * @param {Array<string>} options.files - Files to review
   * @param {string} [options.context] - Additional context
   * @returns {Promise<object>} Review result
   */
  async runCodeReview({ files, context = '' }) {
    const fileContents = await Promise.all(
      files.map(async (file) => {
        try {
          const content = await readFile(file, 'utf-8');
          return { file, content };
        } catch {
          return { file, content: null };
        }
      })
    );

    const validFiles = fileContents.filter(f => f.content !== null);

    const prompt = `Review the following code changes and provide feedback:

${context ? `Context: ${context}\n` : ''}
Files changed: ${validFiles.length}

${validFiles.map(({ file, content }) => `
File: ${file}
\`\`\`
${content}
\`\`\`
`).join('\n')}

Provide:
1. Summary of changes
2. Potential issues or bugs
3. Security concerns
4. Performance implications
5. Recommendations

Format as JSON with keys: summary, issues, security, performance, recommendations`;

    const result = await this.runner.execute({
      prompt,
      outputFormat: 'json',
      allowedTools: ['Read', 'Grep'],
      timeout: 300000, // 5 minutes
    });

    return {
      files: validFiles.map(f => f.file),
      review: result.output,
      timestamp: new Date().toISOString(),
    };
  }

  /**
   * Run automated tests and parse results
   *
   * @param {object} options - Test options
   * @param {string} options.command - Test command to run
   * @param {string} [options.outputFile] - Output file path
   * @returns {Promise<object>} Test results
   */
  async runTests({ command, outputFile }) {
    const prompt = `Run the following test command and parse the results:

Command: ${command}

1. Execute the command
2. Parse test output
3. Extract metrics: total, passed, failed, skipped, duration
4. Identify failed test names and errors

Return JSON with structure:
{
  "total": number,
  "passed": number,
  "failed": number,
  "skipped": number,
  "duration": number,
  "passRate": number,
  "failures": [{"name": string, "error": string}]
}`;

    const result = await this.runner.execute({
      prompt,
      outputFormat: 'json',
      allowedTools: ['Bash'],
      dangerouslySkipPermissions: true,
      timeout: 600000, // 10 minutes
    });

    const testResult = TestResultSchema.parse(result.output);

    if (outputFile) {
      await writeFile(outputFile, JSON.stringify(testResult, null, 2));
    }

    return testResult;
  }

  /**
   * Automate pull request workflow
   *
   * @param {object} options - PR automation options
   * @param {number} options.pr - PR number
   * @param {object} [options.config] - Automation config
   * @returns {Promise<object>} Automation result
   */
  async automatePR({ pr, config = {} }) {
    const prConfig = PRAutomationConfigSchema.parse(config);
    const tasks = [];

    if (prConfig.autoTest) {
      tasks.push({
        id: 'run-tests',
        prompt: `Run all tests for PR #${pr} and report results`,
        priority: TaskPriority.HIGH,
        options: {
          allowedTools: ['Bash'],
          dangerouslySkipPermissions: true,
        },
      });
    }

    if (prConfig.autoReview) {
      tasks.push({
        id: 'code-review',
        prompt: `Review code changes in PR #${pr} and identify issues`,
        priority: TaskPriority.NORMAL,
        options: {
          allowedTools: ['Read', 'Grep', 'Bash'],
        },
      });
    }

    if (prConfig.autoFix) {
      tasks.push({
        id: 'auto-fix',
        prompt: `Automatically fix linting and formatting issues in PR #${pr}`,
        priority: TaskPriority.LOW,
        options: {
          allowedTools: ['Edit', 'Bash'],
        },
        dependencies: ['run-tests'],
      });
    }

    this.processor.addTasks(tasks);
    const summary = await this.processor.process();

    return {
      pr,
      tasks: summary.totalTasks,
      completed: summary.completed,
      failed: summary.failed,
      results: Object.fromEntries(summary.results),
      timestamp: new Date().toISOString(),
    };
  }

  /**
   * Generate CI workflow file
   *
   * @param {object} options - Workflow options
   * @param {string} options.platform - CI platform
   * @param {Array<string>} options.steps - Workflow steps
   * @param {string} [options.outputPath] - Output file path
   * @returns {Promise<string>} Workflow content
   */
  async generateWorkflow({ platform, steps, outputPath }) {
    const templates = {
      [CIPlatform.GITHUB_ACTIONS]: this._generateGitHubActionsWorkflow(steps),
      [CIPlatform.GITLAB_CI]: this._generateGitLabCIWorkflow(steps),
    };

    const content = templates[platform];
    if (!content) {
      throw new Error(`Unsupported platform: ${platform}`);
    }

    if (outputPath) {
      await writeFile(outputPath, content);
    }

    return content;
  }

  /**
   * Parse test output from various formats
   *
   * @param {string} output - Test output
   * @param {string} format - Output format (jest, mocha, vitest, etc.)
   * @returns {object} Parsed test results
   */
  parseTestOutput(output, format = 'auto') {
    // Auto-detect format
    if (format === 'auto') {
      if (output.includes('Jest')) format = 'jest';
      else if (output.includes('Vitest')) format = 'vitest';
      else if (output.includes('Mocha')) format = 'mocha';
    }

    // Simple regex-based parsing (can be enhanced)
    const totalMatch = output.match(/(\d+)\s+total/i);
    const passedMatch = output.match(/(\d+)\s+passed/i);
    const failedMatch = output.match(/(\d+)\s+failed/i);
    const skippedMatch = output.match(/(\d+)\s+skipped/i);
    const durationMatch = output.match(/Time:\s+(\d+\.?\d*)\s*s/i);

    const total = totalMatch ? parseInt(totalMatch[1]) : 0;
    const passed = passedMatch ? parseInt(passedMatch[1]) : 0;
    const failed = failedMatch ? parseInt(failedMatch[1]) : 0;
    const skipped = skippedMatch ? parseInt(skippedMatch[1]) : 0;
    const duration = durationMatch ? parseFloat(durationMatch[1]) * 1000 : 0;

    return TestResultSchema.parse({
      total,
      passed,
      failed,
      skipped,
      duration,
      passRate: total > 0 ? (passed / total) * 100 : 0,
      failures: [],
    });
  }

  /**
   * Generate GitHub Actions workflow
   *
   * @private
   */
  _generateGitHubActionsWorkflow(steps) {
    return `name: Claude Code Automation

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main, develop ]

jobs:
  claude-automation:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v3

      - name: Setup Node.js
        uses: actions/setup-node@v3
        with:
          node-version: '18'

      - name: Install Claude Code
        run: npm install -g claude

${steps.map((step, i) => `      - name: ${step}
        run: |
          claude -p "${step}" \\
            --output-format json \\
            --dangerously-skip-permissions \\
            > step-${i}-result.json
`).join('\n')}

      - name: Upload Results
        uses: actions/upload-artifact@v3
        with:
          name: claude-results
          path: step-*-result.json
`;
  }

  /**
   * Generate GitLab CI workflow
   *
   * @private
   */
  _generateGitLabCIWorkflow(steps) {
    return `claude-automation:
  image: node:18

  before_script:
    - npm install -g claude

  script:
${steps.map((step, i) => `    - claude -p "${step}" --output-format json --dangerously-skip-permissions > step-${i}-result.json`).join('\n')}

  artifacts:
    paths:
      - step-*-result.json
    expire_in: 1 week

  only:
    - main
    - develop
    - merge_requests
`;
  }
}

/**
 * Create CI integration instance
 *
 * @param {object} config - CI configuration
 * @returns {CIIntegration} CI integration instance
 */
export function createCIIntegration(config = {}) {
  return new CIIntegration(config);
}

/**
 * Quick CI environment detection
 *
 * @returns {object|null} CI event details
 */
export function detectCI() {
  const ci = createCIIntegration();
  return ci.detectCIEnvironment();
}

/**
 * Quick test runner
 *
 * @param {string} command - Test command
 * @param {string} [outputFile] - Output file path
 * @returns {Promise<object>} Test results
 */
export async function runCITests(command, outputFile) {
  const ci = createCIIntegration();
  return ci.runTests({ command, outputFile });
}
