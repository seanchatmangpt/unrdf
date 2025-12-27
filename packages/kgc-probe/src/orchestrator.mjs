#!/usr/bin/env node
/**
 * @fileoverview Orchestrator for KGC Probe - deterministic merge of agent outputs
 *
 * Orchestrator = coordinator that collects observations from all agents and produces
 * deterministic, verifiable output (RDF + JSON index + receipt chain).
 *
 * Design principles:
 * - Deterministic: Same inputs → same outputs (stable sorting, hashing)
 * - Composable: Agents work independently, orchestrator merges
 * - Verifiable: Receipt chain proves integrity
 * - Scalable: Can process outputs from 10+ agents efficiently
 */

import { writeFile, mkdir } from 'node:fs/promises';
import { join } from 'node:path';
import { validateObservation } from './observation.mjs';
import { buildReceiptChain, createManifest, verifyReceiptChain } from './receipt.mjs';
import { GuardManager } from './guards.mjs';
import { convertToTurtle } from './reporters/rdf.mjs';
import { renderReport } from './reporters/markdown.mjs';

/**
 * Orchestrator configuration
 *
 * @typedef {Object} OrchestratorConfig
 * @property {string} outputDir - Output directory for results
 * @property {string[]} allowedRoots - Allowed filesystem roots
 * @property {string[]} allowedHosts - Allowed network hosts
 * @property {number} budgetMs - Time budget per agent (ms)
 * @property {number} samples - Benchmark sample count
 */

/**
 * Orchestrator for multi-agent probe execution
 */
export class Orchestrator {
  /**
   * @param {OrchestratorConfig} config - Orchestrator configuration
   */
  constructor(config) {
    this.config = config;
    this.guards = new GuardManager({
      allowedRoots: config.allowedRoots,
      allowedHosts: config.allowedHosts,
      defaultTimeoutMs: config.budgetMs
    });

    /** @type {import('./observation.mjs').Observation[]} */
    this.observations = [];

    /** @type {Map<string, any>} */
    this.agentOutputs = new Map();
  }

  /**
   * Register output from an agent
   *
   * @param {string} agentId - Agent identifier
   * @param {import('./observation.mjs').Observation[]} observations - Observations from agent
   */
  registerAgentOutput(agentId, observations) {
    // Validate all observations
    const validated = observations.map(obs => validateObservation(obs));

    this.agentOutputs.set(agentId, validated);
    this.observations.push(...validated);
  }

  /**
   * Merge all agent outputs deterministically
   *
   * Stable sort order:
   * 1. Category (alphabetical)
   * 2. Severity (fatal > error > warn > info > debug > trace)
   * 3. File path (alphabetical)
   * 4. Line number (ascending)
   * 5. Agent ID (alphabetical)
   * 6. Timestamp (ascending)
   *
   * @returns {import('./observation.mjs').Observation[]} - Sorted observations
   */
  mergeObservations() {
    const severityOrder = {
      fatal: 0,
      error: 1,
      warn: 2,
      info: 3,
      debug: 4,
      trace: 5
    };

    // Add guard denials
    const guardDenials = this.guards.getDenials();
    const allObservations = [...this.observations, ...guardDenials];

    // Stable sort
    return allObservations.slice().sort((a, b) => {
      // 1. Category
      if (a.category !== b.category) {
        return a.category.localeCompare(b.category);
      }

      // 2. Severity
      if (a.severity !== b.severity) {
        return severityOrder[a.severity] - severityOrder[b.severity];
      }

      // 3. File path
      const aFile = a.location?.file || '';
      const bFile = b.location?.file || '';
      if (aFile !== bFile) {
        return aFile.localeCompare(bFile);
      }

      // 4. Line number
      const aLine = a.location?.line || 0;
      const bLine = b.location?.line || 0;
      if (aLine !== bLine) {
        return aLine - bLine;
      }

      // 5. Agent ID
      if (a.metadata.agentId !== b.metadata.agentId) {
        return a.metadata.agentId.localeCompare(b.metadata.agentId);
      }

      // 6. Timestamp
      return a.metadata.timestamp.localeCompare(b.metadata.timestamp);
    });
  }

  /**
   * Generate RDF/Turtle representation of observations
   *
   * Uses the RDF reporter to convert observations to Turtle format
   * with derived capabilities and constraints.
   *
   * @param {import('./observation.mjs').Observation[]} observations
   * @returns {Promise<string>} - Turtle content
   */
  async generateTurtle(observations) {
    return await convertToTurtle(observations);
  }

  /**
   * Generate Markdown report from observations
   *
   * Uses the Markdown reporter to create a human-readable report.
   *
   * @param {import('./observation.mjs').Observation[]} observations
   * @returns {string} - Markdown content
   */
  generateMarkdownReport(observations) {
    return renderReport(observations);
  }

  /**
   * Write all outputs to disk
   *
   * @returns {Promise<void>}
   */
  async writeOutputs() {
    // Ensure output directory exists
    await mkdir(this.config.outputDir, { recursive: true });

    // Merge and sort observations
    const merged = this.mergeObservations();

    // Build receipt chain
    const receipts = buildReceiptChain(merged);
    const manifest = createManifest(receipts);

    // Verify chain integrity (sanity check)
    if (!verifyReceiptChain(receipts)) {
      throw new Error('Receipt chain verification failed (internal error)');
    }

    // Attach receipt hashes to observations
    for (let i = 0; i < merged.length; i++) {
      merged[i].receiptHash = receipts[i].hash;
    }

    // Write JSON index
    const indexPath = join(this.config.outputDir, 'observations.json');
    await writeFile(indexPath, JSON.stringify(merged, null, 2), 'utf-8');

    // Write Turtle/RDF
    const turtlePath = join(this.config.outputDir, 'observations.ttl');
    const turtle = await this.generateTurtle(merged);
    await writeFile(turtlePath, turtle, 'utf-8');

    // Write Markdown report
    const reportPath = join(this.config.outputDir, 'report.md');
    const markdownReport = this.generateMarkdownReport(merged);
    await writeFile(reportPath, markdownReport, 'utf-8');

    // Write receipts
    const receiptsPath = join(this.config.outputDir, 'receipts.json');
    await writeFile(receiptsPath, JSON.stringify(receipts, null, 2), 'utf-8');

    // Write manifest
    const manifestPath = join(this.config.outputDir, 'manifest.json');
    await writeFile(manifestPath, JSON.stringify(manifest, null, 2), 'utf-8');

    // Write guard statistics
    const statsPath = join(this.config.outputDir, 'guard-stats.json');
    const guardStats = this.guards.getStats();
    await writeFile(statsPath, JSON.stringify(guardStats, null, 2), 'utf-8');

    return {
      observationCount: merged.length,
      receiptCount: receipts.length,
      manifestHash: manifest.chainHash,
      guardDenials: guardStats.totalDenials,
      files: {
        observations: indexPath,
        turtle: turtlePath,
        report: reportPath,
        receipts: receiptsPath,
        manifest: manifestPath,
        guardStats: statsPath
      }
    };
  }

  /**
   * Get guard manager (for agents to check access)
   *
   * @returns {GuardManager}
   */
  getGuardManager() {
    return this.guards;
  }
}

/**
 * Escape string for Turtle output
 *
 * @param {string} str
 * @returns {string}
 */
function escapeString(str) {
  return str
    .replace(/\\/g, '\\\\')
    .replace(/"/g, '\\"')
    .replace(/\n/g, '\\n')
    .replace(/\r/g, '\\r')
    .replace(/\t/g, '\\t');
}

/**
 * Create orchestrator from CLI options
 *
 * @param {Object} options - CLI options
 * @returns {Orchestrator}
 */
export function createOrchestrator(options) {
  return new Orchestrator({
    outputDir: options.out,
    allowedRoots: options.root || [process.cwd()],
    allowedHosts: options.netAllow || [],
    budgetMs: options.budgetMs || 5000,
    samples: options.samples || 10
  });
}

export default {
  Orchestrator,
  createOrchestrator
};
