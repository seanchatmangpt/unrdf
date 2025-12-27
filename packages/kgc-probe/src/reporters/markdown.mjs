/**
 * @fileoverview Markdown reporter for KGC Probe observations
 *
 * Generates human-readable reports from observations with:
 * - Executive summary (platform, runtime, key capabilities)
 * - Capabilities by domain (grouped)
 * - Constraints by domain (grouped)
 * - Performance metrics (tables)
 * - Guard denials (if any)
 * - Provenance (observation count, hash chain)
 *
 * Design principles:
 * - Human-readable: Clear structure, visual hierarchy
 * - Comprehensive: All relevant information included
 * - Actionable: Highlights issues and limitations
 */

import crypto from 'crypto';
import { deriveCapabilities, deriveConstraints } from './rdf.mjs';

/**
 * Generate deterministic hash for observation
 *
 * @param {Object} observation - Observation object
 * @returns {string} SHA-256 hash (first 16 chars)
 */
function generateHash(observation) {
  if (observation.hash) return observation.hash;
  if (observation.receiptHash) return observation.receiptHash;

  const content = JSON.stringify({
    method: observation.method,
    category: observation.category,
    message: observation.message,
    outputs: observation.outputs || observation.data,
    timestamp: observation.timestamp || observation.metadata?.timestamp,
  });

  return crypto.createHash('sha256').update(content).digest('hex').substring(0, 16);
}

/**
 * Extract platform information from observations
 *
 * @param {Array<Object>} observations - Array of observations
 * @returns {Object} Platform info
 */
function extractPlatformInfo(observations) {
  const info = {
    platform: 'unknown',
    runtime: 'unknown',
    arch: 'unknown',
    nodeVersion: 'unknown'
  };

  for (const obs of observations) {
    const data = obs.outputs || obs.data || {};

    // Extract Node.js version
    if (data.nodeVersion) {
      info.nodeVersion = data.nodeVersion;
      info.runtime = `Node.js ${data.nodeVersion}`;
    }

    // Extract platform/arch
    if (data.platform) info.platform = data.platform;
    if (data.arch) info.arch = data.arch;
  }

  return info;
}

/**
 * Group observations by domain
 *
 * @param {Array<Object>} observations - Array of observations
 * @returns {Map<string, Array<Object>>} Observations grouped by domain
 */
function groupByDomain(observations) {
  const byDomain = new Map();

  for (const obs of observations) {
    const domain = obs.domain || obs.category || 'general';
    if (!byDomain.has(domain)) {
      byDomain.set(domain, []);
    }
    byDomain.get(domain).push(obs);
  }

  return byDomain;
}

/**
 * Extract performance metrics from observations
 *
 * @param {Array<Object>} observations - Array of observations
 * @returns {Array<Object>} Performance metrics
 */
function extractPerformanceMetrics(observations) {
  const metrics = [];

  for (const obs of observations) {
    const data = obs.outputs || obs.data || {};

    // Check for performance data
    if (data.mean !== undefined || data.median !== undefined || data.p95 !== undefined) {
      metrics.push({
        method: obs.method || obs.message || 'unknown',
        mean: data.mean,
        median: data.median,
        p95: data.p95,
        min: data.min,
        max: data.max,
        unit: data.unit || 'ms',
        samples: data.samples || 1
      });
    }

    // Check for throughput data
    if (data.throughputMBps !== undefined) {
      metrics.push({
        method: obs.method || obs.message || 'unknown',
        throughput: data.throughputMBps,
        unit: 'MB/s',
        samples: data.samples || 1
      });
    }
  }

  return metrics;
}

/**
 * Extract guard denials from observations
 *
 * @param {Array<Object>} observations - Array of observations
 * @returns {Array<Object>} Guard denials
 */
function extractGuardDenials(observations) {
  const denials = [];

  for (const obs of observations) {
    const data = obs.outputs || obs.data || {};

    if (obs.guardDecision === 'denied' || obs.category === 'guard' || data.guardDecision === 'denied') {
      denials.push({
        method: obs.method || obs.message || 'unknown',
        reason: data.reason || obs.message || 'Access denied',
        guard: data.guardName || 'unknown',
        hash: generateHash(obs)
      });
    }
  }

  return denials;
}

/**
 * Calculate hash chain from observations
 *
 * @param {Array<Object>} observations - Array of observations
 * @returns {string} Chain hash
 */
function calculateChainHash(observations) {
  const hashes = observations.map(obs => generateHash(obs)).sort();
  const content = hashes.join('');
  return crypto.createHash('sha256').update(content).digest('hex').substring(0, 16);
}

/**
 * Render report as Markdown
 *
 * @param {Array<Object>} observations - Array of observation objects
 * @returns {string} Markdown-formatted report
 *
 * @example
 * const report = renderReport(observations);
 * console.log(report); // # KGC Probe Report\n\n## Executive Summary\n...
 */
export function renderReport(observations) {
  // Sort observations for deterministic output
  const sortedObs = [...observations].sort((a, b) => {
    const hashA = generateHash(a);
    const hashB = generateHash(b);
    return hashA.localeCompare(hashB);
  });

  // Extract components
  const platform = extractPlatformInfo(sortedObs);
  const capabilities = deriveCapabilities(sortedObs);
  const constraints = deriveConstraints(sortedObs);
  const byDomain = groupByDomain(sortedObs);
  const perfMetrics = extractPerformanceMetrics(sortedObs);
  const guardDenials = extractGuardDenials(sortedObs);
  const chainHash = calculateChainHash(sortedObs);

  // Calculate run ID from first timestamp
  const firstTimestamp = sortedObs.length > 0
    ? (sortedObs[0].timestamp || sortedObs[0].metadata?.timestamp || Date.now())
    : Date.now();
  const runDate = typeof firstTimestamp === 'number'
    ? new Date(firstTimestamp).toISOString()
    : firstTimestamp;
  const runId = `run_${runDate.replace(/[:.]/g, '-').substring(0, 23)}`;

  // Build Markdown report
  let report = '# KGC Probe Report\n\n';

  // Metadata header
  report += `**Run ID**: ${runId}  \n`;
  report += `**Observations**: ${sortedObs.length}  \n`;
  report += `**Hash**: sha256:${chainHash}\n\n`;

  // Executive Summary
  report += '## Executive Summary\n\n';
  report += `- **Platform**: ${platform.platform} ${platform.arch}\n`;
  report += `- **Runtime**: ${platform.runtime}\n`;

  // Key capabilities summary
  const wasmCap = capabilities.find(c => c.name.includes('wasm'));
  const workerCap = capabilities.find(c => c.name.includes('worker'));

  report += `- **WASM**: ${wasmCap ? 'Available' : 'Not detected'}\n`;
  report += `- **Workers**: ${workerCap ? 'Available' : 'Not detected'}\n`;
  report += `- **Capabilities**: ${capabilities.length} discovered\n`;
  report += `- **Constraints**: ${constraints.length} detected\n`;
  report += '\n';

  // Capabilities Section
  report += '## Capabilities\n\n';

  if (capabilities.length > 0) {
    // Group capabilities by domain
    const capByDomain = new Map();
    for (const cap of capabilities) {
      const domain = cap.name.split('.')[0] || 'general';
      if (!capByDomain.has(domain)) {
        capByDomain.set(domain, []);
      }
      capByDomain.get(domain).push(cap);
    }

    // Sort domains alphabetically
    const sortedDomains = Array.from(capByDomain.keys()).sort();

    for (const domain of sortedDomains) {
      report += `### ${domain.charAt(0).toUpperCase() + domain.slice(1)}\n\n`;

      const domainCaps = capByDomain.get(domain);
      for (const cap of domainCaps) {
        report += `- ✅ **${cap.name}**\n`;

        if (cap.data && Object.keys(cap.data).length > 0) {
          const dataStr = JSON.stringify(cap.data, null, 2);
          report += `  \`\`\`json\n  ${dataStr.split('\n').join('\n  ')}\n  \`\`\`\n`;
        }
      }
      report += '\n';
    }
  } else {
    report += '*No capabilities detected*\n\n';
  }

  // Constraints Section
  report += '## Constraints\n\n';

  if (constraints.length > 0) {
    // Group constraints by type
    const constraintByType = new Map();
    for (const constraint of constraints) {
      const type = constraint.type || 'general';
      if (!constraintByType.has(type)) {
        constraintByType.set(type, []);
      }
      constraintByType.get(type).push(constraint);
    }

    // Sort types alphabetically
    const sortedTypes = Array.from(constraintByType.keys()).sort();

    for (const type of sortedTypes) {
      report += `### ${type.split('-').map(w => w.charAt(0).toUpperCase() + w.slice(1)).join(' ')}\n\n`;

      const typeConstraints = constraintByType.get(type);
      for (const constraint of typeConstraints) {
        report += `- ⚠️  ${constraint.description}\n`;
      }
      report += '\n';
    }
  } else {
    report += '*No constraints detected*\n\n';
  }

  // Performance Metrics Section
  if (perfMetrics.length > 0) {
    report += '## Performance\n\n';
    report += '| Method | Mean | Median | P95 | Min | Max | Unit | Samples |\n';
    report += '|--------|------|--------|-----|-----|-----|------|--------|\n';

    for (const metric of perfMetrics) {
      const mean = metric.mean !== undefined ? metric.mean.toFixed(2) : '-';
      const median = metric.median !== undefined ? metric.median.toFixed(2) : '-';
      const p95 = metric.p95 !== undefined ? metric.p95.toFixed(2) : '-';
      const min = metric.min !== undefined ? metric.min.toFixed(2) : '-';
      const max = metric.max !== undefined ? metric.max.toFixed(2) : '-';
      const throughput = metric.throughput !== undefined ? metric.throughput.toFixed(2) : '-';

      if (metric.throughput !== undefined) {
        report += `| ${metric.method} | ${throughput} | - | - | - | - | ${metric.unit} | ${metric.samples} |\n`;
      } else {
        report += `| ${metric.method} | ${mean} | ${median} | ${p95} | ${min} | ${max} | ${metric.unit} | ${metric.samples} |\n`;
      }
    }
    report += '\n';
  }

  // Guard Denials Section
  if (guardDenials.length > 0) {
    report += '## Guard Denials\n\n';
    report += `${guardDenials.length} operation(s) were denied by security guards:\n\n`;

    for (const denial of guardDenials) {
      report += `- **${denial.method}**\n`;
      report += `  - Guard: \`${denial.guard}\`\n`;
      report += `  - Reason: ${denial.reason}\n`;
      report += `  - Hash: \`${denial.hash}\`\n\n`;
    }
  }

  // Provenance Section
  report += '## Provenance\n\n';
  report += `- **Observation count**: ${sortedObs.length}\n`;
  report += `- **Hash chain**: \`sha256:${chainHash}\`\n`;
  report += `- **Domains probed**: ${byDomain.size}\n`;

  const domainCounts = Array.from(byDomain.entries())
    .map(([domain, obs]) => `  - ${domain}: ${obs.length}`)
    .join('\n');

  report += '\n**Observations by domain**:\n';
  report += domainCounts + '\n\n';

  // Footer
  report += `---\n\n`;
  report += `*Report generated at ${new Date().toISOString()}*\n`;

  return report;
}

export default {
  renderReport,
  extractPlatformInfo,
  extractPerformanceMetrics,
  extractGuardDenials
};
