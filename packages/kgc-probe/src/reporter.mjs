/**
 * KGC Probe Reporter - Convert observations to RDF/Turtle and generate reports
 *
 * Transforms runtime observations into semantic RDF graphs and human-readable
 * Markdown reports with derived capabilities and constraints.
 */

import { createStore, dataFactory } from '@unrdf/oxigraph';
import { z } from 'zod';
import crypto from 'crypto';

// ===== Zod Schemas =====

/**
 * Schema for probe observation metadata
 */
const ObservationSchema = z.object({
  method: z.string(),
  domain: z.string().optional(),
  timestamp: z.number().optional(),
  outputs: z.any(),
  error: z.string().optional(),
  guardDecision: z.string().optional(),
  hash: z.string().optional(),
});

/**
 * Schema for derived capability claim
 */
const CapabilitySchema = z.object({
  type: z.literal('capability'),
  title: z.string(),
  description: z.string(),
  evidence: z.array(z.string()),
});

/**
 * Schema for derived constraint claim
 */
const ConstraintSchema = z.object({
  type: z.literal('constraint'),
  title: z.string(),
  description: z.string(),
  evidence: z.array(z.string()),
});

// ===== RDF Vocabulary Constants =====

const KGC_NS = 'http://unrdf.dev/kgc#';
const XSD_NS = 'http://www.w3.org/2001/XMLSchema#';
const RDF_NS = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';
const RDFS_NS = 'http://www.w3.org/2000/01/rdf-schema#';

/**
 * Create a KGC namespace URI
 * @param {string} localName - Local name in KGC namespace
 * @returns {string} Full URI
 */
function kgcUri(localName) {
  return `${KGC_NS}${localName}`;
}

/**
 * Generate deterministic hash for observation
 * @param {Object} observation - Observation object
 * @returns {string} SHA-256 hash
 */
function generateHash(observation) {
  if (observation.hash) return observation.hash;

  const content = JSON.stringify({
    method: observation.method,
    outputs: observation.outputs,
    timestamp: observation.timestamp,
  });

  return crypto.createHash('sha256').update(content).digest('hex').substring(0, 16);
}

/**
 * Convert observations array to RDF/Turtle format
 *
 * Each observation becomes a kgc:Observation resource with:
 * - kgc:method (method name)
 * - kgc:timestamp (ISO datetime)
 * - kgc:hash (content hash for provenance)
 * - kgc:outputs (JSON-serialized outputs)
 * - kgc:guardDecision (if present)
 * - kgc:error (if present)
 * - kgc:domain (if present)
 *
 * @param {Array<Object>} observations - Array of observation objects
 * @returns {string} Turtle-formatted RDF string
 *
 * @example
 * const obs = [{ method: 'probeRuntime', outputs: { node: 'v18.19.0' } }];
 * const turtle = observationsToRdf(obs);
 * console.log(turtle); // @prefix kgc: <http://unrdf.dev/kgc#> . ...
 */
export function observationsToRdf(observations) {
  // Validate input
  const validatedObs = z.array(ObservationSchema).parse(observations);

  // Sort observations by method name for deterministic output
  const sortedObs = [...validatedObs].sort((a, b) =>
    a.method.localeCompare(b.method)
  );

  // Create RDF store
  const store = createStore();

  // Add observations as RDF quads
  for (const obs of sortedObs) {
    const hash = generateHash(obs);
    const obsUri = dataFactory.namedNode(kgcUri(`observation/${hash}`));
    const timestamp = obs.timestamp || Date.now();

    // Type declaration
    store.add(dataFactory.quad(
      obsUri,
      dataFactory.namedNode(`${RDF_NS}type`),
      dataFactory.namedNode(kgcUri('Observation'))
    ));

    // Method
    store.add(dataFactory.quad(
      obsUri,
      dataFactory.namedNode(kgcUri('method')),
      dataFactory.literal(obs.method)
    ));

    // Timestamp (as xsd:dateTime)
    const timestampDate = new Date(timestamp).toISOString();
    store.add(dataFactory.quad(
      obsUri,
      dataFactory.namedNode(kgcUri('timestamp')),
      dataFactory.literal(timestampDate, dataFactory.namedNode(`${XSD_NS}dateTime`))
    ));

    // Hash
    store.add(dataFactory.quad(
      obsUri,
      dataFactory.namedNode(kgcUri('hash')),
      dataFactory.literal(hash, dataFactory.namedNode(`${XSD_NS}string`))
    ));

    // Outputs (as JSON)
    store.add(dataFactory.quad(
      obsUri,
      dataFactory.namedNode(kgcUri('outputs')),
      dataFactory.literal(JSON.stringify(obs.outputs), dataFactory.namedNode(`${RDF_NS}JSON`))
    ));

    // Optional: Guard decision
    if (obs.guardDecision) {
      store.add(dataFactory.quad(
        obsUri,
        dataFactory.namedNode(kgcUri('guardDecision')),
        dataFactory.literal(obs.guardDecision, dataFactory.namedNode(`${XSD_NS}string`))
      ));
    }

    // Optional: Error
    if (obs.error) {
      store.add(dataFactory.quad(
        obsUri,
        dataFactory.namedNode(kgcUri('error')),
        dataFactory.literal(obs.error, dataFactory.namedNode(`${XSD_NS}string`))
      ));
    }

    // Optional: Domain
    if (obs.domain) {
      store.add(dataFactory.quad(
        obsUri,
        dataFactory.namedNode(kgcUri('domain')),
        dataFactory.literal(obs.domain, dataFactory.namedNode(`${XSD_NS}string`))
      ));
    }
  }

  // Serialize to Turtle
  return store.dump({ format: 'turtle' });
}

/**
 * Derive high-level capability and constraint claims from observations
 *
 * Analyzes observation patterns to extract:
 * - Capabilities: Features/resources available in the environment
 * - Constraints: Limitations or restrictions discovered
 *
 * @param {Array<Object>} observations - Array of observation objects
 * @returns {Object} Object with capabilities and constraints arrays
 *
 * @example
 * const claims = deriveClaims(observations);
 * console.log(claims.capabilities); // [{ type: 'capability', title: '...', ... }]
 * console.log(claims.constraints); // [{ type: 'constraint', title: '...', ... }]
 */
export function deriveClaims(observations) {
  // Validate input
  const validatedObs = z.array(ObservationSchema).parse(observations);

  const capabilities = [];
  const constraints = [];

  // Group observations by domain
  const byDomain = new Map();
  for (const obs of validatedObs) {
    const domain = obs.domain || 'general';
    if (!byDomain.has(domain)) {
      byDomain.set(domain, []);
    }
    byDomain.get(domain).push(obs);
  }

  // Analyze runtime observations
  const runtimeObs = validatedObs.filter(o =>
    o.method?.toLowerCase().includes('runtime') ||
    o.domain === 'runtime'
  );

  if (runtimeObs.length > 0) {
    const evidence = runtimeObs.map(o => o.method);
    const outputs = runtimeObs.map(o => o.outputs).filter(Boolean);

    // Extract Node.js version if present
    const nodeVersion = outputs.find(o => o.node || o.version)?.node ||
                       outputs.find(o => o.node || o.version)?.version;

    if (nodeVersion) {
      capabilities.push({
        type: 'capability',
        title: `Node.js Runtime ${nodeVersion}`,
        description: `Runtime environment running Node.js version ${nodeVersion}`,
        evidence,
      });
    }

    // Check for worker_threads
    const workerThreads = outputs.some(o =>
      o.worker_threads === true ||
      o.workerThreads === true ||
      (typeof o === 'object' && 'worker_threads' in o)
    );

    if (workerThreads) {
      capabilities.push({
        type: 'capability',
        title: 'Worker Threads Available',
        description: 'Runtime supports worker_threads for parallel execution',
        evidence,
      });
    }
  }

  // Analyze WASM observations
  const wasmObs = validatedObs.filter(o =>
    o.method?.toLowerCase().includes('wasm') ||
    o.domain === 'wasm'
  );

  if (wasmObs.length > 0) {
    const evidence = wasmObs.map(o => o.method);
    const outputs = wasmObs.map(o => o.outputs).filter(Boolean);

    const wasmAvailable = outputs.some(o => o.available === true || o.wasm === true);

    if (wasmAvailable) {
      const maxMemory = outputs.find(o => o.maxMemory)?.maxMemory;
      const memoryDesc = maxMemory ? ` with maximum memory ${maxMemory}` : '';

      capabilities.push({
        type: 'capability',
        title: 'WebAssembly Support',
        description: `WASM compilation and execution available${memoryDesc}`,
        evidence,
      });
    }
  }

  // Analyze filesystem observations
  const fsObs = validatedObs.filter(o =>
    o.method?.toLowerCase().includes('filesystem') ||
    o.method?.toLowerCase().includes('fs') ||
    o.domain === 'filesystem'
  );

  if (fsObs.length > 0) {
    const evidence = fsObs.map(o => o.method);
    const outputs = fsObs.map(o => o.outputs).filter(Boolean);

    // Check for restricted paths
    const allowedPaths = outputs.find(o => o.allowedPaths || o.paths)?.allowedPaths ||
                        outputs.find(o => o.allowedPaths || o.paths)?.paths;

    if (allowedPaths && Array.isArray(allowedPaths)) {
      constraints.push({
        type: 'constraint',
        title: 'Filesystem Access Restricted',
        description: `Filesystem access limited to: ${allowedPaths.join(', ')}`,
        evidence,
      });
    }

    // Check for denied operations
    const deniedOps = fsObs.filter(o =>
      o.guardDecision === 'denied' ||
      o.error?.includes('permission') ||
      o.error?.includes('denied')
    );

    if (deniedOps.length > 0) {
      constraints.push({
        type: 'constraint',
        title: 'Filesystem Operations Denied',
        description: `${deniedOps.length} filesystem operation(s) were denied by security guards`,
        evidence: deniedOps.map(o => o.method),
      });
    }
  }

  // Analyze network observations
  const networkObs = validatedObs.filter(o =>
    o.method?.toLowerCase().includes('network') ||
    o.domain === 'network'
  );

  if (networkObs.length > 0) {
    const evidence = networkObs.map(o => o.method);
    const outputs = networkObs.map(o => o.outputs).filter(Boolean);

    // Check for allowlisted URLs
    const allowedUrls = outputs.find(o => o.allowedUrls || o.urls)?.allowedUrls ||
                       outputs.find(o => o.allowedUrls || o.urls)?.urls;

    if (allowedUrls && Array.isArray(allowedUrls)) {
      constraints.push({
        type: 'constraint',
        title: 'Network Access Allowlist',
        description: `Network requests restricted to ${allowedUrls.length} allowlisted URL(s)`,
        evidence,
      });
    }

    // Check for denied requests
    const deniedReqs = networkObs.filter(o =>
      o.guardDecision === 'denied' ||
      o.error?.includes('network') ||
      o.error?.includes('blocked')
    );

    if (deniedReqs.length > 0) {
      constraints.push({
        type: 'constraint',
        title: 'Network Requests Blocked',
        description: `${deniedReqs.length} network request(s) were blocked`,
        evidence: deniedReqs.map(o => o.method),
      });
    }
  }

  // Analyze performance observations
  const perfObs = validatedObs.filter(o =>
    o.method?.toLowerCase().includes('performance') ||
    o.method?.toLowerCase().includes('benchmark') ||
    o.domain === 'performance'
  );

  if (perfObs.length > 0) {
    const evidence = perfObs.map(o => o.method);
    const outputs = perfObs.map(o => o.outputs).filter(Boolean);

    // Look for execution times
    const executionTimes = outputs
      .map(o => o.executionTime || o.duration || o.time)
      .filter(t => typeof t === 'number');

    if (executionTimes.length > 0) {
      const avgTime = executionTimes.reduce((a, b) => a + b, 0) / executionTimes.length;
      capabilities.push({
        type: 'capability',
        title: 'Performance Metrics',
        description: `Average execution time: ${avgTime.toFixed(2)}ms across ${executionTimes.length} measurement(s)`,
        evidence,
      });
    }
  }

  // Validate outputs
  const validatedCapabilities = capabilities.map(c => CapabilitySchema.parse(c));
  const validatedConstraints = constraints.map(c => ConstraintSchema.parse(c));

  return {
    capabilities: validatedCapabilities,
    constraints: validatedConstraints,
  };
}

/**
 * Generate comprehensive Markdown report from observations
 *
 * Creates a structured report with:
 * - Executive summary with counts
 * - Discovered capabilities
 * - Detected constraints
 * - Observations grouped by domain
 *
 * @param {Array<Object>} observations - Array of observation objects
 * @returns {string} Markdown-formatted report
 *
 * @example
 * const report = generateReport(observations);
 * console.log(report); // # KGC Probe Report\n\n## Summary\n...
 */
export function generateReport(observations) {
  // Validate input
  const validatedObs = z.array(ObservationSchema).parse(observations);

  // Derive claims
  const { capabilities, constraints } = deriveClaims(validatedObs);

  // Calculate execution time range
  const timestamps = validatedObs
    .map(o => o.timestamp)
    .filter(t => typeof t === 'number')
    .sort((a, b) => a - b);

  const executionTime = timestamps.length >= 2
    ? timestamps[timestamps.length - 1] - timestamps[0]
    : 0;

  // Group observations by domain
  const byDomain = new Map();
  for (const obs of validatedObs) {
    const domain = obs.domain || 'general';
    if (!byDomain.has(domain)) {
      byDomain.set(domain, []);
    }
    byDomain.get(domain).push(obs);
  }

  // Sort domains alphabetically
  const sortedDomains = Array.from(byDomain.keys()).sort();

  // Build report
  let report = '# KGC Probe Report\n\n';

  // Summary section
  report += '## Summary\n\n';
  report += `- **Total observations**: ${validatedObs.length}\n`;
  report += `- **Capabilities discovered**: ${capabilities.length}\n`;
  report += `- **Constraints detected**: ${constraints.length}\n`;
  report += `- **Execution time**: ${executionTime}ms\n`;
  report += `- **Domains probed**: ${sortedDomains.length}\n`;
  report += '\n';

  // Capabilities section
  if (capabilities.length > 0) {
    report += '## Capabilities\n\n';
    report += 'The following capabilities were discovered in the runtime environment:\n\n';

    for (const capability of capabilities) {
      report += `### ${capability.title}\n\n`;
      report += `${capability.description}\n\n`;
      report += `**Evidence**: ${capability.evidence.join(', ')}\n\n`;
    }
  } else {
    report += '## Capabilities\n\n';
    report += '*No capabilities detected*\n\n';
  }

  // Constraints section
  if (constraints.length > 0) {
    report += '## Constraints\n\n';
    report += 'The following constraints and limitations were detected:\n\n';

    for (const constraint of constraints) {
      report += `### ${constraint.title}\n\n`;
      report += `${constraint.description}\n\n`;
      report += `**Evidence**: ${constraint.evidence.join(', ')}\n\n`;
    }
  } else {
    report += '## Constraints\n\n';
    report += '*No constraints detected*\n\n';
  }

  // Observations by domain
  report += '## Observations by Domain\n\n';

  for (const domain of sortedDomains) {
    const domainObs = byDomain.get(domain);
    report += `### ${domain.charAt(0).toUpperCase() + domain.slice(1)} (${domainObs.length} observations)\n\n`;

    // Sort observations by method name
    const sortedDomainObs = [...domainObs].sort((a, b) =>
      a.method.localeCompare(b.method)
    );

    for (const obs of sortedDomainObs) {
      report += `#### ${obs.method}\n\n`;

      // Guard decision
      if (obs.guardDecision) {
        report += `**Guard Decision**: ${obs.guardDecision}\n\n`;
      }

      // Outputs
      if (obs.outputs) {
        report += '**Outputs**:\n```json\n';
        report += JSON.stringify(obs.outputs, null, 2);
        report += '\n```\n\n';
      }

      // Error
      if (obs.error) {
        report += `**Error**: ${obs.error}\n\n`;
      }

      // Timestamp
      if (obs.timestamp) {
        const timestampDate = new Date(obs.timestamp).toISOString();
        report += `**Timestamp**: ${timestampDate}\n\n`;
      }

      // Hash
      const hash = generateHash(obs);
      report += `**Hash**: ${hash}\n\n`;

      report += '---\n\n';
    }
  }

  // Footer
  report += `\n*Report generated at ${new Date().toISOString()}*\n`;

  return report;
}

// Export schemas for external use
export {
  ObservationSchema,
  CapabilitySchema,
  ConstraintSchema,
};

export default {
  observationsToRdf,
  generateReport,
  deriveClaims,
  ObservationSchema,
  CapabilitySchema,
  ConstraintSchema,
};
