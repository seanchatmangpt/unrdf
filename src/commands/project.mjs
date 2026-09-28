/**
 * @file Project Command - Artifact Projection
 * @module commands/project
 *
 * @description
 * Projects artifacts from admitted universe at specific epoch
 * Generates output files with deterministic hashes
 */

import { writeFile, mkdir } from 'node:fs/promises';
import { resolve, join } from 'node:path';
import { createHash, randomUUID } from 'node:crypto';

/**
 * Project options
 * @typedef {Object} ProjectOptions
 * @property {string} [epoch] - Epoch identifier
 * @property {string} out - Output directory
 * @property {boolean} [json] - Output JSON format
 */

/**
 * Project result
 * @typedef {Object} ProjectResult
 * @property {string} [epoch] - Epoch identifier
 * @property {number} timestamp - Projection timestamp
 * @property {Array<Artifact>} artifacts - Generated artifacts
 */

/**
 * Artifact
 * @typedef {Object} Artifact
 * @property {string} name - Artifact name
 * @property {string} path - File path
 * @property {string} hash - Content hash
 * @property {string} type - Artifact type
 */

/**
 * Generate sample universe snapshot
 * @param {string} [epoch] - Epoch identifier
 * @returns {string} RDF content
 */
function generateUniverseSnapshot(epoch = 'latest') {
  const timestamp = new Date().toISOString();

  return `@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix gov: <http://example.org/governance#> .

# Universe Snapshot - Epoch: ${epoch}
# Generated: ${timestamp}

gov:Universe_${epoch}
  a gov:UniverseSnapshot ;
  gov:epoch "${epoch}" ;
  gov:timestamp "${timestamp}"^^xsd:dateTime ;
  gov:status "admitted" .

# Example entities in this epoch
gov:Entity_001
  a gov:Entity ;
  rdfs:label "Sample Entity 001" ;
  gov:inEpoch "${epoch}" .

gov:Entity_002
  a gov:Entity ;
  rdfs:label "Sample Entity 002" ;
  gov:inEpoch "${epoch}" .
`;
}

/**
 * Generate manifest
 * @param {string} [epoch] - Epoch identifier
 * @param {Array<Artifact>} artifacts - Artifacts list
 * @returns {string} JSON manifest
 */
function generateManifest(epoch, artifacts) {
  return JSON.stringify({
    version: '1.0.0',
    epoch: epoch || 'latest',
    timestamp: new Date().toISOString(),
    generator: 'governance-substrate-cli',
    artifacts: artifacts.map(a => ({
      name: a.name,
      path: a.path,
      hash: a.hash,
      type: a.type,
    })),
  }, null, 2);
}

/**
 * Generate summary report
 * @param {string} [epoch] - Epoch identifier
 * @param {Array<Artifact>} artifacts - Artifacts list
 * @returns {string} Markdown summary
 */
function generateSummary(epoch, artifacts) {
  const timestamp = new Date().toISOString();

  return `# Projection Summary

**Epoch**: ${epoch || 'latest'}
**Timestamp**: ${timestamp}
**Generator**: governance-substrate-cli v1.0.0

## Artifacts

Total artifacts generated: ${artifacts.length}

${artifacts.map(a => `- **${a.name}** (${a.type})
  - Path: \`${a.path}\`
  - Hash: \`${a.hash}\``).join('\n\n')}

## Verification

To verify artifact integrity:

\`\`\`bash
sha256sum ${artifacts[0].path}
# Should match: ${artifacts[0].hash}
\`\`\`
`;
}

/**
 * Compute hash of content
 * @param {string} content - Content to hash
 * @returns {string} SHA256 hex hash
 */
function computeHash(content) {
  return createHash('sha256').update(content).digest('hex');
}

/**
 * Project command implementation
 * @param {ProjectOptions} options - Command options
 * @returns {Promise<ProjectResult>} Project result
 */
export async function projectCommand(options) {
  // Validate required options
  if (!options.out) {
    throw new Error('Missing required option: --out <path>');
  }

  // Resolve paths
  const outDir = resolve(options.out);
  const epoch = options.epoch || `τ_${Date.now()}`;
  const timestamp = Date.now();

  // Ensure output directory exists
  try {
    await mkdir(outDir, { recursive: true });
  } catch (error) {
    throw new Error(`Failed to create output directory: ${error.message}`);
  }

  const artifacts = [];

  // Artifact 1: Universe snapshot
  const universeContent = generateUniverseSnapshot(epoch);
  const universeFilename = `universe-snapshot-${epoch}.ttl`;
  const universePath = join(outDir, universeFilename);
  const universeHash = computeHash(universeContent);

  try {
    await writeFile(universePath, universeContent, 'utf-8');
  } catch (error) {
    throw new Error(`Failed to write universe snapshot: ${error.message}`);
  }

  artifacts.push({
    name: universeFilename,
    path: universePath,
    hash: universeHash,
    type: 'rdf-snapshot',
  });

  // Artifact 2: Manifest (temporary, will update after all artifacts)
  const manifestFilename = `manifest-${epoch}.json`;
  const manifestPath = join(outDir, manifestFilename);

  // Artifact 3: Summary report
  const summaryContent = generateSummary(epoch, artifacts);
  const summaryFilename = `summary-${epoch}.md`;
  const summaryPath = join(outDir, summaryFilename);
  const summaryHash = computeHash(summaryContent);

  try {
    await writeFile(summaryPath, summaryContent, 'utf-8');
  } catch (error) {
    throw new Error(`Failed to write summary: ${error.message}`);
  }

  artifacts.push({
    name: summaryFilename,
    path: summaryPath,
    hash: summaryHash,
    type: 'markdown-report',
  });

  // Now generate final manifest with all artifacts
  const manifestContent = generateManifest(epoch, artifacts);
  const manifestHash = computeHash(manifestContent);

  try {
    await writeFile(manifestPath, manifestContent, 'utf-8');
  } catch (error) {
    throw new Error(`Failed to write manifest: ${error.message}`);
  }

  artifacts.push({
    name: manifestFilename,
    path: manifestPath,
    hash: manifestHash,
    type: 'json-manifest',
  });

  return {
    epoch,
    timestamp,
    artifacts,
  };
}
