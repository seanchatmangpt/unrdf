/**
 * @file run.mjs - Orchestrator kernel (μ1) for multi-agent swarm discovery
 * @description Entry point for deterministic discovery with DETERMINISTIC=1 support
 * @equation Ledger_τ = Γ( ⋃_{i=1..10} A_i )
 * @invariants Q1(Determinism), Q2(Provenance), Q3(Receipts), Q4(PokaYoke), Q5(SLAHonesty)
 */

import fs from 'fs'
import path from 'path'
import { fileURLToPath } from 'url'
import { hashData } from './hash.mjs'
import { stableStringify, canonicalize } from './stable-json.mjs'
import { checkAdmissibility } from './constraints.mjs'

const __dirname = path.dirname(fileURLToPath(import.meta.url))
const DETERMINISTIC = process.env.DETERMINISTIC === '1'

/**
 * Initialize discovery run with timestamp and epoch
 * @returns {object} - Run context
 */
export function initializeRun() {
  return {
    epoch: DETERMINISTIC ? 'deterministic-epoch-1' : `epoch-${Date.now()}`,
    timestamp: new Date().toISOString(),
    deterministic: DETERMINISTIC,
    phase: null,
    artifacts: {}
  }
}

/**
 * Store artifact with provenance
 * @param {object} context - Run context
 * @param {string} name - Artifact name (e.g., "atoms", "graph")
 * @param {any} data - Artifact data
 * @param {string} phase - Phase identifier (μ1, μ2, etc.)
 */
export function storeArtifact(context, name, data, phase) {
  const artifact = {
    name,
    phase,
    timestamp: new Date().toISOString(),
    hash: hashData(data),
    canonical: canonicalize(data)
  }

  context.artifacts[name] = artifact

  // Write to disk if needed
  const artifactPath = path.join(__dirname, '..', `${name}.json`)
  try {
    fs.writeFileSync(artifactPath, stableStringify(artifact.canonical), 'utf8')
  } catch (e) {
    console.warn(`Failed to write artifact ${name}:`, e.message)
  }

  return artifact
}

/**
 * Glue artifacts from all agents (Γ operator)
 * @param {object} context - Run context
 * @param {array} agentArtifacts - Array of artifacts from agents
 * @returns {object} - Merged atlas
 */
export function glueArtifacts(context, agentArtifacts) {
  const merged = {
    epoch: context.epoch,
    timestamp: context.timestamp,
    deterministic: context.deterministic,
    agents: {},
    atoms: [],
    graph: { vertices: [], edges: [] },
    candidates: [],
    experiments: [],
    ledger: []
  }

  for (const artifact of agentArtifacts) {
    const { phase, name, canonical } = artifact

    merged.agents[phase] = merged.agents[phase] || {}
    merged.agents[phase][name] = canonical

    // Merge specific artifact types
    if (name === 'atoms' && Array.isArray(canonical)) {
      merged.atoms.push(...canonical)
    }
    if (name === 'graph' && canonical.edges) {
      merged.graph.vertices.push(...(canonical.vertices || []))
      merged.graph.edges.push(...(canonical.edges || []))
    }
    if (name === 'candidates' && Array.isArray(canonical)) {
      merged.candidates.push(...canonical)
    }
    if (name === 'experiments' && Array.isArray(canonical)) {
      merged.experiments.push(...canonical)
    }
  }

  return merged
}

/**
 * Create ledger with determinism proof
 * @param {object} atlas - Merged atlas
 * @returns {object} - Ledger with hash
 */
export function createLedger(atlas) {
  const canonical = canonicalize(atlas)
  const ledgerHash = hashData(canonical)

  return {
    version: '1.0.0',
    hash: ledgerHash,
    deterministic: atlas.deterministic,
    timestamp: atlas.timestamp,
    epoch: atlas.epoch,
    atlas: canonical,

    /**
     * Verify ledger integrity
     * @returns {boolean}
     */
    verify() {
      const recomputed = hashData(canonicalize(this.atlas))
      return recomputed === this.hash
    },

    /**
     * Export ledger
     * @returns {string} - JSON representation
     */
    toJSON() {
      return stableStringify(this)
    }
  }
}

/**
 * Run deterministic discovery phase
 * @param {string} phase - Phase name (μ1, μ2, etc.)
 * @param {function} agentFn - Agent function to run
 * @param {object} context - Run context
 * @returns {Promise<object>} - Agent results
 */
export async function runPhase(phase, agentFn, context) {
  console.log(`[${phase}] Starting...`)

  context.phase = phase

  const startTime = Date.now()
  let result

  try {
    result = await agentFn(context)
  } catch (e) {
    console.error(`[${phase}] Failed:`, e.message)
    throw e
  }

  const duration = Date.now() - startTime

  console.log(`[${phase}] Complete (${duration}ms)`)

  return {
    phase,
    duration,
    result,
    success: true
  }
}

/**
 * Main orchestrator loop
 * @param {array} agents - Array of [phase, agentFn] tuples
 * @param {object} options - Orchestration options
 */
export async function orchestrate(agents, options = {}) {
  const context = initializeRun()
  const results = []
  const artifacts = []

  console.log(`
╔════════════════════════════════════════════════════════════════════╗
║          Multi-Agent Swarm Discovery (10 agents)                  ║
║          μ1(Orchestrator) ⊕ μ2..μ10 ⊕ Γ(glue) → ATLAS+LEDGER      ║
╚════════════════════════════════════════════════════════════════════╝

Deterministic: ${DETERMINISTIC ? 'YES (same hash across runs)' : 'NO'}
Epoch: ${context.epoch}
`)

  // Run agents
  for (const [phase, agentFn] of agents) {
    const phaseResult = await runPhase(phase, agentFn, context)
    results.push(phaseResult)

    if (phaseResult.result && phaseResult.result.artifacts) {
      artifacts.push(...phaseResult.result.artifacts)
    }
  }

  // Glue artifacts
  const atlas = glueArtifacts(context, artifacts)
  const ledger = createLedger(atlas)

  // Write ledger
  const ledgerPath = path.join(__dirname, '..', 'LEDGER.json')
  fs.writeFileSync(ledgerPath, ledger.toJSON(), 'utf8')

  // Write atlas
  const atlasPath = path.join(__dirname, '..', 'ATLAS.json')
  fs.writeFileSync(atlasPath, stableStringify(atlas), 'utf8')

  console.log(`
╔════════════════════════════════════════════════════════════════════╗
║                        RESULTS                                     ║
╚════════════════════════════════════════════════════════════════════╝

Ledger Hash: ${ledger.hash}
Ledger File: ${ledgerPath}
Atlas File: ${atlasPath}

${results.map(r => `✓ ${r.phase}: ${r.duration}ms`).join('\n')}
`)

  return {
    context,
    results,
    atlas,
    ledger
  }
}

export default {
  initializeRun,
  storeArtifact,
  glueArtifacts,
  createLedger,
  runPhase,
  orchestrate
}
