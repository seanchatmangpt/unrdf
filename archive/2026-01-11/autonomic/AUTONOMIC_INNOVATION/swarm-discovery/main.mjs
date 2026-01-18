#!/usr/bin/env node

/**
 * @file main.mjs - Multi-agent swarm discovery orchestrator
 * @description Entry point for the 10-agent discovery process
 * @usage node main.mjs [--deterministic] [--beam-width N] [--top-n N]
 *
 * Runs: μ1(orchestrator) ⊕ μ2..μ10 ⊕ Γ(glue) → ATLAS.json + LEDGER.json
 */

import fs from 'fs'
import path from 'path'
import { fileURLToPath } from 'url'
import { orchestrate } from './kernel/run.mjs'
import { extractAtoms, formatAtomsForAtlas } from './agents/mu2-atomization.mjs'
import { inferGraph, findCompositionLoops } from './agents/mu3-graph-inference.mjs'
import { searchCompositions } from './agents/mu5-search.mjs'
import { computeMetrics, computeBaselines } from './agents/mu8-metrics.mjs'
import { rankCandidates, generatePromotions, createPromotionManifest } from './agents/mu10-ranking.mjs'
import { stableStringify } from './kernel/stable-json.mjs'
import { hashData } from './kernel/hash.mjs'

const __dirname = path.dirname(fileURLToPath(import.meta.url))

// Parse command-line options
const args = process.argv.slice(2)
const DETERMINISTIC = args.includes('--deterministic') || process.env.DETERMINISTIC === '1'
const BEAM_WIDTH = parseInt(args.find(a => a.startsWith('--beam-width='))?.split('=')[1] || '10', 10)
const TOP_N = parseInt(args.find(a => a.startsWith('--top-n='))?.split('=')[1] || '5', 10)

console.log(`\n${'='.repeat(72)}`)
console.log('  UNRDF Multi-Agent Swarm Discovery')
console.log(`${'='.repeat(72)}\n`)
console.log(`Deterministic: ${DETERMINISTIC}`)
console.log(`Beam Width: ${BEAM_WIDTH}`)
console.log(`Promote Top-N: ${TOP_N}\n`)

/**
 * Agent μ2: Extract atoms
 */
async function mu2_atomization(context) {
  console.log('  [μ2] Extracting atoms from observable state...')

  const atomsResult = await extractAtoms({})
  const formatted = formatAtomsForAtlas(atomsResult.atoms)

  return {
    artifacts: [{
      phase: 'μ2',
      name: 'atoms',
      canonical: formatted
    }],
    atoms: formatted,
    count: formatted.length
  }
}

/**
 * Agent μ3: Infer graph
 */
async function mu3_graph(context) {
  console.log('  [μ3] Inferring dependency graph...')

  const graph = await inferGraph(context.result.atoms)

  return {
    artifacts: [{
      phase: 'μ3',
      name: 'graph',
      canonical: graph
    }],
    graph
  }
}

/**
 * Agent μ5: Search compositions
 */
async function mu5_search(context) {
  console.log('  [μ5] Searching for high-synergy compositions...')

  const results = await searchCompositions(context.graph, context.atoms, {
    beamWidth: BEAM_WIDTH,
    maxDepth: 4
  })

  return {
    artifacts: [{
      phase: 'μ5',
      name: 'candidates',
      canonical: results.candidates
    }],
    candidates: results.candidates,
    searchStats: results.statistics
  }
}

/**
 * Agent μ8: Compute metrics
 */
async function mu8_metrics(context) {
  console.log('  [μ8] Computing utility and synergy metrics...')

  const baselines = computeBaselines(context.atoms)
  const metrics = computeMetrics(context.candidates, context.atoms)

  return {
    artifacts: [
      {
        phase: 'μ8',
        name: 'baselines',
        canonical: baselines
      },
      {
        phase: 'μ8',
        name: 'metrics',
        canonical: metrics
      }
    ],
    baselines,
    metrics
  }
}

/**
 * Agent μ10: Rank and promote
 */
async function mu10_ranking(context) {
  console.log('  [μ10] Ranking and promoting top capabilities...')

  const rankingResult = rankCandidates(context.metrics, context.candidates, {
    topN: TOP_N,
    minSynergy: -0.1 // Accept all for now
  })

  const promotions = generatePromotions(rankingResult.promoted, context.atoms, {
    deterministic: DETERMINISTIC
  })
  const manifest = createPromotionManifest(promotions, {
    deterministic: DETERMINISTIC
  })

  return {
    artifacts: [
      {
        phase: 'μ10',
        name: 'ranking',
        canonical: rankingResult.ranked
      },
      {
        phase: 'μ10',
        name: 'promotions',
        canonical: promotions
      },
      {
        phase: 'μ10',
        name: 'promotion-manifest',
        canonical: manifest
      }
    ],
    ranked: rankingResult.ranked,
    promoted: promotions,
    manifest
  }
}

/**
 * Wrapper agent functions for orchestrator
 */
async function agentMu2(context) {
  const result = await mu2_atomization(context)
  context.atoms = result.atoms
  context.result = result
  return result
}

async function agentMu3(context) {
  if (!context.atoms) {
    const mu2result = await mu2_atomization(context)
    context.atoms = mu2result.atoms
  }
  const result = await mu3_graph(context)
  context.graph = result.graph
  return result
}

async function agentMu5(context) {
  if (!context.atoms) {
    const mu2result = await mu2_atomization(context)
    context.atoms = mu2result.atoms
  }
  if (!context.graph) {
    const mu3result = await mu3_graph(context)
    context.graph = mu3result.graph
  }
  const result = await mu5_search(context)
  context.candidates = result.candidates
  return result
}

async function agentMu8(context) {
  if (!context.atoms || !context.candidates) {
    if (!context.atoms) {
      const mu2result = await mu2_atomization(context)
      context.atoms = mu2result.atoms
    }
    if (!context.graph) {
      const mu3result = await mu3_graph(context)
      context.graph = mu3result.graph
    }
    const mu5result = await mu5_search(context)
    context.candidates = mu5result.candidates
  }
  const result = await mu8_metrics(context)
  context.metrics = result.metrics
  context.baselines = result.baselines
  return result
}

async function agentMu10(context) {
  if (!context.metrics) {
    if (!context.atoms) {
      const mu2result = await mu2_atomization(context)
      context.atoms = mu2result.atoms
    }
    if (!context.graph) {
      const mu3result = await mu3_graph(context)
      context.graph = mu3result.graph
    }
    if (!context.candidates) {
      const mu5result = await mu5_search(context)
      context.candidates = mu5result.candidates
    }
    const mu8result = await mu8_metrics(context)
    context.metrics = mu8result.metrics
  }
  const result = await mu10_ranking(context)
  return result
}

/**
 * Main execution
 */
async function main() {
  try {
    // Set DETERMINISTIC env var for kernel
    if (DETERMINISTIC) {
      process.env.DETERMINISTIC = '1'
    }

    const context = {}

    // Phase 1: Extract atoms (μ2)
    console.log('Phase 1: Atomization (μ2)')
    const mu2Result = await agentMu2(context)
    console.log(`  → Extracted ${mu2Result.count} atoms\n`)

    // Phase 2: Infer graph (μ3)
    console.log('Phase 2: Graph Inference (μ3)')
    const mu3Result = await agentMu3(context)
    console.log(`  → Graph: ${mu3Result.graph.statistics.vertexCount} vertices, ${mu3Result.graph.statistics.edgeCount} edges\n`)

    // Phase 3: Search compositions (μ5)
    console.log('Phase 3: Search (μ5)')
    const mu5Result = await mu5_search(context)
    context.candidates = mu5Result.candidates
    console.log(`  → Explored ${mu5Result.searchStats.totalExplored} compositions, top score: ${mu5Result.searchStats.topCandidateScore.toFixed(3)}`)
    console.log(`  → Top candidate atoms: ${mu5Result.candidates[0]?.atoms.join(', ') || 'none'}\n`)

    // Phase 4: Compute metrics (μ8)
    console.log('Phase 4: Metrics (μ8)')
    const mu8Result = await mu8_metrics(context)
    context.metrics = mu8Result.metrics
    context.baselines = mu8Result.baselines
    console.log(`  → Computed ${mu8Result.metrics.length} metrics`)
    console.log(`  → Top metric synergy: ${mu8Result.metrics[0]?.synergy.toFixed(4) || 'none'}\n`)

    // Phase 5: Rank and promote (μ10)
    console.log('Phase 5: Ranking (μ10)')
    const mu10Result = await mu10_ranking(context)
    console.log(`  → Promoted ${mu10Result.promoted.length} top capabilities\n`)

    // Glue all artifacts
    console.log('Phase 6: Artifact Gluing (Γ operator)')

    const allArtifacts = [
      ...mu2Result.artifacts,
      ...mu3Result.artifacts,
      ...mu5Result.artifacts,
      ...mu8Result.artifacts,
      ...mu10Result.artifacts
    ]

    // Use fixed timestamp for determinism
    const epoch = DETERMINISTIC ? '2025-12-26T00:00:00.000Z' : new Date().toISOString()

    // Create ATLAS
    const atlas = {
      epoch: epoch,
      deterministic: DETERMINISTIC,
      atoms: mu2Result.atoms,
      graph: mu3Result.graph,
      candidates: mu5Result.candidates,
      baselines: mu8Result.baselines,
      metrics: mu8Result.metrics,
      ranked: mu10Result.ranked,
      promoted: mu10Result.promoted,
      manifest: mu10Result.manifest
    }

    // Create LEDGER
    const atlasHash = hashData(atlas)

    // Use fixed timestamp for determinism
    const timestamp = DETERMINISTIC ? '2025-12-26T00:00:00.000Z' : new Date().toISOString()

    const ledger = {
      version: '1.0.0',
      hash: atlasHash,
      deterministic: DETERMINISTIC,
      timestamp: timestamp,
      summary: {
        atoms: mu2Result.count,
        edges: mu3Result.graph.statistics.edgeCount,
        candidates: mu5Result.candidates.length,
        topPromotion: mu10Result.promoted[0]
      },
      artifacts: allArtifacts.map(a => ({ phase: a.phase, name: a.name }))
    }

    // Write artifacts
    const outDir = __dirname
    const atlasPath = path.join(outDir, 'ATLAS.json')
    const ledgerPath = path.join(outDir, 'LEDGER.json')

    fs.writeFileSync(atlasPath, stableStringify(atlas), 'utf8')
    fs.writeFileSync(ledgerPath, stableStringify(ledger), 'utf8')

    console.log(`✓ ATLAS written to ${atlasPath}`)
    console.log(`✓ LEDGER written to ${ledgerPath}`)
    console.log(`✓ LEDGER hash: ${ledger.hash}\n`)

    // Print summary
    console.log(`${'='.repeat(72)}`)
    console.log(`  Discovery Complete`)
    console.log(`${'='.repeat(72)}\n`)

    console.log('Top Promoted Capabilities:')
    mu10Result.promoted.slice(0, 3).forEach((p, i) => {
      console.log(`\n${i + 1}. ${p.id} (Rank: ${p.rank})`)
      console.log(`   Atoms: ${p.atomDetails.map(a => a.name).join(', ')}`)
      console.log(`   Synergy: ${p.synergy.toFixed(4)}`)
      console.log(`   Capabilities: ${p.capabilities.join(', ')}`)
    })

    console.log(`\n${'='.repeat(72)}\n`)

    return {
      success: true,
      atlas,
      ledger,
      promoted: mu10Result.promoted
    }
  } catch (error) {
    console.error('\n✗ Discovery failed:', error.message)
    console.error(error.stack)
    process.exit(1)
  }
}

// Run
main().catch(e => {
  console.error('Fatal error:', e)
  process.exit(1)
})
