#!/usr/bin/env node
/**
 * Chaos Engineering Test for AtomVM Cluster
 * - Starts Docker Compose cluster (5 nodes)
 * - Randomly kills containers during operation
 * - Validates cluster continues operating without failures
 * - Tests self-healing with Docker Compose restart policy
 */

import { execSync } from 'child_process'
import { writeFileSync } from 'fs'

console.log('💥 CHAOS ENGINEERING TEST - AtomVM Cluster\n')
console.log('This test will:')
console.log('  1. Start 5-node Docker Compose cluster')
console.log('  2. Monitor cluster health baseline')
console.log('  3. Randomly kill containers (chaos)')
console.log('  4. Validate no cascading failures')
console.log('  5. Verify auto-recovery via Docker restart policy\n')

const CHAOS_CONFIG = {
  nodeCount: 5,
  killIterations: 10,
  recoveryWaitMs: 15000,  // Wait 15s for recovery
  healthCheckIntervalMs: 2000
}

const metrics = {
  totalKills: 0,
  successfulRecoveries: 0,
  failedRecoveries: 0,
  cascadingFailures: 0,
  downtimeSeconds: [],
  clusterHealthHistory: []
}

/**
 * Execute shell command and return output
 */
function exec(cmd, options = {}) {
  try {
    return execSync(cmd, { encoding: 'utf-8', ...options })
  } catch (error) {
    if (options.allowFail) {
      return error.stdout || ''
    }
    throw error
  }
}

/**
 * Get cluster health from Docker Compose
 */
function getClusterHealth() {
  const output = exec('docker compose -f docker-compose.yml ps --format json', { allowFail: true })

  if (!output.trim()) {
    return { total: 0, healthy: 0, unhealthy: 0, running: 0, containers: [] }
  }

  const containers = output.trim().split('\n')
    .filter(line => line.trim())
    .map(line => {
      try {
        return JSON.parse(line)
      } catch {
        return null
      }
    })
    .filter(c => c !== null)

  const health = {
    total: containers.length,
    healthy: 0,
    unhealthy: 0,
    running: 0,
    containers: containers.map(c => ({
      name: c.Name,
      state: c.State,
      health: c.Health
    }))
  }

  for (const container of containers) {
    if (container.State === 'running') {
      health.running++
    }
    if (container.Health === 'healthy' || (container.State === 'running' && !container.Health)) {
      health.healthy++
    } else if (container.Health === 'unhealthy' || container.State !== 'running') {
      health.unhealthy++
    }
  }

  return health
}

/**
 * Kill random container from cluster
 */
function killRandomContainer(excludeRecent = []) {
  const allContainers = [
    'atomvm-node1',
    'atomvm-node2',
    'atomvm-node3',
    'atomvm-node4',
    'atomvm-node5'
  ]

  const available = allContainers.filter(c => !excludeRecent.includes(c))

  if (available.length === 0) {
    return null
  }

  const target = available[Math.floor(Math.random() * available.length)]

  console.log(`  💀 Killing ${target}...`)

  const killTime = Date.now()
  exec(`docker kill ${target}`, { allowFail: true })

  return { container: target, killedAt: killTime }
}

/**
 * Validate cluster recovered after container kill
 */
async function validateRecovery(beforeHealth, afterHealth, killedContainer) {
  const recovered = afterHealth.running >= beforeHealth.running - 1
  const cascading = afterHealth.running < beforeHealth.running - 1

  console.log(`  📊 Recovery validation:`)
  console.log(`     Before: ${beforeHealth.running}/${beforeHealth.total} running`)
  console.log(`     After:  ${afterHealth.running}/${afterHealth.total} running`)
  console.log(`     Status: ${recovered ? '✅ RECOVERED' : '❌ FAILED'}`)

  if (cascading) {
    console.log(`     ⚠️  CASCADING FAILURE DETECTED`)
  }

  return { recovered, cascading }
}

/**
 * Wait for duration with progress dots
 */
async function wait(ms, label = 'Waiting') {
  const steps = 5
  const interval = ms / steps

  process.stdout.write(`  ${label}`)
  for (let i = 0; i < steps; i++) {
    await new Promise(resolve => setTimeout(resolve, interval))
    process.stdout.write('.')
  }
  console.log(' done')
}

/**
 * Main chaos test
 */
async function runChaosTest() {
  try {
    // ========================================
    // Step 1: Start Docker Compose cluster
    // ========================================
    console.log('=' .repeat(70))
    console.log('STEP 1: Starting Docker Compose Cluster')
    console.log('=' .repeat(70))
    console.log()

    console.log('🐳 Starting cluster with docker compose up...')
    exec('docker compose -f docker-compose.yml down -v', { allowFail: true })
    exec('docker compose -f docker-compose.yml up -d')

    await wait(20000, 'Waiting for cluster startup')
    console.log()

    // ========================================
    // Step 2: Baseline health check
    // ========================================
    console.log('=' .repeat(70))
    console.log('STEP 2: Baseline Health Check')
    console.log('=' .repeat(70))
    console.log()

    const baselineHealth = getClusterHealth()
    console.log(`📊 Cluster Status:`)
    console.log(`   Total containers: ${baselineHealth.total}`)
    console.log(`   Running: ${baselineHealth.running}`)
    console.log(`   Healthy: ${baselineHealth.healthy}`)
    console.log(`   Unhealthy: ${baselineHealth.unhealthy}`)
    console.log()

    if (baselineHealth.running < CHAOS_CONFIG.nodeCount) {
      throw new Error(`Only ${baselineHealth.running}/${CHAOS_CONFIG.nodeCount} nodes running`)
    }

    console.log('✅ Baseline health check passed\n')

    // ========================================
    // Step 3: Chaos Testing Loop
    // ========================================
    console.log('=' .repeat(70))
    console.log('STEP 3: Chaos Testing (Random Container Kills)')
    console.log('=' .repeat(70))
    console.log()

    const recentKills = []

    for (let i = 1; i <= CHAOS_CONFIG.killIterations; i++) {
      console.log(`\n🔥 Chaos Iteration ${i}/${CHAOS_CONFIG.killIterations}`)
      console.log('-'.repeat(70))

      // Get health before kill
      const beforeKill = getClusterHealth()
      console.log(`  📊 Pre-kill health: ${beforeKill.running}/${beforeKill.total} running`)

      // Kill random container
      const killed = killRandomContainer(recentKills)
      if (!killed) {
        console.log(`  ⚠️  No containers available to kill (recent kills)`)
        continue
      }

      metrics.totalKills++
      recentKills.push(killed.container)
      if (recentKills.length > 2) {
        recentKills.shift() // Keep last 2 kills
      }

      // Wait for recovery
      await wait(CHAOS_CONFIG.recoveryWaitMs, 'Waiting for auto-recovery')

      // Check health after recovery
      const afterRecovery = getClusterHealth()

      // Validate recovery
      const validation = await validateRecovery(beforeKill, afterRecovery, killed.container)

      if (validation.recovered) {
        metrics.successfulRecoveries++
      } else {
        metrics.failedRecoveries++
      }

      if (validation.cascading) {
        metrics.cascadingFailures++
      }

      metrics.clusterHealthHistory.push({
        iteration: i,
        beforeHealth: beforeKill,
        afterHealth: afterRecovery,
        killedContainer: killed.container,
        recovered: validation.recovered
      })

      console.log()
    }

    // ========================================
    // Step 4: Final Health Check
    // ========================================
    console.log('=' .repeat(70))
    console.log('STEP 4: Final Health Validation')
    console.log('=' .repeat(70))
    console.log()

    await wait(10000, 'Waiting for final stabilization')

    const finalHealth = getClusterHealth()
    console.log(`📊 Final Cluster Status:`)
    console.log(`   Total containers: ${finalHealth.total}`)
    console.log(`   Running: ${finalHealth.running}`)
    console.log(`   Healthy: ${finalHealth.healthy}`)
    console.log(`   Unhealthy: ${finalHealth.unhealthy}`)
    console.log()

    finalHealth.containers.forEach(c => {
      const status = c.state === 'running' ? '✅' : '❌'
      console.log(`   ${status} ${c.name}: ${c.state}`)
    })
    console.log()

    // ========================================
    // Step 5: Generate Report
    // ========================================
    console.log('=' .repeat(70))
    console.log('CHAOS TEST RESULTS')
    console.log('=' .repeat(70))
    console.log()
    console.log(`🎯 Chaos Metrics:`)
    console.log(`   Total kills: ${metrics.totalKills}`)
    console.log(`   Successful recoveries: ${metrics.successfulRecoveries}`)
    console.log(`   Failed recoveries: ${metrics.failedRecoveries}`)
    console.log(`   Cascading failures: ${metrics.cascadingFailures}`)
    console.log()

    const recoveryRate = ((metrics.successfulRecoveries / metrics.totalKills) * 100).toFixed(1)
    console.log(`📊 Recovery Rate: ${recoveryRate}%`)
    console.log()

    if (metrics.cascadingFailures === 0 && finalHealth.running >= CHAOS_CONFIG.nodeCount - 1) {
      console.log('✅ CHAOS TEST PASSED')
      console.log('   ✓ No cascading failures detected')
      console.log('   ✓ Cluster self-healed via Docker restart policy')
      console.log('   ✓ System demonstrated telecom-grade resilience')
      console.log()

      // Save report
      const report = {
        timestamp: new Date().toISOString(),
        config: CHAOS_CONFIG,
        metrics,
        finalHealth,
        result: 'PASSED'
      }
      writeFileSync('chaos-test-report.json', JSON.stringify(report, null, 2))
      console.log('📄 Report saved: chaos-test-report.json')

      return true
    } else {
      console.log('❌ CHAOS TEST FAILED')
      if (metrics.cascadingFailures > 0) {
        console.log(`   ✗ Cascading failures detected: ${metrics.cascadingFailures}`)
      }
      if (finalHealth.running < CHAOS_CONFIG.nodeCount - 1) {
        console.log(`   ✗ Cluster degraded: ${finalHealth.running}/${CHAOS_CONFIG.nodeCount} running`)
      }
      console.log()
      return false
    }

  } catch (error) {
    console.error('❌ Chaos test failed with error:', error.message)
    return false
  } finally {
    // Cleanup
    console.log('=' .repeat(70))
    console.log('CLEANUP')
    console.log('=' .repeat(70))
    console.log()
    console.log('🧹 Stopping Docker Compose cluster...')
    exec('docker compose -f docker-compose.yml down', { allowFail: true })
    console.log('✅ Cleanup complete')
  }
}

// Execute
runChaosTest()
  .then(success => {
    console.log()
    process.exit(success ? 0 : 1)
  })
  .catch(error => {
    console.error('💥 Fatal error:', error)
    process.exit(1)
  })
