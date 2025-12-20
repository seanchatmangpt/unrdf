#!/usr/bin/env node
/**
 * Simple demonstration of Docker Compose self-healing
 * Kill one container, wait for it to auto-restart, validate cluster intact
 */

import { execSync } from 'child_process'

function exec(cmd) {
  return execSync(cmd, { encoding: 'utf-8' })
}

async function wait(ms, label) {
  process.stdout.write(`${label}`)
  const steps = 10
  for (let i = 0; i < steps; i++) {
    await new Promise(r => setTimeout(r, ms / steps))
    process.stdout.write('.')
  }
  console.log(' done')
}

console.log('🔄 Docker Compose Self-Healing Demonstration\n')

// Start cluster
console.log('1. Starting 5-node cluster...')
exec('docker compose down 2>&1 > /dev/null || true')
exec('docker compose up -d')
await wait(25000, '   Waiting for startup')

// Check initial health
const before = exec('docker compose ps --format json')
  .trim().split('\n').filter(l => l.trim()).map(l => JSON.parse(l))
console.log(`   ✅ Cluster running: ${before.length} containers\n`)

before.forEach(c => console.log(`      - ${c.Name}: ${c.State}`))
console.log()

// Kill one container
console.log('2. Killing atomvm-node3 (simulating failure)...')
exec('docker kill atomvm-node3')
console.log('   💀 Container killed\n')

// Wait a bit
await wait(3000, '   Waiting')

// Check degraded state
const degraded = exec('docker compose ps --format json')
  .trim().split('\n').filter(l => l.trim()).map(l => JSON.parse(l))
const running = degraded.filter(c => c.State === 'running')
console.log(`   📊 Degraded state: ${running.length}/${before.length} running\n`)

// Wait for auto-restart
console.log('3. Waiting for Docker auto-restart (restart: always policy)...')
await wait(20000, '   Monitoring restart')

// Check recovered state
const after = exec('docker compose ps --format json')
  .trim().split('\n').filter(l => l.trim()).map(l => JSON.parse(l))
const recovered = after.filter(c => c.State === 'running')

console.log(`   📊 Recovered state: ${recovered.length}/${before.length} running\n`)

after.forEach(c => {
  const status = c.State === 'running' ? '✅' : '❌'
  console.log(`      ${status} ${c.Name}: ${c.State}`)
})
console.log()

// Validate
if (recovered.length === before.length) {
  console.log('✅ SELF-HEALING VERIFIED')
  console.log('   ✓ Container auto-restarted')
  console.log('   ✓ Cluster fully recovered')
  console.log('   ✓ No manual intervention required')
} else if (recovered.length >= before.length - 1) {
  console.log('⚠️  PARTIAL RECOVERY')
  console.log(`   Container restarting (${recovered.length}/${before.length})`)
  console.log('   May need more time for full recovery')
} else {
  console.log('❌ RECOVERY FAILED')
  console.log(`   Only ${recovered.length}/${before.length} containers running`)
}

console.log()
console.log('4. Cleanup...')
exec('docker compose down')
console.log('   ✅ Cluster stopped\n')
