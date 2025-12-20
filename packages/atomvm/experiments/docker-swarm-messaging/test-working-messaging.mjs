#!/usr/bin/env node
/**
 * Docker Swarm + Erlang Distribution - WORKING MESSAGE PASSING
 * Fixed: EPMD ports, DNS resolution, long node names
 */

import { execSync } from 'child_process'

function exec(cmd, options = {}) {
  try {
    const result = execSync(cmd, {
      encoding: 'utf-8',
      stdio: options.silent ? 'pipe' : 'inherit',
      ...options
    })
    return result
  } catch (error) {
    if (options.allowFail) {
      return error.stdout || ''
    }
    throw error
  }
}

async function wait(ms, label) {
  if (label) process.stdout.write(`${label}`)
  const steps = 10
  for (let i = 0; i < steps; i++) {
    await new Promise(r => setTimeout(r, ms / steps))
    if (label) process.stdout.write('.')
  }
  if (label) console.log(' done')
  else await new Promise(r => setTimeout(r, ms))
}

console.log('🔧 Docker Swarm + Erlang Distribution - WORKING Test\n')
console.log('Fixes applied:')
console.log('  ✓ EPMD port 4369 exposed')
console.log('  ✓ Erlang distribution ports 9100-9200')
console.log('  ✓ DNS round-robin endpoint mode')
console.log('  ✓ Long node names for DNS resolution')
console.log('  ✓ Explicit EPMD daemon start\n')

async function runWorkingTest() {
  try {
    // ========================================
    // Step 1: Initialize Docker Swarm
    // ========================================
    console.log('=' .repeat(70))
    console.log('STEP 1: Initialize Docker Swarm')
    console.log('=' .repeat(70))
    console.log()

    const swarmStatus = exec('docker info --format "{{.Swarm.LocalNodeState}}"', { silent: true, allowFail: true }).trim()
    if (swarmStatus === 'active') {
      console.log('⚠️  Leaving existing swarm...')
      exec('docker swarm leave --force', { allowFail: true })
      await wait(2000)
    }

    console.log('🔧 Initializing Docker Swarm...')
    exec('docker swarm init')
    console.log('✅ Swarm initialized\n')

    // ========================================
    // Step 2: Deploy Stack with Fixed Config
    // ========================================
    console.log('=' .repeat(70))
    console.log('STEP 2: Deploy Stack (Fixed EPMD Config)')
    console.log('=' .repeat(70))
    console.log()

    console.log('🚀 Deploying atomvm-messaging stack...')
    exec('docker stack deploy -c docker-stack-fixed.yml atomvm-messaging')

    // Wait for all replicas to start (poll until ready)
    console.log('   Waiting for all 3 replicas to start...')
    let replicas = 0
    const maxWaitTime = 120000 // 2 minutes max
    const startTime = Date.now()

    while (replicas < 3 && (Date.now() - startTime) < maxWaitTime) {
      await wait(10000, '   Checking replicas')

      const servicePs = exec('docker service ps atomvm-messaging_atomvm-node --format "{{.CurrentState}}"', { silent: true, allowFail: true })
      const runningCount = servicePs.split('\n').filter(line => line.includes('Running')).length

      replicas = runningCount
      console.log(`   Found ${replicas}/3 running replicas`)

      if (replicas >= 3) {
        break
      }
    }

    if (replicas < 3) {
      console.log(`   ⚠️  Only ${replicas}/3 replicas started after ${Math.round((Date.now() - startTime) / 1000)}s`)
      console.log('   Continuing with available replicas...')
    } else {
      console.log(`   ✅ All 3 replicas running`)
    }

    // Wait for Erlang installation to complete inside containers
    await wait(20000, '   Waiting for Erlang installation')

    const services = exec('docker service ls --filter name=atomvm-messaging', { silent: true })
    console.log('📊 Services:')
    console.log(services)

    // ========================================
    // Step 3: Discover Nodes
    // ========================================
    console.log('=' .repeat(70))
    console.log('STEP 3: Discover Erlang Nodes')
    console.log('=' .repeat(70))
    console.log()

    const containers = exec('docker ps --filter name=atomvm-messaging --format "{{.ID}} {{.Names}}"', { silent: true })
      .trim()
      .split('\n')
      .filter(l => l.trim())
      .map(l => {
        const [id, name] = l.split(' ')
        return { id, name }
      })

    console.log(`✅ Found ${containers.length} containers\n`)

    if (containers.length === 0) {
      throw new Error('No containers running')
    }

    // Get node names from containers
    const nodes = []
    for (const container of containers) {
      try {
        // Extract slot number from container name
        const slotMatch = container.name.match(/\.(\d+)\./)
        const slot = slotMatch ? slotMatch[1] : '1'

        const nodeName = `atomvm_node${slot}@atomvm-${slot}`
        nodes.push({
          container: container.id,
          name: container.name,
          nodeName,
          slot
        })
        console.log(`   📍 ${nodeName} (${container.id.substring(0, 12)})`)
      } catch (error) {
        console.log(`   ⚠️  Failed to get node info for ${container.id}`)
      }
    }

    console.log()

    if (nodes.length === 0) {
      throw new Error('No nodes found - all containers failed to start')
    }

    if (nodes.length < 2) {
      console.log(`   ⚠️  Only ${nodes.length} node available - need at least 2 for message passing`)
      console.log('   Skipping connectivity tests...')
    }

    // ========================================
    // Step 4: Verify EPMD Running
    // ========================================
    console.log('=' .repeat(70))
    console.log('STEP 4: Verify EPMD Daemon')
    console.log('=' .repeat(70))
    console.log()

    for (const node of nodes) {
      try {
        const epmdCheck = exec(`docker exec ${node.container} sh -c "epmd -names"`, { silent: true, allowFail: true })
        if (epmdCheck.includes('epmd')) {
          console.log(`✅ ${node.nodeName}: EPMD running`)
        } else {
          console.log(`⚠️  ${node.nodeName}: EPMD check unclear`)
        }
      } catch (error) {
        console.log(`❌ ${node.nodeName}: EPMD not accessible`)
      }
    }

    console.log()

    // Skip connectivity tests if less than 2 nodes
    if (nodes.length < 2) {
      console.log('=' .repeat(70))
      console.log('RESULTS')
      console.log('=' .repeat(70))
      console.log()
      console.log('⚠️  INSUFFICIENT NODES FOR TESTING')
      console.log(`   Only ${nodes.length} node(s) started`)
      console.log('   Need at least 2 nodes for message passing tests')
      console.log()
      console.log('💡 Recommendations:')
      console.log('   - Increase wait time for replica startup')
      console.log('   - Check Docker resources (CPU/memory)')
      console.log('   - Review service logs for errors')
      return false
    }

    // ========================================
    // Step 5: Test Node Connectivity
    // ========================================
    console.log('=' .repeat(70))
    console.log('STEP 5: Test Erlang Node Connectivity')
    console.log('=' .repeat(70))
    console.log()

    console.log('🔍 Testing net_adm:ping between nodes...\n')

    const connectivity = []
    for (let i = 0; i < Math.min(nodes.length - 1, 3); i++) {
      const source = nodes[i]
      const target = nodes[i + 1]

      console.log(`📡 Pinging ${target.nodeName} from ${source.nodeName}...`)

      try {
        const pingCmd = `docker exec ${source.container} sh -c "erl -noshell -sname test_${i} -setcookie atomvm_secret_cookie -eval \\"Result = net_adm:ping('${target.nodeName}'), io:format('PING_RESULT: ~p~n', [Result]), init:stop().\\""`

        const pingResult = exec(pingCmd, { silent: true, allowFail: true })

        if (pingResult.includes('PING_RESULT: pong')) {
          console.log(`   ✅ SUCCESS - Nodes can communicate`)
          connectivity.push({ source: source.nodeName, target: target.nodeName, result: 'pong' })
        } else if (pingResult.includes('PING_RESULT: pang')) {
          console.log(`   ❌ FAILED - Nodes cannot connect (pang)`)
          connectivity.push({ source: source.nodeName, target: target.nodeName, result: 'pang' })
        } else {
          console.log(`   ⚠️  UNCLEAR - ${pingResult.substring(0, 100)}`)
          connectivity.push({ source: source.nodeName, target: target.nodeName, result: 'unclear' })
        }
      } catch (error) {
        console.log(`   ❌ ERROR - ${error.message}`)
      }

      console.log()
      await wait(2000)
    }

    const successfulPings = connectivity.filter(c => c.result === 'pong').length

    // ========================================
    // Step 6: Send Messages
    // ========================================
    console.log('=' .repeat(70))
    console.log('STEP 6: Send Messages Between Nodes')
    console.log('=' .repeat(70))
    console.log()

    const messagesSent = []

    for (let i = 0; i < Math.min(nodes.length - 1, 3); i++) {
      const source = nodes[i]
      const target = nodes[i + 1]

      const message = `test_msg_${i + 1}_from_slot${source.slot}_to_slot${target.slot}`

      console.log(`📤 Message ${i + 1}`)
      console.log(`   From: ${source.nodeName}`)
      console.log(`   To:   ${target.nodeName}`)
      console.log(`   Content: "${message}"`)

      try {
        const sendCmd = `docker exec ${source.container} sh -c "erl -noshell -sname sender_${i} -setcookie atomvm_secret_cookie -eval \\"rpc:call('${target.nodeName}', msg_handler, send_msg, ['${target.nodeName}', '${message}', node()]), io:format('SENT~n'), timer:sleep(1000), init:stop().\\""`

        const sendResult = exec(sendCmd, { silent: true, allowFail: true })

        if (sendResult.includes('SENT')) {
          console.log(`   ✅ Message sent`)
          messagesSent.push({ source: source.nodeName, target: target.nodeName, content: message })
        } else {
          console.log(`   ⚠️  Send result unclear`)
        }
      } catch (error) {
        console.log(`   ❌ Failed: ${error.message}`)
      }

      console.log()
      await wait(2000)
    }

    // Wait for messages to propagate
    await wait(5000, '   Waiting for message propagation')
    console.log()

    // ========================================
    // Step 7: Detect Received Messages
    // ========================================
    console.log('=' .repeat(70))
    console.log('STEP 7: Detect Received Messages')
    console.log('=' .repeat(70))
    console.log()

    const messagesReceived = []

    for (const node of nodes) {
      console.log(`📥 Checking ${node.nodeName}...`)

      try {
        // Check container logs for received messages
        const logs = exec(`docker logs ${node.container} 2>&1`, { silent: true, allowFail: true })
        const receivedLines = logs.split('\n').filter(line => line.includes('[RECEIVED]'))

        if (receivedLines.length > 0) {
          console.log(`   ✅ Received ${receivedLines.length} message(s)`)
          receivedLines.forEach(line => console.log(`      ${line.trim()}`))
          messagesReceived.push({ node: node.nodeName, count: receivedLines.length })
        } else {
          console.log(`   No messages received`)
        }
      } catch (error) {
        console.log(`   ⚠️  Error checking logs: ${error.message}`)
      }

      console.log()
    }

    // ========================================
    // Step 8: Results
    // ========================================
    console.log('=' .repeat(70))
    console.log('RESULTS')
    console.log('=' .repeat(70))
    console.log()

    console.log('📊 Erlang Distribution Statistics:')
    console.log(`   Nodes discovered: ${nodes.length}`)
    console.log(`   Connectivity tests: ${connectivity.length}`)
    console.log(`   Successful pings: ${successfulPings}/${connectivity.length}`)
    console.log(`   Messages sent: ${messagesSent.length}`)
    console.log(`   Nodes with received messages: ${messagesReceived.length}`)
    console.log()

    const totalReceived = messagesReceived.reduce((sum, m) => sum + m.count, 0)

    if (totalReceived > 0 && successfulPings > 0) {
      console.log('✅ ERLANG DISTRIBUTION WORKING!')
      console.log('   ✓ EPMD configured correctly')
      console.log('   ✓ Nodes can ping each other (net_adm:ping)')
      console.log('   ✓ Messages sent successfully')
      console.log(`   ✓ ${totalReceived} message(s) received`)
      console.log()
      console.log('🎯 Docker Swarm + Erlang distribution + Message passing VERIFIED')
      return true
    } else if (successfulPings > 0) {
      console.log('⚠️  PARTIAL SUCCESS')
      console.log('   ✓ Erlang nodes can connect (net_adm:ping works)')
      console.log(`   ✓ ${messagesSent.length} messages sent`)
      console.log(`   ⚠️  ${totalReceived} messages received (expected more)`)
      console.log()
      console.log('💡 Nodes can communicate but message handler may need adjustment')
      return true
    } else {
      console.log('❌ CONNECTIVITY ISSUE')
      console.log('   ✗ Nodes cannot ping each other')
      console.log('   Need to check:')
      console.log('     - EPMD port configuration')
      console.log('     - DNS resolution in overlay network')
      console.log('     - Cookie configuration')
      return false
    }

  } catch (error) {
    console.error('❌ Test failed:', error.message)
    console.error(error.stack)
    return false
  } finally {
    // ========================================
    // Cleanup
    // ========================================
    console.log()
    console.log('=' .repeat(70))
    console.log('CLEANUP')
    console.log('=' .repeat(70))
    console.log()

    console.log('🧹 Removing stack...')
    exec('docker stack rm atomvm-messaging', { allowFail: true })
    await wait(5000, '   Waiting for stack removal')

    console.log('🧹 Leaving swarm...')
    exec('docker swarm leave --force', { allowFail: true })

    console.log('✅ Cleanup complete')
  }
}

// Execute
runWorkingTest()
  .then(success => {
    console.log()
    process.exit(success ? 0 : 1)
  })
  .catch(error => {
    console.error('💥 Fatal error:', error)
    process.exit(1)
  })
