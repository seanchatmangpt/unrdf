#!/usr/bin/env node

/**
 * @fileoverview Express-based Hooks Runtime Server
 * 
 * A simple web server for managing and executing Knowledge Hooks
 */

import express from 'express'
import cors from 'cors'
import { defineHook, evaluateHook, planHook, registerPredicate } from '../src/hooks.mjs'
import { initStore } from '../src/context/index.mjs'
import { useTurtle } from '../src/composables/use-turtle.mjs'
import { useGraph } from '../src/composables/use-graph.mjs'
import fs from 'node:fs/promises'
import path from 'node:path'
import { fileURLToPath } from 'node:url'

const __filename = fileURLToPath(import.meta.url)
const __dirname = path.dirname(__filename)

const app = express()
const PORT = process.env.PORT || 3000

// Middleware
app.use(cors())
app.use(express.json())
app.use(express.static(path.join(__dirname, 'public')))

// In-memory stores
const hookRegistry = new Map()
const hookResults = new Map()
const dataStore = new Map()

// Custom predicates
registerPredicate('HEALTH_SCORE', async (spec, ctx) => {
  const { rows } = ctx
  let totalScore = 0
  let serviceCount = 0
  
  for (const row of rows) {
    const errorRate = Number(row.errorRate?.value ?? 0)
    const latency = Number(row.latency?.value ?? 0)
    
    const errorScore = Math.max(0, 100 - (errorRate * 1000))
    const latencyScore = Math.max(0, 100 - (latency / 20))
    const serviceScore = (errorScore + latencyScore) / 2
    
    totalScore += serviceScore
    serviceCount++
  }
  
  const avgScore = serviceCount > 0 ? totalScore / serviceCount : 0
  const threshold = spec.threshold || 70
  
  return {
    ok: avgScore >= threshold,
    meta: {
      avgScore: Math.round(avgScore * 100) / 100,
      threshold,
      serviceCount,
      kind: 'HEALTH_SCORE'
    }
  }
})

// Routes

/**
 * GET / - Serve the web interface
 */
app.get('/', (req, res) => {
  res.sendFile(path.join(__dirname, 'public', 'index.html'))
})

/**
 * GET /api/hooks - List all hooks
 */
app.get('/api/hooks', (req, res) => {
  res.json({
    hooks: Array.from(hookRegistry.values()),
    total: hookRegistry.size,
    timestamp: new Date().toISOString()
  })
})

/**
 * POST /api/hooks - Create a new hook
 */
app.post('/api/hooks', async (req, res) => {
  try {
    const { id, name, description, select, predicates, combine } = req.body
    
    if (!id) {
      return res.status(400).json({ error: 'Hook ID is required' })
    }
    
    const hook = defineHook({
      id,
      name,
      description,
      select,
      predicates: predicates || [],
      combine: combine || 'AND'
    })
    
    hookRegistry.set(hook.id, hook)
    
    res.json({
      success: true,
      hook: {
        id: hook.id,
        name: hook.name,
        predicates: hook.predicates.length,
        combine: hook.combine
      },
      timestamp: new Date().toISOString()
    })
  } catch (error) {
    res.status(400).json({ error: error.message })
  }
})

/**
 * GET /api/hooks/:id - Get specific hook
 */
app.get('/api/hooks/:id', (req, res) => {
  const { id } = req.params
  
  if (!hookRegistry.has(id)) {
    return res.status(404).json({ error: 'Hook not found' })
  }
  
  const hook = hookRegistry.get(id)
  const results = hookResults.get(id) || []
  
  res.json({
    hook,
    recentResults: results.slice(-10),
    totalEvaluations: results.length,
    timestamp: new Date().toISOString()
  })
})

/**
 * POST /api/hooks/:id/evaluate - Evaluate a hook
 */
app.post('/api/hooks/:id/evaluate', async (req, res) => {
  try {
    const { id } = req.params
    const { data } = req.body
    
    if (!hookRegistry.has(id)) {
      return res.status(404).json({ error: 'Hook not found' })
    }
    
    const hook = hookRegistry.get(id)
    
    // Initialize store with composable architecture
    const runApp = initStore()
    
    const result = await runApp(async () => {
      const turtle = await useTurtle()
      
      // Load data if provided
      if (data) {
        await turtle.parse(data)
      } else {
        // Use default sample data
        const sampleData = `
@prefix ex: <http://example.org/> .

ex:service1 a ex:Service ;
  ex:errorRate 0.05 ;
  ex:latency 1500 ;
  ex:requests 1000 .

ex:service2 a ex:Service ;
  ex:errorRate 0.01 ;
  ex:latency 300 ;
  ex:requests 2000 .

ex:service3 a ex:Service ;
  ex:errorRate 0.08 ;
  ex:latency 2000 ;
  ex:requests 500 .
`
        await turtle.parse(sampleData)
      }
      
      // Evaluate hook
      const receipt = await evaluateHook(hook)
      
      // Store result
      const results = hookResults.get(id) || []
      results.push(receipt)
      hookResults.set(id, results)
      
      return receipt
    })
    
    res.json({
      success: true,
      result,
      timestamp: new Date().toISOString()
    })
  } catch (error) {
    res.status(500).json({ error: error.message })
  }
})

/**
 * POST /api/hooks/:id/plan - Plan hook execution
 */
app.post('/api/hooks/:id/plan', (req, res) => {
  const { id } = req.params
  
  if (!hookRegistry.has(id)) {
    return res.status(404).json({ error: 'Hook not found' })
  }
  
  const hook = hookRegistry.get(id)
  const plan = planHook(hook)
  
  res.json({
    plan,
    timestamp: new Date().toISOString()
  })
})

/**
 * DELETE /api/hooks/:id - Delete a hook
 */
app.delete('/api/hooks/:id', (req, res) => {
  const { id } = req.params
  
  if (!hookRegistry.has(id)) {
    return res.status(404).json({ error: 'Hook not found' })
  }
  
  hookRegistry.delete(id)
  hookResults.delete(id)
  
  res.json({
    success: true,
    message: `Hook ${id} deleted`,
    timestamp: new Date().toISOString()
  })
})

/**
 * GET /api/data - List all data sources
 */
app.get('/api/data', (req, res) => {
  res.json({
    dataSources: Array.from(dataStore.entries()).map(([id, data]) => ({
      id,
      name: data.name,
      format: data.format,
      size: data.content.length,
      createdAt: data.createdAt
    })),
    total: dataStore.size,
    timestamp: new Date().toISOString()
  })
})

/**
 * POST /api/data - Create a new data source
 */
app.post('/api/data', async (req, res) => {
  try {
    const { id, name, content, format } = req.body
    
    if (!id || !content) {
      return res.status(400).json({ error: 'ID and content are required' })
    }
    
    const dataSource = {
      id,
      name: name || id,
      content,
      format: format || 'Turtle',
      createdAt: new Date().toISOString()
    }
    
    dataStore.set(id, dataSource)
    
    res.json({
      success: true,
      dataSource: {
        id: dataSource.id,
        name: dataSource.name,
        format: dataSource.format,
        size: dataSource.content.length
      },
      timestamp: new Date().toISOString()
    })
  } catch (error) {
    res.status(400).json({ error: error.message })
  }
})

/**
 * POST /api/data/:id/query - Query data source
 */
app.post('/api/data/:id/query', async (req, res) => {
  try {
    const { id } = req.params
    const { query } = req.body
    
    if (!dataStore.has(id)) {
      return res.status(404).json({ error: 'Data source not found' })
    }
    
    if (!query) {
      return res.status(400).json({ error: 'Query is required' })
    }
    
    const dataSource = dataStore.get(id)
    
    // Initialize store with composable architecture
    const runApp = initStore()
    
    const result = await runApp(async () => {
      const turtle = await useTurtle()
      await turtle.parse(dataSource.content)
      
      const graph = await useGraph()
      
      if (query.trim().toUpperCase().startsWith('SELECT')) {
        return await graph.select(query)
      } else if (query.trim().toUpperCase().startsWith('ASK')) {
        return await graph.ask(query)
      } else {
        throw new Error('Only SELECT and ASK queries are supported')
      }
    })
    
    res.json({
      success: true,
      query,
      result,
      timestamp: new Date().toISOString()
    })
  } catch (error) {
    res.status(500).json({ error: error.message })
  }
})

/**
 * GET /api/runtime/status - Get runtime status
 */
app.get('/api/runtime/status', (req, res) => {
  const totalHooks = hookRegistry.size
  const totalDataSources = dataStore.size
  const totalEvaluations = Array.from(hookResults.values()).reduce((sum, results) => sum + results.length, 0)
  
  // Get recent activity
  const recentActivity = []
  for (const [hookId, results] of hookResults.entries()) {
    if (results.length > 0) {
      const lastResult = results[results.length - 1]
      recentActivity.push({
        hookId,
        fired: lastResult.fired,
        timestamp: lastResult.at,
        duration: lastResult.durations.totalMs
      })
    }
  }
  
  // Sort by timestamp
  recentActivity.sort((a, b) => new Date(b.timestamp) - new Date(a.timestamp))
  
  res.json({
    status: 'running',
    uptime: process.uptime(),
    memory: process.memoryUsage(),
    hooks: {
      total: totalHooks,
      active: totalHooks,
      evaluations: totalEvaluations
    },
    data: {
      total: totalDataSources,
      active: totalDataSources
    },
    recentActivity: recentActivity.slice(0, 10),
    timestamp: new Date().toISOString()
  })
})

/**
 * POST /api/runtime/status - Execute runtime commands
 */
app.post('/api/runtime/status', (req, res) => {
  const { command } = req.body
  
  switch (command) {
    case 'clear-results':
      hookResults.clear()
      res.json({
        success: true,
        message: 'All hook results cleared',
        timestamp: new Date().toISOString()
      })
      break
    
    case 'reset-runtime':
      hookRegistry.clear()
      hookResults.clear()
      dataStore.clear()
      res.json({
        success: true,
        message: 'Runtime reset complete',
        timestamp: new Date().toISOString()
      })
      break
    
    case 'health-check':
      res.json({
        success: true,
        status: 'healthy',
        timestamp: new Date().toISOString()
      })
      break
    
    default:
      res.status(400).json({ error: `Unknown command: ${command}` })
  }
})

// Error handling middleware
app.use((err, req, res, next) => {
  console.error(err.stack)
  res.status(500).json({ error: 'Something went wrong!' })
})

// Start server
app.listen(PORT, () => {
  console.log(`🚀 UNRDF Hooks Runtime Server running on http://localhost:${PORT}`)
  console.log(`📊 Web Interface: http://localhost:${PORT}`)
  console.log(`🔧 API Endpoints:`)
  console.log(`   GET  /api/hooks - List hooks`)
  console.log(`   POST /api/hooks - Create hook`)
  console.log(`   POST /api/hooks/:id/evaluate - Evaluate hook`)
  console.log(`   GET  /api/runtime/status - Runtime status`)
  console.log(`\n✨ Ready to manage Knowledge Hooks!`)
})
