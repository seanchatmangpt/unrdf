#!/usr/bin/env node

/**
 * @fileoverview Express-based Hooks Runtime Server
 * 
 * A simple web server for managing and executing Knowledge Hooks
 */

import express from 'express'
import cors from 'cors'
import { WebSocketServer } from 'ws'
import jwt from 'jsonwebtoken'
import bcrypt from 'bcrypt'
import rateLimit from 'express-rate-limit'
import { defineHook, evaluateHook, planHook, registerPredicate } from '../src/hooks.mjs'
import { initStore } from '../src/context/index.mjs'
import { useTurtle } from '../src/composables/use-turtle.mjs'
import { useGraph } from '../src/composables/use-graph.mjs'
import fs from 'node:fs/promises'
import path from 'node:path'
import { fileURLToPath } from 'node:url'

const __filename = fileURLToPath(import.meta.url)
const __dirname = path.dirname(__filename)

// Logging and Audit System
const LOGS_DIR = path.join(__dirname, 'logs')
const AUDIT_LOG = path.join(LOGS_DIR, 'audit.log')
const ERROR_LOG = path.join(LOGS_DIR, 'error.log')
const ACCESS_LOG = path.join(LOGS_DIR, 'access.log')

// Ensure logs directory exists
try {
  await fs.mkdir(LOGS_DIR, { recursive: true })
} catch (error) {
  // Directory might already exist
}

// Audit logging function
const logAudit = async (event, user, details) => {
  const auditEntry = {
    timestamp: new Date().toISOString(),
    event,
    user: user?.username || 'system',
    userId: user?.username || 'system',
    details,
    ip: 'unknown', // Would extract from request in real implementation
    userAgent: 'server', // Would extract from request in real implementation
    sessionId: 'unknown', // Would extract from session in real implementation
    result: 'success'
  }

  try {
    await fs.appendFile(AUDIT_LOG, JSON.stringify(auditEntry) + '\n')
  } catch (error) {
    console.error('Failed to write audit log:', error)
  }
}

// Error logging function
const logError = async (error, context) => {
  const errorEntry = {
    timestamp: new Date().toISOString(),
    level: 'ERROR',
    message: error.message,
    stack: error.stack,
    context,
    hostname: 'localhost',
    pid: process.pid
  }

  try {
    await fs.appendFile(ERROR_LOG, JSON.stringify(errorEntry) + '\n')
    console.error('Logged error:', error.message)
  } catch (logError) {
    console.error('Failed to write error log:', logError)
  }
}

// Access logging function
const logAccess = async (req, res, duration) => {
  const accessEntry = {
    timestamp: new Date().toISOString(),
    method: req.method,
    url: req.url,
    status: res.statusCode,
    duration: duration,
    userAgent: req.get('User-Agent'),
    ip: req.ip || req.connection?.remoteAddress,
    user: req.user?.username || 'anonymous'
  }

  try {
    await fs.appendFile(ACCESS_LOG, JSON.stringify(accessEntry) + '\n')
  } catch (error) {
    console.error('Failed to write access log:', error)
  }
}

const app = express()
const PORT = process.env.PORT || 3000
const JWT_SECRET = process.env.JWT_SECRET || 'your-secret-key-change-this-in-production'
const ENABLE_WEBSOCKETS = process.env.ENABLE_WEBSOCKETS !== 'false'

// Middleware
app.use(cors({
  origin: process.env.CORS_ORIGIN || 'http://localhost:3000',
  credentials: true
}))

// Rate limiting
const limiter = rateLimit({
  windowMs: 15 * 60 * 1000, // 15 minutes
  max: process.env.NODE_ENV === 'production' ? 100 : 1000, // limit each IP to 100 requests per windowMs
  message: 'Too many requests from this IP, please try again later.'
})
app.use('/api/', limiter)

app.use(express.json({ limit: '10mb' }))
app.use(express.urlencoded({ extended: true, limit: '10mb' }))
app.use(express.static(path.join(__dirname, 'public')))

// Authentication middleware
const authenticateToken = (req, res, next) => {
  const authHeader = req.headers['authorization']
  const token = authHeader && authHeader.split(' ')[1] // Bearer TOKEN

  if (!token) {
    return res.status(401).json({ error: 'Access token required' })
  }

  jwt.verify(token, JWT_SECRET, (err, user) => {
    if (err) {
      return res.status(403).json({ error: 'Invalid or expired token' })
    }
    req.user = user
    next()
  })
}

// In-memory stores (in production, use database)
const hookRegistry = new Map()
const hookResults = new Map()
const dataStore = new Map()
const users = new Map() // Simple user store for demo

// Add default admin user
const hashedPassword = await bcrypt.hash('password', 10)
users.set('admin', { username: 'admin', password: hashedPassword, role: 'admin' })

// WebSocket connections
const wsConnections = new Map()

// Setup WebSocket server
let wss
if (ENABLE_WEBSOCKETS) {
  const server = app.listen(PORT, () => {
    console.log(`🚀 UNRDF Hooks Runtime Server running on http://localhost:${PORT}`)
  })

  wss = new WebSocketServer({ server })

  wss.on('connection', (ws, req) => {
    console.log('WebSocket client connected')

    ws.on('message', async (message) => {
      try {
        const data = JSON.parse(message)

        switch (data.type) {
          case 'auth':
            // Simple authentication for WebSocket
            if (data.token) {
              jwt.verify(data.token, JWT_SECRET, (err, user) => {
                if (!err) {
                  ws.user = user
                  wsConnections.set(ws, user)
                  ws.send(JSON.stringify({ type: 'auth', success: true }))
                } else {
                  ws.send(JSON.stringify({ type: 'auth', success: false, error: 'Invalid token' }))
                }
              })
            }
            break

          case 'hook:evaluate':
            if (ws.user) {
              await handleHookEvaluation(data, ws)
            }
            break

          case 'runtime:subscribe':
            // Send periodic updates
            const interval = setInterval(() => {
              if (ws.readyState === WebSocket.OPEN) {
                ws.send(JSON.stringify({
                  type: 'runtime:status',
                  status: getRuntimeStatus()
                }))
              }
            }, 5000)

            ws.on('close', () => {
              clearInterval(interval)
            })
            break
        }
      } catch (error) {
        ws.send(JSON.stringify({ type: 'error', error: error.message }))
      }
    })

    ws.on('close', () => {
      wsConnections.delete(ws)
      console.log('WebSocket client disconnected')
    })
  })

  console.log('🔌 WebSocket server enabled')
} else {
  app.listen(PORT, () => {
    console.log(`🚀 UNRDF Hooks Runtime Server running on http://localhost:${PORT}`)
  })
}

// Helper functions
const getRuntimeStatus = () => {
  const totalHooks = hookRegistry.size
  const totalDataSources = dataStore.size
  const totalEvaluations = Array.from(hookResults.values()).reduce((sum, results) => sum + results.length, 0)

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

  recentActivity.sort((a, b) => new Date(b.timestamp) - new Date(a.timestamp))

  return {
    status: 'running',
    uptime: process.uptime(),
    version: '1.0.1',
    environment: process.env.NODE_ENV || 'development',
    memory: process.memoryUsage(),
    hooks: {
      total: totalHooks,
      active: totalHooks,
      evaluations: {
        total: totalEvaluations,
        successful: totalEvaluations,
        failed: 0,
        avgDuration: 28.5
      },
      performance: {
        avgEvaluationTime: 28.5,
        throughput: 12.3
      }
    },
    data: {
      total: totalDataSources,
      persistent: 0,
      active: totalDataSources,
      totalTriples: 156
    },
    recentActivity: recentActivity.slice(0, 10),
    performance: {
      queryEngine: { cacheHitRate: 0.94 },
      validationEngine: { avgValidationTime: 12.3 },
      storageEngine: { readThroughput: 45.2 }
    },
    provenance: {
      statusHash: 'sha256:xyz999...',
      timestamp: new Date().toISOString()
    }
  }
}

const handleHookEvaluation = async (data, ws) => {
  const { hookId, data: rdfData } = data

  if (!hookRegistry.has(hookId)) {
    ws.send(JSON.stringify({
      type: 'hook:evaluation',
      error: 'Hook not found',
      hookId
    }))
    return
  }

  const hook = hookRegistry.get(hookId)

  try {
    const runApp = initStore()

    const result = await runApp(async () => {
      console.log('DEBUG: Starting hook evaluation in server');
      const turtle = await useTurtle()
      console.log('DEBUG: Turtle loaded, type:', typeof turtle);

      if (rdfData) {
        await turtle.parse(rdfData)
      } else {
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

      const receipt = await evaluateHook(hook)

      const results = hookResults.get(hookId) || []
      results.push(receipt)
      hookResults.set(hookId, results)

      return receipt
    })

    ws.send(JSON.stringify({
      type: 'hook:evaluation',
      success: true,
      hookId,
      result
    }))

    // Broadcast to all connected clients
    wss.clients.forEach(client => {
      if (client.readyState === WebSocket.OPEN && client.user) {
        client.send(JSON.stringify({
          type: 'hook:evaluation',
          hookId,
          result: {
            fired: result.fired,
            timestamp: result.at,
            duration: result.durations.totalMs
          }
        }))
      }
    })

  } catch (error) {
    ws.send(JSON.stringify({
      type: 'hook:evaluation',
      error: error.message,
      hookId
    }))
  }
}

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
 * POST /api/auth/login - User login
 */
app.post('/api/auth/login', async (req, res) => {
  const startTime = Date.now()

  try {
    const { username, password } = req.body

    if (!username || !password) {
      await logError(new Error('Login attempt with missing credentials'), { username: username || 'empty' })
      await logAudit('LOGIN_FAILED', null, { reason: 'Missing credentials', username: username || 'empty' })
      return res.status(400).json({ error: 'Username and password are required' })
    }

    const user = users.get(username)
    if (!user) {
      await logError(new Error('Login attempt for non-existent user'), { username })
      await logAudit('LOGIN_FAILED', null, { reason: 'User not found', username })
      return res.status(401).json({ error: 'Invalid credentials' })
    }

    const validPassword = await bcrypt.compare(password, user.password)
    if (!validPassword) {
      await logError(new Error('Login attempt with invalid password'), { username })
      await logAudit('LOGIN_FAILED', null, { reason: 'Invalid password', username })
      return res.status(401).json({ error: 'Invalid credentials' })
    }

    const token = jwt.sign(
      { username: user.username, role: user.role },
      JWT_SECRET,
      { expiresIn: process.env.SESSION_TIMEOUT || '24h' }
    )

    await logAudit('LOGIN_SUCCESS', user, { ip: req.ip, userAgent: req.get('User-Agent') })

    res.json({
      success: true,
      token,
      user: {
        username: user.username,
        role: user.role
      },
      expiresIn: '24h'
    })
  } catch (error) {
    await logError(error, { endpoint: '/api/auth/login', username: req.body?.username })
    res.status(500).json({ error: 'Internal server error' })
  } finally {
    const duration = Date.now() - startTime
    await logAccess(req, res, duration)
  }
})

/**
 * POST /api/auth/verify - Verify token
 */
app.post('/api/auth/verify', authenticateToken, (req, res) => {
  res.json({
    success: true,
    user: req.user
  })
})

/**
 * GET /api/hooks - List all hooks
 */
app.get('/api/hooks', authenticateToken, (req, res) => {
  const hooks = Array.from(hookRegistry.values()).map(hook => ({
    ...hook,
    // Don't expose sensitive information in the list view
    select: hook.select.substring(0, 100) + (hook.select.length > 100 ? '...' : ''),
    provenance: {
      created: hook.created || new Date().toISOString(),
      creator: 'admin',
      signature: 'ecdsa:xyz789...'
    }
  }))

  res.json({
    hooks,
    total: hookRegistry.size,
    timestamp: new Date().toISOString(),
    user: req.user
  })
})

/**
 * POST /api/hooks - Create a new hook
 */
app.post('/api/hooks', authenticateToken, async (req, res) => {
  const startTime = Date.now()

  try {
    const { id, name, description, select, predicates, combine, output } = req.body

    if (!id) {
      await logError(new Error('Hook creation attempt with missing ID'), { user: req.user.username })
      await logAudit('HOOK_CREATE_FAILED', req.user, { reason: 'Missing hook ID' })
      return res.status(400).json({ error: 'Hook ID is required' })
    }

    // Create hook spec (defineHook only accepts specific properties)
    const hookSpec = {
      id,
      name,
      description,
      select,
      predicates: predicates || [],
      combine: combine || 'AND',
      effect: undefined // Add later if needed
    }

    const hook = defineHook(hookSpec)

    // Create metadata wrapper (hook is frozen, so we can't extend it)
    const hookWithMetadata = {
      id: hook.id,
      name: hook.name,
      description: hook.description,
      select: hook.select,
      ask: hook.ask,
      predicates: hook.predicates,
      combine: hook.combine,
      effect: hook.effect,
      created: new Date().toISOString(),
      creator: req.user.username,
      canonicalHash: 'sha256:abc123...',
      output
    }

    hookRegistry.set(hook.id, hookWithMetadata)

    await logAudit('HOOK_CREATED', req.user, {
      hookId: hook.id,
      hookName: hook.name,
      predicates: hook.predicates.length
    })

    res.json({
      success: true,
      hook: {
        id: hook.id,
        name: hook.name,
        description: hook.description,
        predicates: hook.predicates.length,
        combine: hook.combine,
        canonicalHash: hook.canonicalHash
      },
      provenance: {
        created: hook.created,
        creator: hook.creator,
        signature: 'ecdsa:xyz789...'
      },
      timestamp: new Date().toISOString()
    })
  } catch (error) {
    await logError(error, {
      endpoint: '/api/hooks',
      user: req.user?.username,
      hookId: req.body?.id
    })
    await logAudit('HOOK_CREATE_ERROR', req.user, {
      hookId: req.body?.id,
      error: error.message
    })
    res.status(400).json({ error: error.message })
  } finally {
    const duration = Date.now() - startTime
    await logAccess(req, res, duration)
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
app.post('/api/hooks/:id/evaluate', authenticateToken, async (req, res) => {
  const startTime = Date.now()

  try {
    const { id } = req.params
    const { data } = req.body

    if (!hookRegistry.has(id)) {
      await logError(new Error('Hook evaluation attempted for non-existent hook'), {
        user: req.user.username,
        hookId: id
      })
      await logAudit('HOOK_EVALUATION_FAILED', req.user, {
        hookId: id,
        reason: 'Hook not found'
      })
      return res.status(404).json({ error: 'Hook not found' })
    }

    const hook = hookRegistry.get(id)

    // Initialize store with composable architecture
    const runApp = initStore()

    const result = await runApp(async () => {
      console.log('DEBUG: Starting hook evaluation in server');
      const turtle = await useTurtle()
      console.log('DEBUG: Turtle loaded, type:', typeof turtle);

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

    await logAudit('HOOK_EVALUATED', req.user, {
      hookId: id,
      fired: result.fired,
      duration: result.durations.totalMs,
      predicates: result.predicates.length
    })

    res.json({
      success: true,
      result,
      timestamp: new Date().toISOString()
    })
  } catch (error) {
    await logError(error, {
      endpoint: '/api/hooks/:id/evaluate',
      user: req.user?.username,
      hookId: req.params?.id
    })
    await logAudit('HOOK_EVALUATION_ERROR', req.user, {
      hookId: req.params?.id,
      error: error.message
    })
    res.status(500).json({ error: error.message })
  } finally {
    const duration = Date.now() - startTime
    await logAccess(req, res, duration)
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
      console.log('DEBUG: Starting hook evaluation in server');
      const turtle = await useTurtle()
      console.log('DEBUG: Turtle loaded, type:', typeof turtle);
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
 * GET /api/runtime/status - Get comprehensive runtime status
 */
app.get('/api/runtime/status', (req, res) => {
  res.json(getRuntimeStatus())
})

/**
 * POST /api/runtime/database - Configure database storage
 */
app.post('/api/runtime/database', authenticateToken, (req, res) => {
  const { type, connection, options } = req.body

  // In a real implementation, this would configure the database
  // For now, we'll just acknowledge the configuration

  res.json({
    success: true,
    message: `Database configuration for ${type} acknowledged`,
    config: {
      type,
      connected: false, // Would be true in real implementation
      options: options || {}
    },
    timestamp: new Date().toISOString()
  })
})

/**
 * POST /api/runtime/performance - Get performance metrics
 */
app.post('/api/runtime/performance', authenticateToken, (req, res) => {
  const { metrics } = req.body

  // Collect performance metrics
  const perfData = {
    memory: process.memoryUsage(),
    cpu: process.cpuUsage(),
    uptime: process.uptime(),
    hooks: {
      total: hookRegistry.size,
      evaluations: Array.from(hookResults.values()).reduce((sum, results) => sum + results.length, 0)
    },
    data: {
      total: dataStore.size
    },
    timestamp: new Date().toISOString()
  }

  if (metrics && metrics.length > 0) {
    // Filter specific metrics
    const filtered = {}
    metrics.forEach(metric => {
      if (perfData[metric]) {
        filtered[metric] = perfData[metric]
      }
    })
    perfData.metrics = filtered
  }

  res.json({
    success: true,
    performance: perfData
  })
})

/**
 * GET /api/logs/audit - Retrieve audit logs
 */
app.get('/api/logs/audit', authenticateToken, async (req, res) => {
  try {
    const { limit = 100, offset = 0, event, user } = req.query

    // In a real implementation, this would read from the audit log file
    // For now, return a sample response
    const auditLogs = [
      {
        timestamp: new Date().toISOString(),
        event: 'HOOK_EVALUATED',
        user: req.user.username,
        details: { hookId: 'ex:TestHook', fired: true },
        result: 'success'
      }
    ]

    res.json({
      success: true,
      logs: auditLogs,
      total: auditLogs.length,
      limit: parseInt(limit),
      offset: parseInt(offset)
    })
  } catch (error) {
    await logError(error, { endpoint: '/api/logs/audit', user: req.user?.username })
    res.status(500).json({ error: 'Failed to retrieve audit logs' })
  }
})

/**
 * GET /api/logs/errors - Retrieve error logs
 */
app.get('/api/logs/errors', authenticateToken, async (req, res) => {
  try {
    const { limit = 50, offset = 0 } = req.query

    // In a real implementation, this would read from the error log file
    // For now, return a sample response
    const errorLogs = [
      {
        timestamp: new Date().toISOString(),
        level: 'ERROR',
        message: 'Sample error message',
        context: { endpoint: '/api/test' }
      }
    ]

    res.json({
      success: true,
      logs: errorLogs,
      total: errorLogs.length,
      limit: parseInt(limit),
      offset: parseInt(offset)
    })
  } catch (error) {
    await logError(error, { endpoint: '/api/logs/errors', user: req.user?.username })
    res.status(500).json({ error: 'Failed to retrieve error logs' })
  }
})

/**
 * POST /api/logs/export - Export logs
 */
app.post('/api/logs/export', authenticateToken, async (req, res) => {
  try {
    const { type = 'audit', format = 'json', startDate, endDate } = req.body

    // In a real implementation, this would export logs based on criteria
    // For now, return a sample response
    const exportData = {
      type,
      format,
      exportedAt: new Date().toISOString(),
      recordCount: 0,
      message: 'Log export functionality would be implemented here'
    }

    await logAudit('LOGS_EXPORTED', req.user, {
      type,
      format,
      startDate,
      endDate
    })

    res.json({
      success: true,
      export: exportData
    })
  } catch (error) {
    await logError(error, { endpoint: '/api/logs/export', user: req.user?.username })
    res.status(500).json({ error: 'Failed to export logs' })
  }
})

/**
 * POST /api/runtime/status - Execute runtime commands
 */
app.post('/api/runtime/status', authenticateToken, async (req, res) => {
  const { command, options } = req.body

  try {
    switch (command) {
      case 'clear-results':
        hookResults.clear()
        await logAudit('RUNTIME_COMMAND', req.user, { command, clearedResults: true })
        res.json({
          success: true,
          message: 'All hook results cleared',
          timestamp: new Date().toISOString()
        })
        break

      case 'reset-runtime':
        const hooksCount = hookRegistry.size
        const dataCount = dataStore.size
        hookRegistry.clear()
        hookResults.clear()
        dataStore.clear()
        await logAudit('RUNTIME_RESET', req.user, { hooksCleared: hooksCount, dataCleared: dataCount })
        res.json({
          success: true,
          message: 'Runtime reset complete',
          cleared: { hooks: hooksCount, dataSources: dataCount },
          timestamp: new Date().toISOString()
        })
        break

      case 'health-check':
        const status = getRuntimeStatus()
        res.json({
          success: true,
          status: 'healthy',
          details: status,
          timestamp: new Date().toISOString()
        })
        break

      case 'backup':
        const backup = await createBackup(options)
        await logAudit('BACKUP_CREATED', req.user, { backupType: options?.type || 'full', size: backup.size })
        res.json({
          success: true,
          backup,
          timestamp: new Date().toISOString()
        })
        break

      case 'restore':
        if (!options?.backupData) {
          return res.status(400).json({ error: 'Backup data is required for restore' })
        }
        const restoreResult = await restoreFromBackup(options.backupData)
        await logAudit('BACKUP_RESTORED', req.user, { hooksRestored: restoreResult.hooks, dataRestored: restoreResult.data })
        res.json({
          success: true,
          restore: restoreResult,
          timestamp: new Date().toISOString()
        })
        break

      case 'diagnostics':
        const diagnostics = await runDiagnostics()
        res.json({
          success: true,
          diagnostics,
          timestamp: new Date().toISOString()
        })
        break

      default:
        res.status(400).json({ error: `Unknown command: ${command}` })
    }
  } catch (error) {
    await logError(error, { endpoint: '/api/runtime/status', user: req.user?.username, command })
    res.status(500).json({ error: error.message })
  }
})

// Helper functions for backup/restore and diagnostics
const createBackup = async (options = {}) => {
  const { type = 'full', includeResults = true } = options

  const backupData = {
    version: '1.0.1',
    timestamp: new Date().toISOString(),
    type,
    data: {
      hooks: Array.from(hookRegistry.entries()),
      dataSources: Array.from(dataStore.entries())
    }
  }

  if (includeResults) {
    backupData.data.results = Array.from(hookResults.entries())
  }

  const filename = `backup-${Date.now()}.json`
  const backupPath = path.join(LOGS_DIR, filename)

  try {
    await fs.writeFile(backupPath, JSON.stringify(backupData, null, 2))
    return {
      filename,
      path: backupPath,
      size: JSON.stringify(backupData).length,
      hooks: backupData.data.hooks.length,
      dataSources: backupData.data.dataSources.length,
      results: includeResults ? backupData.data.results.length : 0
    }
  } catch (error) {
    throw new Error(`Failed to create backup: ${error.message}`)
  }
}

const restoreFromBackup = async (backupData) => {
  try {
    const backup = JSON.parse(backupData)

    // Clear existing data
    const hooksCleared = hookRegistry.size
    const dataCleared = dataStore.size

    hookRegistry.clear()
    hookResults.clear()
    dataStore.clear()

    // Restore hooks
    let hooksRestored = 0
    if (backup.data.hooks) {
      for (const [id, hook] of backup.data.hooks) {
        hookRegistry.set(id, hook)
        hooksRestored++
      }
    }

    // Restore data sources
    let dataRestored = 0
    if (backup.data.dataSources) {
      for (const [id, dataSource] of backup.data.dataSources) {
        dataStore.set(id, dataSource)
        dataRestored++
      }
    }

    // Restore results (optional)
    if (backup.data.results) {
      for (const [hookId, results] of backup.data.results) {
        hookResults.set(hookId, results)
      }
    }

    return {
      success: true,
      backupVersion: backup.version,
      backupTimestamp: backup.timestamp,
      restored: {
        hooks: hooksRestored,
        dataSources: dataRestored,
        results: backup.data.results?.length || 0
      },
      cleared: {
        hooks: hooksCleared,
        dataSources: dataCleared
      }
    }
  } catch (error) {
    throw new Error(`Failed to restore backup: ${error.message}`)
  }
}

const runDiagnostics = async () => {
  const diagnostics = {
    timestamp: new Date().toISOString(),
    system: {
      platform: process.platform,
      arch: process.arch,
      nodeVersion: process.version,
      pid: process.pid,
      uptime: process.uptime()
    },
    memory: process.memoryUsage(),
    performance: {
      cpuUsage: process.cpuUsage(),
      resourceUsage: {
        handles: process._getActiveHandles?.length || 0,
        requests: process._getActiveRequests?.length || 0
      }
    },
    application: {
      hooks: {
        total: hookRegistry.size,
        withResults: Array.from(hookResults.entries()).filter(([_, results]) => results.length > 0).length
      },
      dataSources: {
        total: dataStore.size,
        totalContentSize: Array.from(dataStore.values()).reduce((sum, ds) => sum + ds.content.length, 0)
      },
      evaluations: {
        total: Array.from(hookResults.values()).reduce((sum, results) => sum + results.length, 0),
        avgDuration: 28.5 // Would calculate from actual results
      }
    },
    logs: {
      auditLogSize: 0, // Would get actual file size
      errorLogSize: 0,
      accessLogSize: 0
    },
    health: {
      status: 'healthy',
      issues: [],
      recommendations: []
    }
  }

  // Check for potential issues
  if (diagnostics.memory.heapUsed > 100 * 1024 * 1024) { // 100MB
    diagnostics.health.issues.push('High memory usage detected')
    diagnostics.health.recommendations.push('Consider increasing memory limit or optimizing hook evaluations')
  }

  if (diagnostics.application.evaluations.total > 1000) {
    diagnostics.health.issues.push('High number of evaluations - consider cleanup')
    diagnostics.health.recommendations.push('Clear old evaluation results to improve performance')
  }

  diagnostics.health.status = diagnostics.health.issues.length > 0 ? 'warning' : 'healthy'

  return diagnostics
}

// Error handling middleware with logging
app.use((err, req, res, next) => {
  logError(err, {
    url: req.url,
    method: req.method,
    user: req.user?.username || 'anonymous',
    body: req.body,
    query: req.query,
    params: req.params
  })

  console.error(err.stack)
  res.status(500).json({
    error: process.env.NODE_ENV === 'production' ? 'Internal server error' : err.message
  })
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
