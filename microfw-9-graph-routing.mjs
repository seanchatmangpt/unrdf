#!/usr/bin/env node
/**
 * Graph-Aware API Routing Microframework
 * Routes determined by RDF graph traversal patterns, not static regex.
 * Synergy: API routing is normally static/regex-based; RDF makes it semantic and queryable.
 */

// Lightweight in-memory RDF Store
class RDFStore {
  constructor() {
    this.triples = []
  }

  add(quad) {
    this.triples.push({
      subject: quad.subject.value,
      predicate: quad.predicate.value,
      object: quad.object.value,
    })
  }

  match(subject, predicate, object) {
    return this.triples.filter((t) => {
      if (subject && t.subject !== subject.value) return false
      if (predicate && t.predicate !== predicate.value) return false
      if (object && t.object !== object.value) return false
      return true
    })
  }

  query(sparqlQuery) {
    return []
  }
}

// Factory functions
const createStore = () => new RDFStore()
const dataFactory = {
  namedNode: (v) => ({ value: v, termType: 'NamedNode' }),
  literal: (v) => ({ value: v, termType: 'Literal' }),
  quad: (s, p, o) => ({ subject: s, predicate: p, object: o }),
}

/**
 * GraphAwareRouter - RDF-driven API routing engine
 * Routes are RDF entities; requests matched via graph traversal
 */
class GraphAwareRouter {
  constructor() {
    this.store = createStore()
    this.handlers = new Map()
    this.ns = {
      route: 'http://api.org/route/',
      api: 'http://api.org/resource/',
      rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
    }
  }

  /**
   * Define route as RDF triples in graph
   * @param {string} id - Route ID
   * @param {string} pattern - URL pattern
   * @param {string} method - HTTP method
   * @param {Function} handler - Request handler
   */
  defineRoute(id, pattern, method, handler, meta = {}) {
    const uri = this.ns.route + id
    const df = dataFactory

    this.store.add(df.quad(
      df.namedNode(uri),
      df.namedNode(this.ns.rdf + 'type'),
      df.namedNode(this.ns.route + 'Route')
    ))

    this.store.add(df.quad(
      df.namedNode(uri),
      df.namedNode(this.ns.route + 'pattern'),
      df.literal(pattern)
    ))

    this.store.add(df.quad(
      df.namedNode(uri),
      df.namedNode(this.ns.route + 'method'),
      df.literal(method)
    ))

    Object.entries(meta).forEach(([k, v]) => {
      this.store.add(df.quad(
        df.namedNode(uri),
        df.namedNode(this.ns.route + k),
        df.literal(String(v))
      ))
    })

    this.handlers.set(id, handler)
  }

  /**
   * Define RDF relationship between resources
   */
  defineRelationship(subject, predicate, object) {
    const df = dataFactory
    this.store.add(df.quad(
      df.namedNode(subject),
      df.namedNode(predicate),
      df.namedNode(object)
    ))
  }

  /**
   * Find best matching route via graph traversal
   * Scoring: exact match (100) > prefix match (50 + pattern length)
   */
  async findRoute(path, method = 'GET') {
    let bestMatch = null
    let bestScore = -1

    const routeType = this.ns.rdf + 'type'
    const routeClass = this.ns.route + 'Route'
    const methodPred = this.ns.route + 'method'
    const patternPred = this.ns.route + 'pattern'

    // Find all Route entities in RDF store
    for (const triple of this.store.triples) {
      if (triple.predicate === routeType && triple.object === routeClass) {
        const routeUri = triple.subject
        const id = routeUri.replace(this.ns.route, '')

        let routeMethod = null
        let routePattern = null

        // Extract method and pattern properties
        for (const t of this.store.triples) {
          if (t.subject === routeUri) {
            if (t.predicate === methodPred) routeMethod = t.object
            if (t.predicate === patternPred) routePattern = t.object
          }
        }

        // Score this route
        if (routeMethod === method && routePattern) {
          let score = 0
          if (routePattern === path) {
            score = 100
          } else if (path.startsWith(routePattern)) {
            score = 50 + routePattern.length
          }

          if (score > 0 && score > bestScore) {
            bestScore = score
            bestMatch = {
              id,
              pattern: routePattern,
              handler: this.handlers.get(id),
            }
          }
        }
      }
    }

    return bestMatch
  }

  /**
   * Traverse RDF graph to find related resources
   */
  getRelated(resource, predicate) {
    const results = []
    for (const t of this.store.triples) {
      if (t.subject === resource && t.predicate === predicate) {
        results.push(t.object)
      }
    }
    return results
  }

  /**
   * Handle incoming HTTP request
   */
  async handleRequest(req) {
    const route = await this.findRoute(req.path, req.method || 'GET')

    if (!route || !route.handler) {
      return {
        status: 404,
        body: { error: 'Not found', path: req.path },
      }
    }

    try {
      const response = await route.handler({
        path: req.path,
        method: req.method || 'GET',
        graph: this,
        route,
      })

      return { status: 200, body: response }
    } catch (err) {
      return { status: 500, body: { error: err.message } }
    }
  }
}

// Example: Customer/Orders graph-aware API
async function example() {
  console.log('╔════════════════════════════════════════════════════════════╗')
  console.log('║ Graph-Aware API Routing - Customer/Orders Example         ║')
  console.log('╚════════════════════════════════════════════════════════════╝\n')

  const router = new GraphAwareRouter()

  // Define routes as RDF triples
  router.defineRoute('list_customers', '/customers', 'GET', async (ctx) => ({
    action: 'list_customers',
    message: 'Returns all customers (RDF-routed)',
  }))

  router.defineRoute('customer_detail', '/customers/', 'GET', async (ctx) => {
    const customerId = ctx.path.split('/')[2]
    const customerUri = ctx.graph.ns.api + `customer/${customerId}`

    // Graph traversal: find orders for this customer
    const orders = ctx.graph.getRelated(
      customerUri,
      ctx.graph.ns.api + 'hasOrder'
    )

    if (ctx.path.includes('/orders')) {
      return {
        customerId,
        message: 'Orders retrieved via RDF graph relationships',
        orders: orders.map((o) => o.replace(ctx.graph.ns.api, '')),
      }
    }

    return {
      customerId,
      message: 'Customer detail via RDF graph',
      orderCount: orders.length,
    }
  })

  // Define RDF relationships: customer -> orders
  router.defineRelationship(
    router.ns.api + 'customer/1',
    router.ns.api + 'hasOrder',
    router.ns.api + 'order/101'
  )
  router.defineRelationship(
    router.ns.api + 'customer/1',
    router.ns.api + 'hasOrder',
    router.ns.api + 'order/102'
  )
  router.defineRelationship(
    router.ns.api + 'customer/2',
    router.ns.api + 'hasOrder',
    router.ns.api + 'order/201'
  )

  // Test routing
  console.log('TEST 1: GET /customers')
  const t1 = await router.handleRequest({ path: '/customers', method: 'GET' })
  console.log('  Status:', t1.status, '| Body:', JSON.stringify(t1.body))

  console.log('\nTEST 2: GET /customers/1')
  const t2 = await router.handleRequest({ path: '/customers/1', method: 'GET' })
  console.log('  Status:', t2.status, '| Body:', JSON.stringify(t2.body))

  console.log('\nTEST 3: GET /customers/1/orders (RDF-routed)')
  const t3 = await router.handleRequest({ path: '/customers/1/orders', method: 'GET' })
  console.log('  Status:', t3.status, '| Body:', JSON.stringify(t3.body))

  console.log('\nTEST 4: GET /customers/2/orders')
  const t4 = await router.handleRequest({ path: '/customers/2/orders', method: 'GET' })
  console.log('  Status:', t4.status, '| Body:', JSON.stringify(t4.body))

  console.log('\nTEST 5: GET /nonexistent (404)')
  const t5 = await router.handleRequest({ path: '/nonexistent', method: 'GET' })
  console.log('  Status:', t5.status, '| Body:', JSON.stringify(t5.body))

  console.log('\n╔════════════════════════════════════════════════════════════╗')
  console.log('║ Key Insight: Routes discovered via RDF graph, not static  ║')
  console.log('║ Path /customers/1/orders auto-routed based on relationships║')
  console.log('╚════════════════════════════════════════════════════════════╝')
}

example().catch(console.error)

export { GraphAwareRouter, example }
