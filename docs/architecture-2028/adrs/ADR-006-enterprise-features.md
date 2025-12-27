# ADR-006: Enterprise Features Layer

**Status:** Proposed
**Date:** 2025-11-18
**Deciders:** System Architecture Team
**Technical Story:** Enable enterprise-grade multi-tenancy, governance, data lineage, and integration middleware

## Context and Problem Statement

Enterprise deployments require multi-tenant isolation, governance policies, data lineage tracking, and integration with existing enterprise systems. How do we add these capabilities while maintaining the simplicity and performance of the core system?

## Decision Drivers

- **Multi-Tenancy**: Complete isolation between tenants (data, resources, performance)
- **Governance**: Policy-driven data management and compliance
- **Lineage**: Complete data provenance and impact analysis
- **Integration**: Connect with existing enterprise systems (ETL, ESB, APIs)
- **Scalability**: Support 1000+ concurrent tenants
- **Compliance**: SOC 2, ISO 27001, industry-specific regulations
- **Operational Excellence**: Monitoring, alerting, SLA management

## Considered Options

### Option 1: Schema-Based Multi-Tenancy
- **Pros**: Simple, database-native, good isolation
- **Cons**: Limited scalability, migration complexity

### Option 2: Database-Per-Tenant
- **Pros**: Complete isolation, independent scaling
- **Cons**: Operational overhead, resource waste

### Option 3: Hybrid Architecture (Recommended)
- **Pros**: Flexibility, efficient resource use, strong isolation
- **Cons**: Complex implementation

## Decision Outcome

**Chosen option:** Option 3 - Hybrid Architecture

Implement a flexible multi-tenancy architecture with:
1. Shared infrastructure with logical isolation
2. Graph-based tenant separation in RDF stores
3. Policy-driven governance engine
4. Comprehensive lineage tracking
5. Enterprise integration middleware

### Architecture Design

#### Component Structure

```
src/enterprise/
├── core/
│   ├── enterprise-manager.mjs      # Main orchestrator
│   ├── tenant-context.mjs          # Tenant context management
│   ├── sla-manager.mjs             # SLA monitoring
│   └── schemas.mjs
│
├── multi-tenant/
│   ├── tenant-isolator.mjs         # Tenant isolation
│   ├── tenant-router.mjs           # Request routing
│   ├── resource-quotas.mjs         # Resource limits & metering
│   ├── tenant-lifecycle.mjs        # Tenant provisioning/deprovisioning
│   ├── tenant-config.mjs           # Tenant configuration
│   └── isolation-strategies/
│       ├── graph-isolation.mjs     # Graph-based isolation
│       ├── store-isolation.mjs     # Store-based isolation
│       └── schema-isolation.mjs    # Schema-based isolation
│
├── governance/
│   ├── policy-engine.mjs           # Governance policy engine
│   ├── data-catalog.mjs            # Metadata catalog
│   ├── compliance-checker.mjs      # Compliance validation
│   ├── quality-monitor.mjs         # Data quality monitoring
│   ├── glossary-manager.mjs        # Business glossary
│   ├── policies/
│   │   ├── retention-policy.mjs    # Data retention
│   │   ├── classification-policy.mjs # Data classification
│   │   ├── masking-policy.mjs      # Data masking
│   │   └── quality-rules.mjs       # Quality rules
│   └── index.mjs
│
├── lineage/
│   ├── lineage-tracker.mjs         # Lineage tracking
│   ├── provenance-graph.mjs        # Provenance graph builder
│   ├── impact-analyzer.mjs         # Impact analysis
│   ├── dependency-graph.mjs        # Dependency tracking
│   ├── change-tracker.mjs          # Change tracking
│   └── index.mjs
│
├── integration/
│   ├── middleware-adapter.mjs      # Enterprise middleware
│   ├── etl-pipeline.mjs            # ETL pipeline
│   ├── api-gateway.mjs             # API gateway
│   ├── connectors/
│   │   ├── rest-connector.mjs      # REST API connector
│   │   ├── graphql-connector.mjs   # GraphQL connector
│   │   ├── kafka-connector.mjs     # Kafka connector
│   │   ├── database-connector.mjs  # Database connector
│   │   └── file-connector.mjs      # File system connector
│   └── index.mjs
│
├── monitoring/
│   ├── metrics-collector.mjs       # Enterprise metrics
│   ├── alert-manager.mjs           # Alerting
│   ├── dashboard-generator.mjs     # Dashboard generation
│   └── sla-reporter.mjs            # SLA reporting
│
└── index.mjs
```

#### Core Interfaces

```javascript
/**
 * @typedef {Object} EnterpriseConfig
 * @property {Object} multiTenant - Multi-tenancy configuration
 * @property {Object} governance - Governance configuration
 * @property {Object} lineage - Lineage configuration
 * @property {Object} integration - Integration configuration
 */

/**
 * Enterprise Features Manager
 */
export class EnterpriseManager {
  constructor(config = {}) {
    this.config = {
      multiTenant: {
        isolation: config.multiTenant?.isolation || 'graph',
        quotas: config.multiTenant?.quotas || {},
        ...config.multiTenant
      },
      governance: {
        policies: config.governance?.policies || [],
        ...config.governance
      },
      lineage: {
        enabled: config.lineage?.enabled !== false,
        granularity: config.lineage?.granularity || 'quad',
        ...config.lineage
      },
      integration: {
        connectors: config.integration?.connectors || [],
        ...config.integration
      }
    };

    this.tenantIsolator = new TenantIsolator(this.config.multiTenant);
    this.policyEngine = new PolicyEngine(this.config.governance);
    this.lineageTracker = new LineageTracker(this.config.lineage);
    this.apiGateway = new APIGateway(this.config.integration);
  }

  /**
   * Create new tenant
   * @param {Object} tenantConfig - Tenant configuration
   * @returns {Promise<Object>} Tenant details
   */
  async createTenant(tenantConfig) {
    const span = tracer.startSpan('enterprise.tenant.create');

    try {
      span.setAttribute('tenant.id', tenantConfig.id);
      span.setAttribute('isolation.strategy', this.config.multiTenant.isolation);

      // Provision tenant resources
      const tenant = await this.tenantIsolator.provision(tenantConfig);

      // Apply default policies
      await this.policyEngine.applyDefaultPolicies(tenant.id);

      // Initialize lineage tracking
      if (this.config.lineage.enabled) {
        await this.lineageTracker.initialize(tenant.id);
      }

      span.setStatus({ code: SpanStatusCode.OK });
      return tenant;

    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw error;
    } finally {
      span.end();
    }
  }

  /**
   * Execute governed operation
   * @param {string} tenantId - Tenant ID
   * @param {Function} operation - Operation to execute
   * @param {Object} context - Operation context
   * @returns {Promise<any>} Operation result
   */
  async executeGoverned(tenantId, operation, context = {}) {
    const span = tracer.startSpan('enterprise.execute_governed');

    try {
      span.setAttribute('tenant.id', tenantId);

      // Set tenant context
      const tenantCtx = await this.tenantIsolator.getContext(tenantId);

      // Check quotas
      await this._checkQuotas(tenantCtx);

      // Evaluate policies
      const policyResult = await this.policyEngine.evaluate(tenantCtx, context);

      if (!policyResult.allowed) {
        throw new Error(`Policy violation: ${policyResult.reason}`);
      }

      // Track lineage (before)
      const lineageId = await this.lineageTracker.startOperation(
        tenantId,
        context
      );

      // Execute operation
      const result = await operation(tenantCtx);

      // Track lineage (after)
      await this.lineageTracker.completeOperation(lineageId, result);

      span.setStatus({ code: SpanStatusCode.OK });
      return result;

    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw error;
    } finally {
      span.end();
    }
  }

  /**
   * Track data lineage
   * @param {string} tenantId - Tenant ID
   * @param {Object} operation - Operation metadata
   * @returns {Promise<string>} Lineage ID
   */
  async trackLineage(tenantId, operation) {
    return await this.lineageTracker.track(tenantId, operation);
  }

  /**
   * Get impact analysis
   * @param {string} tenantId - Tenant ID
   * @param {string} resourceId - Resource to analyze
   * @returns {Promise<Object>} Impact analysis
   */
  async analyzeImpact(tenantId, resourceId) {
    const span = tracer.startSpan('enterprise.lineage.impact_analysis');

    try {
      const impact = await this.lineageTracker.analyzeImpact(
        tenantId,
        resourceId
      );

      span.setAttribute('impact.downstream', impact.downstream.length);
      span.setAttribute('impact.upstream', impact.upstream.length);
      span.setStatus({ code: SpanStatusCode.OK });

      return impact;

    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw error;
    } finally {
      span.end();
    }
  }
}
```

#### Tenant Isolation Implementation

```javascript
/**
 * Multi-tenant isolation manager
 */
export class TenantIsolator {
  constructor(config) {
    this.config = config;
    this.tenants = new Map();
    this.quotaManager = new ResourceQuotaManager(config.quotas);
  }

  /**
   * Provision new tenant
   * @param {Object} tenantConfig - Tenant configuration
   * @returns {Promise<Object>} Tenant details
   */
  async provision(tenantConfig) {
    const span = tracer.startSpan('enterprise.tenant.provision');

    try {
      const tenantId = tenantConfig.id || crypto.randomUUID();

      span.setAttribute('tenant.id', tenantId);

      // Create isolated resources based on strategy
      const resources = await this._createIsolatedResources(
        tenantId,
        tenantConfig
      );

      // Set up quotas
      const quotas = await this.quotaManager.initialize(tenantId, {
        maxTriples: tenantConfig.quotas?.maxTriples || 1000000,
        maxQueries: tenantConfig.quotas?.maxQueries || 10000,
        maxStorage: tenantConfig.quotas?.maxStorage || 1024 * 1024 * 1024, // 1GB
        ...tenantConfig.quotas
      });

      const tenant = {
        id: tenantId,
        name: tenantConfig.name,
        resources,
        quotas,
        createdAt: Date.now(),
        status: 'active'
      };

      this.tenants.set(tenantId, tenant);

      span.setStatus({ code: SpanStatusCode.OK });
      return tenant;

    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw error;
    } finally {
      span.end();
    }
  }

  /**
   * Get tenant context
   * @param {string} tenantId - Tenant ID
   * @returns {Promise<Object>} Tenant context
   */
  async getContext(tenantId) {
    const tenant = this.tenants.get(tenantId);

    if (!tenant) {
      throw new Error(`Tenant not found: ${tenantId}`);
    }

    return {
      tenant,
      store: tenant.resources.store,
      quotas: tenant.quotas,
      metadata: tenant.metadata || {}
    };
  }

  async _createIsolatedResources(tenantId, config) {
    switch (this.config.isolation) {
      case 'graph':
        return await this._createGraphIsolation(tenantId, config);
      case 'store':
        return await this._createStoreIsolation(tenantId, config);
      case 'schema':
        return await this._createSchemaIsolation(tenantId, config);
      default:
        throw new Error(`Unknown isolation strategy: ${this.config.isolation}`);
    }
  }

  async _createGraphIsolation(tenantId, config) {
    // Create shared store with tenant-specific graph
    const store = new Store();
    const tenantGraph = DataFactory.namedNode(
      `urn:unrdf:tenant:${tenantId}:graph`
    );

    return {
      store,
      graph: tenantGraph,
      isolationType: 'graph'
    };
  }

  async _createStoreIsolation(tenantId, config) {
    // Create dedicated store per tenant
    const store = new Store();

    return {
      store,
      isolationType: 'store'
    };
  }
}
```

#### Policy Engine Implementation

```javascript
/**
 * Governance policy engine
 */
export class PolicyEngine {
  constructor(config) {
    this.config = config;
    this.policies = this._loadPolicies(config.policies);
    this.dataCatalog = new DataCatalog();
  }

  /**
   * Evaluate policies
   * @param {Object} context - Execution context
   * @param {Object} operation - Operation metadata
   * @returns {Promise<Object>} Policy result
   */
  async evaluate(context, operation) {
    const span = tracer.startSpan('enterprise.governance.evaluate');

    try {
      // Get applicable policies
      const applicablePolicies = this.policies.filter((policy) =>
        this._isPolicyApplicable(policy, context, operation)
      );

      span.setAttribute('policies.applicable', applicablePolicies.length);

      // Evaluate each policy
      const results = [];

      for (const policy of applicablePolicies) {
        const result = await this._evaluatePolicy(policy, context, operation);
        results.push(result);

        if (!result.allowed) {
          // Policy violation - fail fast
          span.setAttribute('policy.violated', policy.id);
          return result;
        }
      }

      span.setStatus({ code: SpanStatusCode.OK });
      return {
        allowed: true,
        policies: results.map((r) => r.policyId)
      };

    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw error;
    } finally {
      span.end();
    }
  }

  /**
   * Apply data classification
   * @param {Quad} quad - RDF quad
   * @returns {Promise<string>} Classification level
   */
  async classify(quad) {
    const span = tracer.startSpan('enterprise.governance.classify');

    try {
      // Check predicate against classification rules
      const classification = this._determineClassification(quad);

      // Update catalog
      await this.dataCatalog.updateClassification(
        quad.subject.value,
        classification
      );

      span.setAttribute('classification', classification);
      span.setStatus({ code: SpanStatusCode.OK });

      return classification;

    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw error;
    } finally {
      span.end();
    }
  }

  _determineClassification(quad) {
    // Sensitive predicates
    const sensitive = [
      'http://example.org/ssn',
      'http://example.org/creditCard',
      'http://xmlns.com/foaf/0.1/mbox'
    ];

    if (sensitive.includes(quad.predicate.value)) {
      return 'confidential';
    }

    // Public predicates
    const publicPredicates = [
      'http://www.w3.org/2000/01/rdf-schema#label',
      'http://purl.org/dc/terms/title'
    ];

    if (publicPredicates.includes(quad.predicate.value)) {
      return 'public';
    }

    // Default: internal
    return 'internal';
  }
}
```

#### Lineage Tracker Implementation

```javascript
/**
 * Data lineage tracker
 */
export class LineageTracker {
  constructor(config) {
    this.config = config;
    this.provenanceGraph = new ProvenanceGraph();
    this.operationLog = [];
  }

  /**
   * Start tracking operation
   * @param {string} tenantId - Tenant ID
   * @param {Object} context - Operation context
   * @returns {Promise<string>} Lineage ID
   */
  async startOperation(tenantId, context) {
    const lineageId = crypto.randomUUID();

    const operation = {
      id: lineageId,
      tenantId,
      type: context.type,
      inputs: context.inputs || [],
      startTime: Date.now(),
      status: 'in_progress'
    };

    this.operationLog.push(operation);

    return lineageId;
  }

  /**
   * Complete operation tracking
   * @param {string} lineageId - Lineage ID
   * @param {any} result - Operation result
   * @returns {Promise<void>}
   */
  async completeOperation(lineageId, result) {
    const span = tracer.startSpan('enterprise.lineage.complete');

    try {
      const operation = this.operationLog.find((op) => op.id === lineageId);

      if (!operation) {
        throw new Error(`Operation not found: ${lineageId}`);
      }

      operation.endTime = Date.now();
      operation.duration = operation.endTime - operation.startTime;
      operation.outputs = this._extractOutputs(result);
      operation.status = 'completed';

      // Build provenance graph
      await this.provenanceGraph.addOperation(operation);

      span.setStatus({ code: SpanStatusCode.OK });

    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw error;
    } finally {
      span.end();
    }
  }

  /**
   * Analyze impact of changes
   * @param {string} tenantId - Tenant ID
   * @param {string} resourceId - Resource ID
   * @returns {Promise<Object>} Impact analysis
   */
  async analyzeImpact(tenantId, resourceId) {
    const span = tracer.startSpan('enterprise.lineage.impact');

    try {
      // Find all operations affecting this resource
      const downstream = await this.provenanceGraph.getDownstream(resourceId);
      const upstream = await this.provenanceGraph.getUpstream(resourceId);

      const impact = {
        resource: resourceId,
        downstream,  // Resources that depend on this
        upstream,    // Resources this depends on
        operations: downstream.length + upstream.length
      };

      span.setAttribute('impact.total', impact.operations);
      span.setStatus({ code: SpanStatusCode.OK });

      return impact;

    } catch (error) {
      span.recordException(error);
      span.setStatus({ code: SpanStatusCode.ERROR });
      throw error;
    } finally {
      span.end();
    }
  }

  _extractOutputs(result) {
    // Extract resource identifiers from result
    if (Array.isArray(result)) {
      return result.map((item) => item.id || item.subject?.value);
    }

    return [result.id || result.subject?.value];
  }
}
```

### Integration with Knowledge Hooks

```javascript
// Enterprise hooks
defineHook('enterprise.tenant.created', {
  phase: 'post',
  effect: async ({ tenant }) => {
    // Initialize tenant resources
    await logger.info(`Tenant created: ${tenant.id}`);
  }
});

defineHook('enterprise.policy.violated', {
  phase: 'pre',
  condition: async ({ policyResult }) => !policyResult.allowed,
  effect: async ({ policyResult }) => {
    // Log policy violation
    await auditLogger.log({
      type: 'policy_violation',
      policy: policyResult.policy,
      reason: policyResult.reason
    });

    throw new Error(`Policy violation: ${policyResult.reason}`);
  }
});

defineHook('enterprise.lineage.tracked', {
  phase: 'post',
  effect: async ({ operation }) => {
    // Update lineage dashboard
    await dashboard.update('lineage', operation);
  }
});
```

### Usage Examples

```javascript
// Example 1: Multi-tenant setup
import { EnterpriseManager } from 'unrdf/enterprise';

const enterprise = new EnterpriseManager({
  multiTenant: {
    isolation: 'graph',
    quotas: {
      maxTriples: 10000000,
      maxQueries: 100000
    }
  }
});

// Create tenant
const tenant = await enterprise.createTenant({
  id: 'acme-corp',
  name: 'ACME Corporation',
  quotas: {
    maxTriples: 5000000
  }
});

// Example 2: Governed operations
await enterprise.executeGoverned('acme-corp', async (context) => {
  // Add data within governance framework
  context.store.add(DataFactory.quad(...));
}, {
  type: 'data.add',
  classification: 'internal'
});

// Example 3: Data lineage tracking
const lineageId = await enterprise.trackLineage('acme-corp', {
  type: 'etl.import',
  source: 's3://data-lake/dataset.ttl',
  inputs: ['s3://data-lake/dataset.ttl'],
  outputs: ['urn:graph:imported-data']
});

// Example 4: Impact analysis
const impact = await enterprise.analyzeImpact(
  'acme-corp',
  'urn:dataset:customer-data'
);

console.log(`Downstream dependencies: ${impact.downstream.length}`);
console.log(`Upstream dependencies: ${impact.upstream.length}`);
```

## Consequences

### Positive

- **Enterprise Ready**: Multi-tenancy, governance, lineage
- **Compliance**: Policy-driven data management
- **Visibility**: Complete data lineage and impact analysis
- **Integration**: Connect with existing enterprise systems
- **Scalability**: Support large enterprise deployments

### Negative

- **Complexity**: Significantly more complex than single-tenant
- **Performance**: Governance checks add overhead
- **Operational**: More complex deployment and management

## Performance Targets

| Operation | Target Overhead |
|-----------|----------------|
| Tenant Isolation | <5% |
| Policy Evaluation | <10ms |
| Lineage Tracking | <5ms per operation |
| Impact Analysis | <500ms |

## References

- [Multi-Tenancy Patterns](https://docs.microsoft.com/en-us/azure/architecture/patterns/multi-tenancy)
- [Data Governance Best Practices](https://www.gartner.com/en/information-technology/glossary/data-governance)
- [Data Lineage](https://en.wikipedia.org/wiki/Data_lineage)
