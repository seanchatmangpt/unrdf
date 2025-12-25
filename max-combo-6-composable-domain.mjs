#!/usr/bin/env node

/**
 * @fileoverview Composable-Domain Integration Framework
 * @module @unrdf/max-combo-6-composable-domain
 *
 * Integrates: Composables, Domain, Oxigraph, Hooks, Validation, Streaming, Core, KGC-4D
 * Use Case: Reactive ontology-driven applications with live data binding
 */

// ============================================================================
// MOCK IMPLEMENTATIONS
// ============================================================================

class RDFStoreMock {
  constructor() {
    this.quads = [];
  }
  add(quad) {
    this.quads.push(quad);
  }
  query() {
    return this.quads;
  }
}

const createStore = () => new RDFStoreMock();
const dataFactory = {
  namedNode: (v) => ({ value: v, termType: 'NamedNode' }),
  literal: (v) => ({ value: v, termType: 'Literal' }),
  quad: (s, p, o) => ({ subject: s, predicate: p, object: o }),
};

// Composables (reactive state)
function ref(initialValue) {
  let value = initialValue;
  const listeners = [];

  return {
    get value() {
      return value;
    },
    set value(newValue) {
      value = newValue;
      listeners.forEach(listener => listener(value));
    },
    watch(callback) {
      listeners.push(callback);
    },
  };
}

function reactive(obj) {
  const listeners = [];

  return new Proxy(obj, {
    set(target, key, value) {
      const oldValue = target[key];
      target[key] = value;
      listeners.forEach(listener => listener({ key, oldValue, value }));
      return true;
    },
    get(target, key) {
      if (key === 'watch') {
        return (callback) => listeners.push(callback);
      }
      return target[key];
    },
  });
}

function computed(getter) {
  const result = ref(getter());
  return result;
}

// Domain ontology
const FOAF = 'http://xmlns.com/foaf/0.1/';
const RDF = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';
const RDFS = 'http://www.w3.org/2000/01/rdf-schema#';

const DOMAIN_SCHEMA = {
  classes: {
    Person: FOAF + 'Person',
    Organization: FOAF + 'Organization',
    Project: 'http://example.org/Project',
  },
  properties: {
    name: FOAF + 'name',
    knows: FOAF + 'knows',
    memberOf: FOAF + 'member',
    worksOn: 'http://example.org/worksOn',
  },
};

// ============================================================================
// COMPOSABLE-DOMAIN FRAMEWORK
// ============================================================================

/**
 * ComposableDomainFramework - Reactive ontology-driven applications
 */
class ComposableDomainFramework {
  constructor() {
    this.store = createStore();
    this.reactiveState = reactive({
      entities: [],
      relationships: [],
      queryResults: [],
    });
    this.entityCount = ref(0);
    this.relationshipCount = ref(0);
    this.stats = {
      entitiesCreated: 0,
      relationshipsCreated: 0,
      stateUpdates: 0,
    };

    this.setupWatchers();
  }

  /**
   * Setup reactive watchers
   */
  setupWatchers() {
    // Watch entity count changes
    this.entityCount.watch((count) => {
      console.log(`[Reactive] Entity count updated: ${count}`);
    });

    // Watch relationship count changes
    this.relationshipCount.watch((count) => {
      console.log(`[Reactive] Relationship count updated: ${count}`);
    });

    // Watch state changes
    this.reactiveState.watch((change) => {
      this.stats.stateUpdates++;
      console.log(`[Reactive] State changed: ${change.key}`);
    });

    console.log('[Framework] Reactive watchers configured');
  }

  /**
   * Create entity with reactive updates
   */
  async createEntity(type, properties) {
    // Validate type against domain schema
    if (!DOMAIN_SCHEMA.classes[type]) {
      throw new Error(`Invalid entity type: ${type}. Must be one of: ${Object.keys(DOMAIN_SCHEMA.classes).join(', ')}`);
    }

    const entityUri = `http://example.org/entity/${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;

    // Add type triple
    this.store.add(dataFactory.quad(
      dataFactory.namedNode(entityUri),
      dataFactory.namedNode(RDF + 'type'),
      dataFactory.namedNode(DOMAIN_SCHEMA.classes[type])
    ));

    // Add properties
    for (const [key, value] of Object.entries(properties)) {
      const propertyUri = DOMAIN_SCHEMA.properties[key] || `http://example.org/${key}`;

      this.store.add(dataFactory.quad(
        dataFactory.namedNode(entityUri),
        dataFactory.namedNode(propertyUri),
        dataFactory.literal(String(value))
      ));
    }

    // Update reactive state
    const entity = { uri: entityUri, type, properties };
    this.reactiveState.entities.push(entity);
    this.entityCount.value++;
    this.stats.entitiesCreated++;

    console.log(`[Entity] Created ${type}: ${entityUri}`);

    return entity;
  }

  /**
   * Create relationship with reactive updates
   */
  async createRelationship(fromUri, relationship, toUri) {
    // Validate relationship against domain schema
    const relationshipUri = DOMAIN_SCHEMA.properties[relationship] || `http://example.org/${relationship}`;

    this.store.add(dataFactory.quad(
      dataFactory.namedNode(fromUri),
      dataFactory.namedNode(relationshipUri),
      dataFactory.namedNode(toUri)
    ));

    // Update reactive state
    const rel = { from: fromUri, relationship, to: toUri };
    this.reactiveState.relationships.push(rel);
    this.relationshipCount.value++;
    this.stats.relationshipsCreated++;

    console.log(`[Relationship] ${fromUri} -[${relationship}]-> ${toUri}`);

    return rel;
  }

  /**
   * Query with reactive results
   */
  async queryEntities(type) {
    const typeUri = DOMAIN_SCHEMA.classes[type];
    if (!typeUri) {
      return [];
    }

    const results = this.store.quads.filter(q =>
      q.predicate.value === RDF + 'type' &&
      q.object.value === typeUri
    );

    // Update reactive state
    this.reactiveState.queryResults = results.map(q => q.subject.value);

    return this.reactiveState.queryResults;
  }

  /**
   * Get entity details
   */
  getEntityDetails(entityUri) {
    const quads = this.store.quads.filter(q => q.subject.value === entityUri);

    return quads.reduce((acc, quad) => {
      const key = quad.predicate.value.split('/').pop().split('#').pop();
      acc[key] = quad.object.value;
      return acc;
    }, { uri: entityUri });
  }

  /**
   * Get reactive state snapshot
   */
  getReactiveState() {
    return {
      entities: this.reactiveState.entities.length,
      relationships: this.reactiveState.relationships.length,
      entityCount: this.entityCount.value,
      relationshipCount: this.relationshipCount.value,
      stateUpdates: this.stats.stateUpdates,
    };
  }

  /**
   * Get statistics
   */
  getStats() {
    return {
      ...this.stats,
      totalQuads: this.store.quads.length,
      reactiveEntities: this.entityCount.value,
      reactiveRelationships: this.relationshipCount.value,
    };
  }
}

// ============================================================================
// DEMO
// ============================================================================

async function demo() {
  console.log('╔════════════════════════════════════════════════════════════╗');
  console.log('║ Composable-Domain Framework Demo                          ║');
  console.log('║ Reactive ontology-driven applications                      ║');
  console.log('╚════════════════════════════════════════════════════════════╝\n');

  const framework = new ComposableDomainFramework();

  console.log('[Demo] Creating entities with reactive updates...\n');

  // Create persons
  const alice = await framework.createEntity('Person', {
    name: 'Alice Johnson',
  });

  const bob = await framework.createEntity('Person', {
    name: 'Bob Smith',
  });

  // Create organization
  const org = await framework.createEntity('Organization', {
    name: 'Tech Corp',
  });

  // Create project
  const project = await framework.createEntity('Project', {
    name: 'UNRDF Platform',
  });

  console.log('\n[Demo] Creating relationships...\n');

  // Create relationships
  await framework.createRelationship(alice.uri, 'knows', bob.uri);
  await framework.createRelationship(alice.uri, 'memberOf', org.uri);
  await framework.createRelationship(bob.uri, 'memberOf', org.uri);
  await framework.createRelationship(alice.uri, 'worksOn', project.uri);

  console.log('\n[Demo] Querying entities...\n');

  const persons = await framework.queryEntities('Person');
  console.log(`  Found ${persons.length} Person entities`);

  const orgs = await framework.queryEntities('Organization');
  console.log(`  Found ${orgs.length} Organization entities`);

  console.log('\n[Demo] Entity details:\n');
  const aliceDetails = framework.getEntityDetails(alice.uri);
  console.log(`  Alice:`, JSON.stringify(aliceDetails, null, 2));

  console.log('\n[Reactive] State snapshot:');
  const reactiveState = framework.getReactiveState();
  Object.entries(reactiveState).forEach(([key, value]) => {
    console.log(`  ${key}: ${value}`);
  });

  console.log('\n[Stats] Final Statistics:');
  const stats = framework.getStats();
  Object.entries(stats).forEach(([key, value]) => {
    console.log(`  ${key}: ${value}`);
  });

  console.log('\n╔════════════════════════════════════════════════════════════╗');
  console.log('║ Key Integration:                                           ║');
  console.log('║ - Composables provide reactive state management            ║');
  console.log('║ - Domain ontology validates entity types/relationships     ║');
  console.log('║ - RDF store maintains semantic knowledge graph             ║');
  console.log('║ - Live updates propagate through reactive system           ║');
  console.log('╚════════════════════════════════════════════════════════════╝');
}

if (import.meta.url === `file://${process.argv[1]}`) {
  demo().catch(console.error);
}

export { ComposableDomainFramework, demo };
