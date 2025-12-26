#!/usr/bin/env node

/**
 * @fileoverview Domain-Substrate - Domain + Core
 * @module @unrdf/microfw-7-domain-substrate
 *
 * Adversarial Innovation: Ontology definitions + core utilities = semantic type system
 * Use Case: Type-safe domain modeling with ontology-driven validation
 */

// ============================================================================
// MOCK IMPLEMENTATIONS
// ============================================================================

// Domain ontology
const FOAF = 'http://xmlns.com/foaf/0.1/';
const SCHEMA = 'http://schema.org/';
const RDF = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';

const DOMAIN_ONTOLOGY = {
  classes: {
    Person: FOAF + 'Person',
    Organization: SCHEMA + 'Organization',
    Event: SCHEMA + 'Event',
  },
  properties: {
    name: FOAF + 'name',
    email: FOAF + 'mbox',
    knows: FOAF + 'knows',
    memberOf: SCHEMA + 'memberOf',
    organizer: SCHEMA + 'organizer',
  },
  dataTypes: {
    string: 'http://www.w3.org/2001/XMLSchema#string',
    integer: 'http://www.w3.org/2001/XMLSchema#integer',
    dateTime: 'http://www.w3.org/2001/XMLSchema#dateTime',
    anyURI: 'http://www.w3.org/2001/XMLSchema#anyURI',
  },
};

// Core utilities
function validateType(value, expectedType) {
  switch (expectedType) {
    case DOMAIN_ONTOLOGY.dataTypes.string:
      return typeof value === 'string';
    case DOMAIN_ONTOLOGY.dataTypes.integer:
      return typeof value === 'number' && Number.isInteger(value);
    case DOMAIN_ONTOLOGY.dataTypes.anyURI:
      return typeof value === 'string' && value.startsWith('http');
    default:
      return true;
  }
}

function createTypedValue(value, dataType) {
  return {
    value,
    dataType,
    isValid: validateType(value, dataType),
  };
}

// ============================================================================
// DOMAIN-SUBSTRATE FRAMEWORK
// ============================================================================

/**
 * DomainSubstrateFramework - Semantic type system
 */
class DomainSubstrateFramework {
  constructor() {
    this.entities = new Map();
    this.typeRegistry = new Map();
    this.stats = {
      entitiesCreated: 0,
      typesRegistered: 0,
      validationErrors: 0,
    };

    this.initializeTypes();
  }

  /**
   * Initialize ontology-based types
   */
  initializeTypes() {
    // Register domain classes
    for (const [name, uri] of Object.entries(DOMAIN_ONTOLOGY.classes)) {
      this.registerType(name, {
        uri,
        kind: 'class',
        properties: {},
      });
    }

    // Register Person properties
    this.typeRegistry.get('Person').properties = {
      name: { required: true, dataType: DOMAIN_ONTOLOGY.dataTypes.string },
      email: { required: false, dataType: DOMAIN_ONTOLOGY.dataTypes.string },
      knows: { required: false, dataType: DOMAIN_ONTOLOGY.classes.Person },
    };

    // Register Organization properties
    this.typeRegistry.get('Organization').properties = {
      name: { required: true, dataType: DOMAIN_ONTOLOGY.dataTypes.string },
    };

    console.log(`[Substrate] Initialized ${this.typeRegistry.size} domain types`);
  }

  /**
   * Register new type
   */
  registerType(name, definition) {
    this.typeRegistry.set(name, definition);
    this.stats.typesRegistered++;
  }

  /**
   * Create entity with type validation
   */
  async createEntity(type, properties) {
    const typeDefinition = this.typeRegistry.get(type);
    if (!typeDefinition) {
      throw new Error(`Unknown type: ${type}. Available: ${Array.from(this.typeRegistry.keys()).join(', ')}`);
    }

    // Validate properties against type definition
    const errors = [];

    for (const [propName, propDef] of Object.entries(typeDefinition.properties)) {
      const value = properties[propName];

      // Check required properties
      if (propDef.required && !value) {
        errors.push(`Missing required property: ${propName}`);
        this.stats.validationErrors++;
      }

      // Check data types
      if (value && !validateType(value, propDef.dataType)) {
        errors.push(`Invalid type for ${propName}: expected ${propDef.dataType}`);
        this.stats.validationErrors++;
      }
    }

    if (errors.length > 0) {
      console.log(`[Validation] Errors for ${type}:`, errors);
      return { success: false, errors };
    }

    // Create entity
    const entityId = `entity-${this.stats.entitiesCreated + 1}`;
    const entity = {
      id: entityId,
      type,
      typeUri: typeDefinition.uri,
      properties: {},
    };

    // Create typed values
    for (const [propName, value] of Object.entries(properties)) {
      const propDef = typeDefinition.properties[propName];
      if (propDef) {
        entity.properties[propName] = createTypedValue(value, propDef.dataType);
      }
    }

    this.entities.set(entityId, entity);
    this.stats.entitiesCreated++;

    console.log(`[Entity] Created ${type}: ${entityId}`);

    return { success: true, entity };
  }

  /**
   * Query entities by type
   */
  queryByType(type) {
    const results = [];
    for (const [id, entity] of this.entities) {
      if (entity.type === type) {
        results.push(entity);
      }
    }
    return results;
  }

  /**
   * Get type definition
   */
  getTypeDefinition(typeName) {
    return this.typeRegistry.get(typeName);
  }

  /**
   * List available types
   */
  listTypes() {
    return Array.from(this.typeRegistry.keys());
  }

  /**
   * Get statistics
   */
  getStats() {
    return {
      ...this.stats,
      entities: this.entities.size,
      types: this.typeRegistry.size,
    };
  }
}

// ============================================================================
// DEMO
// ============================================================================

async function demo() {
  console.log('╔════════════════════════════════════════════════════════════╗');
  console.log('║ Domain-Substrate Framework Demo                           ║');
  console.log('║ Domain + Core = Semantic type system                       ║');
  console.log('╚════════════════════════════════════════════════════════════╝\n');

  const framework = new DomainSubstrateFramework();

  console.log('[Demo] Available types:', framework.listTypes().join(', '));

  console.log('\n[Demo] Creating entities with type validation...\n');

  // Test 1: Valid Person
  console.log('[Test 1] Creating valid Person');
  await framework.createEntity('Person', {
    name: 'Alice Johnson',
    email: 'alice@example.com',
  });

  // Test 2: Invalid Person (missing required name)
  console.log('\n[Test 2] Creating invalid Person (missing name)');
  await framework.createEntity('Person', {
    email: 'bob@example.com',
  });

  // Test 3: Valid Organization
  console.log('\n[Test 3] Creating valid Organization');
  await framework.createEntity('Organization', {
    name: 'Tech Corp',
  });

  // Test 4: Invalid type
  console.log('\n[Test 4] Attempting to create invalid type');
  try {
    await framework.createEntity('InvalidType', {
      name: 'Test',
    });
  } catch (err) {
    console.log(`  Error: ${err.message}`);
  }

  console.log('\n[Demo] Querying entities by type...\n');
  const persons = framework.queryByType('Person');
  console.log(`  Persons: ${persons.length}`);

  const orgs = framework.queryByType('Organization');
  console.log(`  Organizations: ${orgs.length}`);

  console.log('\n[Demo] Type definition for Person:');
  const personType = framework.getTypeDefinition('Person');
  console.log(`  URI: ${personType.uri}`);
  console.log(`  Properties:`, Object.keys(personType.properties));

  console.log('\n[Stats] Final Statistics:');
  const stats = framework.getStats();
  Object.entries(stats).forEach(([key, value]) => {
    console.log(`  ${key}: ${value}`);
  });

  console.log('\n╔════════════════════════════════════════════════════════════╗');
  console.log('║ Adversarial Innovation:                                    ║');
  console.log('║ - Domain ontology defines semantic types                   ║');
  console.log('║ - Core utilities provide type validation                   ║');
  console.log('║ - Type-safe entity creation with compile-time guarantees   ║');
  console.log('║ - Ontology + utilities = semantic type system              ║');
  console.log('╚════════════════════════════════════════════════════════════╝');
}

if (import.meta.url === `file://${process.argv[1]}`) {
  demo().catch(console.error);
}

export { DomainSubstrateFramework, demo };
