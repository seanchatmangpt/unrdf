/**
 * @file TOML Loader Tests
 * @module @unrdf/chatman-equation/test/loader
 */

import { describe, it, expect } from 'vitest';
import { resolve, dirname } from 'path';
import { fileURLToPath } from 'url';
import {
  loadEquationSchema,
  loadExamples,
  extractObservation,
  extractDelta,
  extractClosureOperator,
  extractArtifact,
  validateExample,
  extractUnificationMapping,
  loadAndValidateExamples,
  getAllUnificationMappings,
} from '../src/loader.mjs';

const __dirname = dirname(fileURLToPath(import.meta.url));
const configDir = resolve(__dirname, '../config');

describe('loadEquationSchema', () => {
  it('should load equation schema TOML', () => {
    // Arrange
    const schemaPath = resolve(configDir, 'equation.schema.toml');

    // Act
    const schema = loadEquationSchema(schemaPath);

    // Assert
    expect(schema).toBeDefined();
    expect(schema.metadata).toBeDefined();
    expect(schema.metadata.version).toBe('1.0.0');
    expect(schema.observations).toBeDefined();
    expect(schema.closure_operator).toBeDefined();
    expect(schema.artifacts).toBeDefined();
    expect(schema.unification).toBeDefined();
  });
});

describe('loadExamples', () => {
  it('should load examples TOML', () => {
    // Arrange
    const examplesPath = resolve(configDir, 'examples.toml');

    // Act
    const examples = loadExamples(examplesPath);

    // Assert
    expect(examples).toBeDefined();
    expect(examples.market_equilibrium).toBeDefined();
    expect(examples.org_restructuring).toBeDefined();
    expect(examples.strategy_pivot).toBeDefined();
    expect(examples.blue_ocean_creation).toBeDefined();
    expect(examples.product_feature_release).toBeDefined();
    expect(examples.customer_experience).toBeDefined();
  });
});

describe('extractObservation', () => {
  it('should extract and validate observation from example', () => {
    // Arrange
    const examplesPath = resolve(configDir, 'examples.toml');
    const examples = loadExamples(examplesPath);

    // Act
    const result = extractObservation(examples.market_equilibrium);

    // Assert
    expect(result.success).toBe(true);
    expect(result.data.id).toBe('obs-market-001');
    expect(result.data.domain).toBe('market');
    expect(result.data.state.supply).toBe(10000);
  });
});

describe('extractDelta', () => {
  it('should extract and validate delta from example', () => {
    // Arrange
    const examplesPath = resolve(configDir, 'examples.toml');
    const examples = loadExamples(examplesPath);

    // Act
    const result = extractDelta(examples.market_equilibrium);

    // Assert
    expect(result.success).toBe(true);
    expect(result.data.id).toBe('delta-market-001');
    expect(result.data.domain).toBe('market');
    expect(result.data.operations.length).toBeGreaterThan(0);
  });
});

describe('extractClosureOperator', () => {
  it('should extract and validate closure operator from example', () => {
    // Arrange
    const examplesPath = resolve(configDir, 'examples.toml');
    const examples = loadExamples(examplesPath);

    // Act
    const result = extractClosureOperator(examples.market_equilibrium);

    // Assert
    expect(result.success).toBe(true);
    expect(result.data.type).toBe('reconcile');
    expect(result.data.name).toBe('market_equilibrium');
    expect(result.data.domain).toBe('market');
  });
});

describe('extractArtifact', () => {
  it('should extract and validate artifact from example', () => {
    // Arrange
    const examplesPath = resolve(configDir, 'examples.toml');
    const examples = loadExamples(examplesPath);

    // Act
    const result = extractArtifact(examples.market_equilibrium);

    // Assert
    expect(result.success).toBe(true);
    expect(result.data.id).toBe('artifact-market-001');
    expect(result.data.source_observation).toBe('obs-market-001');
    expect(result.data.operator).toBe('market_equilibrium');
  });
});

describe('validateExample', () => {
  it('should validate complete market equilibrium example', () => {
    // Arrange
    const examplesPath = resolve(configDir, 'examples.toml');
    const examples = loadExamples(examplesPath);

    // Act
    const result = validateExample(examples.market_equilibrium);

    // Assert
    expect(result.success).toBe(true);
    expect(result.errors).toHaveLength(0);
    expect(result.data.observation).toBeDefined();
    expect(result.data.delta).toBeDefined();
    expect(result.data.closure_operator).toBeDefined();
    expect(result.data.expected_artifact).toBeDefined();
  });

  it('should validate blue ocean creation example', () => {
    // Arrange
    const examplesPath = resolve(configDir, 'examples.toml');
    const examples = loadExamples(examplesPath);

    // Act
    const result = validateExample(examples.blue_ocean_creation);

    // Assert
    expect(result.success).toBe(true);
    expect(result.data.observation.domain).toBe('blue_ocean');
  });

  it('should validate all examples', () => {
    // Arrange
    const examplesPath = resolve(configDir, 'examples.toml');
    const examples = loadExamples(examplesPath);
    const exampleNames = [
      'market_equilibrium',
      'org_restructuring',
      'strategy_pivot',
      'blue_ocean_creation',
      'product_feature_release',
      'customer_experience',
    ];

    // Act & Assert
    for (const name of exampleNames) {
      const result = validateExample(examples[name]);
      expect(result.success).toBe(true);
    }
  });
});

describe('extractUnificationMapping', () => {
  it('should extract market unification mapping', () => {
    // Arrange
    const schemaPath = resolve(configDir, 'equation.schema.toml');
    const schema = loadEquationSchema(schemaPath);

    // Act
    const result = extractUnificationMapping(schema, 'market');

    // Assert
    expect(result.success).toBe(true);
    expect(result.data.description).toBe('Market dynamics unification');
    expect(result.data.closure_operator).toBe('market_equilibrium');
    expect(result.data.observation_fields).toContain('supply');
    expect(result.data.observation_fields).toContain('demand');
  });

  it('should extract all domain mappings', () => {
    // Arrange
    const schemaPath = resolve(configDir, 'equation.schema.toml');
    const schema = loadEquationSchema(schemaPath);
    const domains = ['market', 'organization', 'strategy', 'blue_ocean', 'product', 'customer'];

    // Act & Assert
    for (const domain of domains) {
      const result = extractUnificationMapping(schema, domain);
      expect(result.success).toBe(true);
    }
  });
});

describe('loadAndValidateExamples', () => {
  it('should load and validate all examples', () => {
    // Arrange
    const examplesPath = resolve(configDir, 'examples.toml');

    // Act
    const results = loadAndValidateExamples(examplesPath);

    // Assert
    expect(Object.keys(results).length).toBeGreaterThan(0);
    for (const [name, result] of Object.entries(results)) {
      expect(result.success).toBe(true);
    }
  });
});

describe('getAllUnificationMappings', () => {
  it('should get all unification mappings', () => {
    // Arrange
    const schemaPath = resolve(configDir, 'equation.schema.toml');

    // Act
    const mappings = getAllUnificationMappings(schemaPath);

    // Assert
    expect(Object.keys(mappings).length).toBeGreaterThan(0);
    expect(mappings.market).toBeDefined();
    expect(mappings.organization).toBeDefined();
    expect(mappings.strategy).toBeDefined();
    expect(mappings.blue_ocean).toBeDefined();
    expect(mappings.product).toBeDefined();
    expect(mappings.customer).toBeDefined();
  });
});
