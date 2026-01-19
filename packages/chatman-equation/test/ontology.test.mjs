/**
 * @file Chatman Equation Ontology Tests
 * @description Tests for loading and validating the Chatman Equation ontology
 */

import { describe, it, expect } from 'vitest';
import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';
import { Parser, Store } from 'n3';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

const ONTOLOGY_DIR = join(__dirname, '..', 'ontology');

/**
 * Load Turtle file into N3 Store
 * @param {string} filename - Turtle file name
 * @returns {Promise<Store>} N3 Store with loaded triples
 */
async function loadTurtle(filename) {
  const path = join(ONTOLOGY_DIR, filename);
  const content = readFileSync(path, 'utf-8');
  const parser = new Parser({ format: 'text/turtle' });
  const store = new Store();

  return new Promise((resolve, reject) => {
    parser.parse(content, (error, quad, prefixes) => {
      if (error) {
        reject(error);
      } else if (quad) {
        store.addQuad(quad);
      } else {
        resolve(store);
      }
    });
  });
}

describe('Chatman Equation Ontology', () => {
  describe('chatman.ttl - Main Ontology', () => {
    let store;

    it('should load without errors', async () => {
      store = await loadTurtle('chatman.ttl');
      expect(store.size).toBeGreaterThan(0);
    });

    it('should define the ChatmanEquationOntology', async () => {
      store = await loadTurtle('chatman.ttl');
      const ontology = store.getQuads(
        'urn:chatman:ChatmanEquationOntology',
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        'http://www.w3.org/2002/07/owl#Ontology'
      );
      expect(ontology.length).toBe(1);
    });

    it('should define core classes: Artifact, Observation, ClosureOperator', async () => {
      store = await loadTurtle('chatman.ttl');

      const artifact = store.getQuads(
        'urn:chatman:equation:Artifact',
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        'http://www.w3.org/2000/01/rdf-schema#Class'
      );
      expect(artifact.length).toBeGreaterThan(0);

      const observation = store.getQuads(
        'urn:chatman:equation:Observation',
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        'http://www.w3.org/2000/01/rdf-schema#Class'
      );
      expect(observation.length).toBeGreaterThan(0);

      const closureOperator = store.getQuads(
        'urn:chatman:equation:ClosureOperator',
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        'http://www.w3.org/2000/01/rdf-schema#Class'
      );
      expect(closureOperator.length).toBeGreaterThan(0);
    });

    it('should define closure operator types', async () => {
      store = await loadTurtle('chatman.ttl');

      const operatorTypes = [
        'urn:chatman:equation:UnificationOperator',
        'urn:chatman:equation:DisruptionOperator',
        'urn:chatman:equation:BlueOceanOperator',
        'urn:chatman:equation:StrategicPivotOperator'
      ];

      for (const type of operatorTypes) {
        const quads = store.getQuads(
          type,
          'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          'http://www.w3.org/2000/01/rdf-schema#Class'
        );
        expect(quads.length).toBeGreaterThan(0);
      }
    });

    it('should define unification domains', async () => {
      store = await loadTurtle('chatman.ttl');

      const domains = [
        'urn:chatman:equation:MarketDomain',
        'urn:chatman:equation:OrganizationalDomain',
        'urn:chatman:equation:InnovationDomain',
        'urn:chatman:equation:ProductDomain',
        'urn:chatman:equation:FinanceDomain',
        'urn:chatman:equation:TechnologyDomain'
      ];

      for (const domain of domains) {
        const quads = store.getQuads(
          domain,
          'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          'urn:chatman:equation:UnificationDomain'
        );
        expect(quads.length).toBeGreaterThan(0);
      }
    });

    it('should define core properties', async () => {
      store = await loadTurtle('chatman.ttl');

      const properties = [
        'urn:chatman:equation:unifies',
        'urn:chatman:equation:transforms',
        'urn:chatman:equation:observes',
        'urn:chatman:equation:appliesTo',
        'urn:chatman:equation:produces',
        'urn:chatman:equation:consumes'
      ];

      for (const property of properties) {
        const quads = store.getQuads(
          property,
          'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
          'http://www.w3.org/1999/02/22-rdf-syntax-ns#Property'
        );
        expect(quads.length).toBeGreaterThan(0);
      }
    });

    it('should include lineage information', async () => {
      store = await loadTurtle('chatman.ttl');

      const jamesChatman = store.getQuads(
        'urn:chatman:lineage:JamesIChatman',
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        null
      );
      expect(jamesChatman.length).toBeGreaterThan(0);

      const seanChatman = store.getQuads(
        'urn:chatman:lineage:SeanChatman',
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        null
      );
      expect(seanChatman.length).toBeGreaterThan(0);
    });

    it('should define standard operator instances', async () => {
      store = await loadTurtle('chatman.ttl');

      const standardOperators = [
        'urn:chatman:equation:MarketConsolidation',
        'urn:chatman:equation:BlueOceanStrategy',
        'urn:chatman:equation:DisruptiveInnovation',
        'urn:chatman:equation:StrategicPivot'
      ];

      for (const operator of standardOperators) {
        const quads = store.getQuads(operator, null, null);
        expect(quads.length).toBeGreaterThan(0);
      }
    });
  });

  describe('examples.ttl - Concrete Examples', () => {
    let store;

    it('should load without errors', async () => {
      store = await loadTurtle('examples.ttl');
      expect(store.size).toBeGreaterThan(0);
    });

    it('should include Netflix streaming market example', async () => {
      store = await loadTurtle('examples.ttl');

      const netflix = store.getQuads(
        'urn:chatman:examples:Netflix2015',
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        'urn:chatman:equation:Artifact'
      );
      expect(netflix.length).toBeGreaterThan(0);
    });

    it('should include Cirque du Soleil blue ocean example', async () => {
      store = await loadTurtle('examples.ttl');

      const cirque = store.getQuads(
        'urn:chatman:examples:CirqueDuSoleil',
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        'urn:chatman:equation:Artifact'
      );
      expect(cirque.length).toBeGreaterThan(0);

      const operator = store.getQuads(
        'urn:chatman:examples:CirqueCreation',
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        'urn:chatman:equation:BlueOceanOperator'
      );
      expect(operator.length).toBeGreaterThan(0);
    });

    it('should include iPhone disruption example', async () => {
      store = await loadTurtle('examples.ttl');

      const iphone = store.getQuads(
        'urn:chatman:examples:iPhone2007',
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        'urn:chatman:equation:Artifact'
      );
      expect(iphone.length).toBeGreaterThan(0);

      const disruption = store.getQuads(
        'urn:chatman:examples:iPhoneDisruption',
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        'urn:chatman:equation:DisruptionOperator'
      );
      expect(disruption.length).toBeGreaterThan(0);
    });

    it('should include Netflix pivot example', async () => {
      store = await loadTurtle('examples.ttl');

      const pivot = store.getQuads(
        'urn:chatman:examples:NetflixStreamingPivot',
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        'urn:chatman:equation:StrategicPivotOperator'
      );
      expect(pivot.length).toBeGreaterThan(0);
    });

    it('should include observations with measurements', async () => {
      store = await loadTurtle('examples.ttl');

      const observations = store.getQuads(
        null,
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        'urn:chatman:equation:Observation'
      );
      expect(observations.length).toBeGreaterThan(10);
    });

    it('should link artifacts via derivation', async () => {
      store = await loadTurtle('examples.ttl');

      const derivations = store.getQuads(
        null,
        'urn:chatman:equation:derivedFrom',
        null
      );
      expect(derivations.length).toBeGreaterThan(0);
    });
  });

  describe('shapes.ttl - SHACL Validation', () => {
    let store;

    it('should load without errors', async () => {
      store = await loadTurtle('shapes.ttl');
      expect(store.size).toBeGreaterThan(0);
    });

    it('should define ArtifactShape', async () => {
      store = await loadTurtle('shapes.ttl');

      const shape = store.getQuads(
        'urn:chatman:shapes:ArtifactShape',
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        'http://www.w3.org/ns/shacl#NodeShape'
      );
      expect(shape.length).toBeGreaterThan(0);
    });

    it('should define ObservationShape', async () => {
      store = await loadTurtle('shapes.ttl');

      const shape = store.getQuads(
        'urn:chatman:shapes:ObservationShape',
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        'http://www.w3.org/ns/shacl#NodeShape'
      );
      expect(shape.length).toBeGreaterThan(0);
    });

    it('should define ClosureOperatorShape', async () => {
      store = await loadTurtle('shapes.ttl');

      const shape = store.getQuads(
        'urn:chatman:shapes:ClosureOperatorShape',
        'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',
        'http://www.w3.org/ns/shacl#NodeShape'
      );
      expect(shape.length).toBeGreaterThan(0);
    });

    it('should include property constraints', async () => {
      store = await loadTurtle('shapes.ttl');

      const constraints = store.getQuads(
        null,
        'http://www.w3.org/ns/shacl#property',
        null
      );
      expect(constraints.length).toBeGreaterThan(10);
    });

    it('should include SPARQL-based validation rules', async () => {
      store = await loadTurtle('shapes.ttl');

      const sparqlRules = store.getQuads(
        null,
        'http://www.w3.org/ns/shacl#sparql',
        null
      );
      expect(sparqlRules.length).toBeGreaterThan(0);
    });
  });

  describe('Integration - Load All Files Together', () => {
    it('should load all ontology files without conflicts', async () => {
      const [ontology, examples, shapes] = await Promise.all([
        loadTurtle('chatman.ttl'),
        loadTurtle('examples.ttl'),
        loadTurtle('shapes.ttl')
      ]);

      const combinedStore = new Store();
      ontology.forEach(quad => combinedStore.addQuad(quad));
      examples.forEach(quad => combinedStore.addQuad(quad));
      shapes.forEach(quad => combinedStore.addQuad(quad));

      expect(combinedStore.size).toBeGreaterThan(
        ontology.size + examples.size + shapes.size - 100
      );
    });

    it('should have consistent namespace usage', async () => {
      const [ontology, examples, shapes] = await Promise.all([
        loadTurtle('chatman.ttl'),
        loadTurtle('examples.ttl'),
        loadTurtle('shapes.ttl')
      ]);

      const namespaces = [
        'urn:chatman:',
        'urn:chatman:equation:',
        'urn:chatman:lineage:',
        'urn:chatman:examples:',
        'urn:chatman:shapes:'
      ];

      for (const ns of namespaces) {
        const inOntology = ontology.getQuads(null, null, null).some(
          quad => quad.subject.value.startsWith(ns)
        );
        expect(inOntology || ns === 'urn:chatman:examples:' || ns === 'urn:chatman:shapes:').toBe(true);
      }
    });
  });
});
