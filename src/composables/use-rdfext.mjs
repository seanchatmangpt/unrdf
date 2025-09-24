/**
 * @fileoverview useRDFExt composable - Advanced RDF operations with rdf-ext
 * 
 * This composable provides advanced RDF operations using the rdf-ext library
 * for dataset management, graph operations, and extended RDF functionality.
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import rdf from "rdf-ext";
import { useStoreContext } from "../context/index.mjs";

/**
 * Create an RDFExt composable for advanced RDF operations
 * 
 * @param {Object} [options] - RDFExt options
 * @param {string} [options.baseIRI] - Base IRI for operations
 * @param {boolean} [options.strict=true] - Enable strict operations
 * @returns {Object} RDFExt interface
 * 
 * @example
 * // Initialize store context first
 * const runApp = initStore();
 * 
 * runApp(() => {
 *   const rdfExt = useRDFExt();
 * 
 *   // Create dataset
 *   const dataset = rdfExt.createDataset();
 * 
 *   // Create namespace
 *   const ns = rdfExt.createNamespace('http://example.org/');
 * 
 *   // Advanced graph operations
 *   const graph = rdfExt.createGraph();
 *   const result = rdfExt.union(graph1, graph2);
 * });
 * 
 * @throws {Error} If store context is not initialized
 */
export function useRDFExt(options = {}) {
  // Get the engine from context
  const storeContext = useStoreContext();
  const engine = storeContext.engine;
  
  const { 
    baseIRI = engine.baseIRI || "http://example.org/",
    strict = true 
  } = options;

  return {
    /**
     * The underlying RDF engine
     * @type {RdfEngine}
     */
    get engine() {
      return engine;
    },

    /**
     * The underlying store
     * @type {Store}
     */
    get store() {
      return storeContext.store;
    },

    /**
     * Create a new RDF dataset
     * @param {Array} [quads] - Initial quads
     * @returns {Dataset} RDF dataset
     * 
     * @example
     * const dataset = rdfExt.createDataset();
     * dataset.add(quad(subject, predicate, object));
     */
    createDataset(quads = []) {
      try {
        return rdf.dataset(quads);
      } catch (error) {
        if (strict) {
          throw new Error(`Dataset creation failed: ${error.message}`);
        }
        return rdf.dataset();
      }
    },

    /**
     * Create a new RDF graph
     * @param {Array} [quads] - Initial quads
     * @returns {Graph} RDF graph
     * 
     * @example
     * const graph = rdfExt.createGraph();
     * graph.add(quad(subject, predicate, object));
     */
    createGraph(quads = []) {
      try {
        return rdf.graph(quads);
      } catch (error) {
        if (strict) {
          throw new Error(`Graph creation failed: ${error.message}`);
        }
        return rdf.graph();
      }
    },

    /**
     * Create a namespace for IRIs
     * @param {string} baseIRI - Base IRI for the namespace
     * @returns {Object} Namespace object
     * 
     * @example
     * const ns = rdfExt.createNamespace('http://example.org/');
     * const person = ns('person'); // http://example.org/person
     */
    createNamespace(baseIRI) {
      try {
        return rdf.namespace(baseIRI);
      } catch (error) {
        if (strict) {
          throw new Error(`Namespace creation failed: ${error.message}`);
        }
        return (localName) => rdf.namedNode(baseIRI + localName);
      }
    },

    /**
     * Create RDF terms using rdf-ext factory
     * @returns {Object} Term factory
     * 
     * @example
     * const factory = rdfExt.createFactory();
     * const node = factory.namedNode('http://example.org/');
     * const literal = factory.literal('Hello');
     */
    createFactory() {
      return {
        namedNode: (iri) => rdf.namedNode(iri),
        literal: (value, datatype, language) => rdf.literal(value, datatype, language),
        blankNode: (id) => rdf.blankNode(id),
        defaultGraph: () => rdf.defaultGraph(),
        variable: (name) => rdf.variable(name),
        quad: (subject, predicate, object, graph) => rdf.quad(subject, predicate, object, graph)
      };
    },

    /**
     * Convert N3 store to rdf-ext dataset
     * @param {Store} [store] - Store to convert (uses context store if not provided)
     * @returns {Dataset} rdf-ext dataset
     * 
     * @example
     * const dataset = rdfExt.storeToDataset(store);
     */
    storeToDataset(store) {
      const targetStore = store || storeContext.store;
      const quads = [];
      
      for (const quad of targetStore) {
        quads.push(rdf.quad(
          rdf.namedNode(quad.subject.value),
          rdf.namedNode(quad.predicate.value),
          quad.object.termType === 'NamedNode' ? rdf.namedNode(quad.object.value) :
          quad.object.termType === 'Literal' ? rdf.literal(quad.object.value, quad.object.datatype?.value, quad.object.language) :
          rdf.blankNode(quad.object.value),
          quad.graph ? rdf.namedNode(quad.graph.value) : rdf.defaultGraph()
        ));
      }
      
      return rdf.dataset(quads);
    },

    /**
     * Convert rdf-ext dataset to N3 store
     * @param {Dataset} dataset - rdf-ext dataset
     * @returns {Store} N3 store
     * 
     * @example
     * const store = rdfExt.datasetToStore(dataset);
     */
    datasetToStore(dataset) {
      const { Store, DataFactory } = await import('n3');
      const store = new Store();
      
      for (const quad of dataset) {
        store.addQuad(DataFactory.quad(
          DataFactory.namedNode(quad.subject.value),
          DataFactory.namedNode(quad.predicate.value),
          quad.object.termType === 'NamedNode' ? DataFactory.namedNode(quad.object.value) :
          quad.object.termType === 'Literal' ? DataFactory.literal(quad.object.value, quad.object.datatype?.value, quad.object.language) :
          DataFactory.blankNode(quad.object.value),
          quad.graph ? DataFactory.namedNode(quad.graph.value) : DataFactory.defaultGraph()
        ));
      }
      
      return store;
    },

    /**
     * Union multiple datasets
     * @param {...Dataset} datasets - Datasets to union
     * @returns {Dataset} Union dataset
     * 
     * @example
     * const union = rdfExt.union(dataset1, dataset2, dataset3);
     */
    union(...datasets) {
      try {
        if (datasets.length === 0) {
          return rdf.dataset();
        }
        
        let result = datasets[0];
        for (let i = 1; i < datasets.length; i++) {
          result = result.union(datasets[i]);
        }
        
        return result;
      } catch (error) {
        if (strict) {
          throw new Error(`Dataset union failed: ${error.message}`);
        }
        return rdf.dataset();
      }
    },

    /**
     * Intersection of multiple datasets
     * @param {...Dataset} datasets - Datasets to intersect
     * @returns {Dataset} Intersection dataset
     * 
     * @example
     * const intersection = rdfExt.intersection(dataset1, dataset2);
     */
    intersection(...datasets) {
      try {
        if (datasets.length === 0) {
          return rdf.dataset();
        }
        
        let result = datasets[0];
        for (let i = 1; i < datasets.length; i++) {
          result = result.intersection(datasets[i]);
        }
        
        return result;
      } catch (error) {
        if (strict) {
          throw new Error(`Dataset intersection failed: ${error.message}`);
        }
        return rdf.dataset();
      }
    },

    /**
     * Difference between datasets
     * @param {Dataset} dataset1 - First dataset
     * @param {Dataset} dataset2 - Second dataset
     * @returns {Dataset} Difference dataset
     * 
     * @example
     * const difference = rdfExt.difference(dataset1, dataset2);
     */
    difference(dataset1, dataset2) {
      try {
        return dataset1.difference(dataset2);
      } catch (error) {
        if (strict) {
          throw new Error(`Dataset difference failed: ${error.message}`);
        }
        return rdf.dataset();
      }
    },

    /**
     * Check if datasets are equal
     * @param {Dataset} dataset1 - First dataset
     * @param {Dataset} dataset2 - Second dataset
     * @returns {boolean} True if datasets are equal
     * 
     * @example
     * const equal = rdfExt.equals(dataset1, dataset2);
     */
    equals(dataset1, dataset2) {
      try {
        return dataset1.equals(dataset2);
      } catch (error) {
        if (strict) {
          throw new Error(`Dataset equality check failed: ${error.message}`);
        }
        return false;
      }
    },

    /**
     * Check if one dataset is a subset of another
     * @param {Dataset} subset - Potential subset
     * @param {Dataset} superset - Potential superset
     * @returns {boolean} True if subset is contained in superset
     * 
     * @example
     * const isSubset = rdfExt.isSubset(subset, superset);
     */
    isSubset(subset, superset) {
      try {
        return subset.isSubset(superset);
      } catch (error) {
        if (strict) {
          throw new Error(`Subset check failed: ${error.message}`);
        }
        return false;
      }
    },

    /**
     * Filter dataset by pattern
     * @param {Dataset} dataset - Dataset to filter
     * @param {Object} pattern - Filter pattern
     * @param {Term} [pattern.subject] - Subject pattern
     * @param {Term} [pattern.predicate] - Predicate pattern
     * @param {Term} [pattern.object] - Object pattern
     * @param {Term} [pattern.graph] - Graph pattern
     * @returns {Dataset} Filtered dataset
     * 
     * @example
     * const filtered = rdfExt.filter(dataset, {
     *   predicate: rdf.namedNode('http://xmlns.com/foaf/0.1/name')
     * });
     */
    filter(dataset, pattern) {
      try {
        return dataset.match(
          pattern.subject,
          pattern.predicate,
          pattern.object,
          pattern.graph
        );
      } catch (error) {
        if (strict) {
          throw new Error(`Dataset filtering failed: ${error.message}`);
        }
        return rdf.dataset();
      }
    },

    /**
     * Get all subjects in dataset
     * @param {Dataset} dataset - Dataset to analyze
     * @returns {Array<Term>} Array of subjects
     * 
     * @example
     * const subjects = rdfExt.getSubjects(dataset);
     */
    getSubjects(dataset) {
      try {
        return [...dataset.subjects()];
      } catch (error) {
        if (strict) {
          throw new Error(`Subject extraction failed: ${error.message}`);
        }
        return [];
      }
    },

    /**
     * Get all predicates in dataset
     * @param {Dataset} dataset - Dataset to analyze
     * @returns {Array<Term>} Array of predicates
     * 
     * @example
     * const predicates = rdfExt.getPredicates(dataset);
     */
    getPredicates(dataset) {
      try {
        return [...dataset.predicates()];
      } catch (error) {
        if (strict) {
          throw new Error(`Predicate extraction failed: ${error.message}`);
        }
        return [];
      }
    },

    /**
     * Get all objects in dataset
     * @param {Dataset} dataset - Dataset to analyze
     * @returns {Array<Term>} Array of objects
     * 
     * @example
     * const objects = rdfExt.getObjects(dataset);
     */
    getObjects(dataset) {
      try {
        return [...dataset.objects()];
      } catch (error) {
        if (strict) {
          throw new Error(`Object extraction failed: ${error.message}`);
        }
        return [];
      }
    },

    /**
     * Get all graphs in dataset
     * @param {Dataset} dataset - Dataset to analyze
     * @returns {Array<Term>} Array of graphs
     * 
     * @example
     * const graphs = rdfExt.getGraphs(dataset);
     */
    getGraphs(dataset) {
      try {
        return [...dataset.graphs()];
      } catch (error) {
        if (strict) {
          throw new Error(`Graph extraction failed: ${error.message}`);
        }
        return [];
      }
    },

    /**
     * Get statistics about dataset
     * @param {Dataset} dataset - Dataset to analyze
     * @returns {Object} Dataset statistics
     * 
     * @example
     * const stats = rdfExt.getStats(dataset);
     * console.log(`Quads: ${stats.quads}, Subjects: ${stats.subjects}`);
     */
    getStats(dataset) {
      try {
        return {
          quads: dataset.size,
          subjects: dataset.subjects().size,
          predicates: dataset.predicates().size,
          objects: dataset.objects().size,
          graphs: dataset.graphs().size,
          namedNodes: [...dataset.subjects()].filter(s => s.termType === 'NamedNode').length,
          literals: [...dataset.objects()].filter(o => o.termType === 'Literal').length,
          blankNodes: [...dataset.subjects()].filter(s => s.termType === 'BlankNode').length
        };
      } catch (error) {
        if (strict) {
          throw new Error(`Statistics calculation failed: ${error.message}`);
        }
        return {
          quads: 0,
          subjects: 0,
          predicates: 0,
          objects: 0,
          graphs: 0,
          namedNodes: 0,
          literals: 0,
          blankNodes: 0
        };
      }
    },

    /**
     * Clone a dataset
     * @param {Dataset} dataset - Dataset to clone
     * @returns {Dataset} Cloned dataset
     * 
     * @example
     * const cloned = rdfExt.clone(dataset);
     */
    clone(dataset) {
      try {
        return rdf.dataset([...dataset]);
      } catch (error) {
        if (strict) {
          throw new Error(`Dataset cloning failed: ${error.message}`);
        }
        return rdf.dataset();
      }
    },

    /**
     * Serialize dataset to format
     * @param {Dataset} dataset - Dataset to serialize
     * @param {string} format - Output format (nquads, turtle, etc.)
     * @param {Object} [options] - Serialization options
     * @returns {Promise<string>} Serialized string
     * 
     * @example
     * const nquads = await rdfExt.serialize(dataset, 'nquads');
     */
    async serialize(dataset, format, options = {}) {
      try {
        const serializer = rdf.serializers[format];
        if (!serializer) {
          throw new Error(`Unsupported format: ${format}`);
        }
        
        return await serializer.serialize(dataset, options);
      } catch (error) {
        if (strict) {
          throw new Error(`Dataset serialization failed: ${error.message}`);
        }
        return '';
      }
    },

    /**
     * Parse string to dataset
     * @param {string} input - Input string
     * @param {string} format - Input format (nquads, turtle, etc.)
     * @param {Object} [options] - Parsing options
     * @returns {Promise<Dataset>} Parsed dataset
     * 
     * @example
     * const dataset = await rdfExt.parse(nquadsString, 'nquads');
     */
    async parse(input, format, options = {}) {
      try {
        const parser = rdf.parsers[format];
        if (!parser) {
          throw new Error(`Unsupported format: ${format}`);
        }
        
        return await parser.parse(input, {
          base: baseIRI,
          ...options
        });
      } catch (error) {
        if (strict) {
          throw new Error(`Dataset parsing failed: ${error.message}`);
        }
        return rdf.dataset();
      }
    }
  };
}
