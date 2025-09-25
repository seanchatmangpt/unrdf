/**
 * @fileoverview Definitive Ingress/Egress Adapters
 * 
 * Comprehensive adapters for converting between external formats and RDF.
 * Integrates with event system for real-time validation and monitoring.
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { DataFactory } from "n3";
import { z } from "zod";

const { namedNode, literal, quad } = DataFactory;

/**
 * @typedef {(schema:any, input:any, opts?:object)=>Promise<{data:any, rdf:import('n3').Quad[], provenance:any}>} IngressFn
 * @typedef {(schema:any, data:any, opts?:object)=>Promise<{output:string, provenance:any}>} EgressFn
 */

/**
 * Ingress adapters for converting external formats to RDF
 */
export const ingress = {
  /**
   * JSON → Zod → RDF
   * @param {z.ZodSchema} schema - Zod schema for validation
   * @param {string} jsonStr - JSON string input
   * @param {Object} opts - Options
   * @param {string} [opts.base='http://ex/'] - Base URI for predicates
   * @param {string} [opts.subject='urn:ingress:item'] - Subject URI
   * @param {RdfEngine} [opts.engine] - RDF engine for event emission
   * @returns {Promise<{data:any, rdf:import('n3').Quad[], provenance:any}>}
   */
  async fromJSON(schema, jsonStr, { base = 'http://ex/', subject = 'urn:ingress:item', engine } = {}) {
    // Emit before ingress event
    if (engine) {
      await engine.eventBus.emit('beforeIngress', {
        event: 'beforeIngress',
        quad: null,
        context: engine.eventBus.createContext('ingress', { format: 'json', subject }),
        store: engine.store,
        engine
      });
    }

    const obj = schema.parse(JSON.parse(jsonStr));
    const quads = Object.entries(obj).map(([k, v]) => {
      // Handle arrays and objects by storing as JSON strings
      const value = Array.isArray(v) || (typeof v === 'object' && v !== null) 
        ? JSON.stringify(v) 
        : String(v);
      return quad(namedNode(subject), namedNode(base + k), literal(value));
    });

    const result = { 
      data: obj, 
      rdf: quads, 
      provenance: prov('json') 
    };

    // Emit after ingress event
    if (engine) {
      await engine.eventBus.emit('afterIngress', {
        event: 'afterIngress',
        quad: quads,
        context: engine.eventBus.createContext('ingress', { format: 'json', subject, quadCount: quads.length }),
        store: engine.store,
        engine
      });
    }

    return result;
  },

  /**
   * Turtle → quads (direct)
   * @param {z.ZodSchema} schema - Schema (unused for Turtle)
   * @param {string} ttl - Turtle string
   * @param {Object} opts - Options
   * @param {RdfEngine} opts.engine - RDF engine for parsing
   * @returns {Promise<{data:any, rdf:import('n3').Quad[], provenance:any}>}
   */
  async fromTurtle(schema, ttl, { engine }) {
    // Emit before ingress event
    await engine.eventBus.emit('beforeIngress', {
      event: 'beforeIngress',
      quad: null,
      context: engine.eventBus.createContext('ingress', { format: 'turtle' }),
      store: engine.store,
      engine
    });

    const store = engine.parseTurtle(ttl);
    const quads = store.getQuads(null, null, null, null);

    const result = { 
      data: null, 
      rdf: quads, 
      provenance: prov('turtle') 
    };

    // Emit after ingress event
    await engine.eventBus.emit('afterIngress', {
      event: 'afterIngress',
      quad: quads,
      context: engine.eventBus.createContext('ingress', { format: 'turtle', quadCount: quads.length }),
      store: engine.store,
      engine
    });

    return result;
  },

  /**
   * CSV → RDF (basic implementation)
   * @param {z.ZodSchema} schema - Zod schema for validation
   * @param {string} csvStr - CSV string input
   * @param {Object} opts - Options
   * @param {string} [opts.base='http://ex/'] - Base URI for predicates
   * @param {string} [opts.subjectPrefix='urn:csv:row'] - Subject URI prefix
   * @param {RdfEngine} [opts.engine] - RDF engine for event emission
   * @returns {Promise<{data:any, rdf:import('n3').Quad[], provenance:any}>}
   */
  async fromCSV(schema, csvStr, { base = 'http://ex/', subjectPrefix = 'urn:csv:row', engine } = {}) {
    // Emit before ingress event
    if (engine) {
      await engine.eventBus.emit('beforeIngress', {
        event: 'beforeIngress',
        quad: null,
        context: engine.eventBus.createContext('ingress', { format: 'csv' }),
        store: engine.store,
        engine
      });
    }

    const lines = csvStr.trim().split('\n');
    const headers = lines[0].split(',').map(h => h.trim());
    const rows = lines.slice(1);
    
    const allQuads = [];
    const data = [];

    for (let i = 0; i < rows.length; i++) {
      const values = rows[i].split(',').map(v => v.trim());
      const rowData = {};
      const subject = `${subjectPrefix}:${i}`;
      
      for (let j = 0; j < headers.length; j++) {
        const header = headers[j];
        const value = values[j] || '';
        rowData[header] = value;
        
        allQuads.push(quad(
          namedNode(subject),
          namedNode(base + header),
          literal(value)
        ));
      }
      
      data.push(rowData);
    }

    const result = { 
      data, 
      rdf: allQuads, 
      provenance: prov('csv') 
    };

    // Emit after ingress event
    if (engine) {
      await engine.eventBus.emit('afterIngress', {
        event: 'afterIngress',
        quad: allQuads,
        context: engine.eventBus.createContext('ingress', { format: 'csv', quadCount: allQuads.length }),
        store: engine.store,
        engine
      });
    }

    return result;
  }
};

/**
 * Egress adapters for converting RDF to external formats
 */
export const egress = {
  /**
   * RDF → JSON by projecting predicates into keys
   * @param {z.ZodSchema} schema - Zod schema for validation
   * @param {import('n3').Store} store - RDF store
   * @param {Object} opts - Options
   * @param {string} opts.subject - Subject URI to project
   * @param {RdfEngine} [opts.engine] - RDF engine for event emission
   * @returns {Promise<{output:string, provenance:any}>}
   */
  async toJSON(schema, store, { subject, engine } = {}) {
    // Emit before egress event
    if (engine) {
      await engine.eventBus.emit('beforeEgress', {
        event: 'beforeEgress',
        quad: null,
        context: engine.eventBus.createContext('egress', { format: 'json', subject }),
        store,
        engine
      });
    }

    const quads = store.getQuads(namedNode(subject), null, null, null);
    const obj = {};
    
    for (const q of quads) {
      // Extract the property name from the predicate URI
      const predicateUri = q.predicate.value;
      const propertyName = predicateUri.split('/').pop() || predicateUri.split('#').pop() || 'unknown';
      
      // Handle different data types
      let value = q.object.value;
      if (q.object.termType === 'Literal') {
        // Try to parse as JSON if it looks like it
        if (value.startsWith('[') || value.startsWith('{')) {
          try {
            value = JSON.parse(value);
          } catch (e) {
            // Keep as string if parsing fails
          }
        }
      }
      
      obj[propertyName] = value;
    }
    
    const data = schema.parse(obj); // enforce schema on the way out
    const output = JSON.stringify(data, null, 2);
    
    const result = { 
      output, 
      provenance: prov('json') 
    };

    // Emit after egress event
    if (engine) {
      await engine.eventBus.emit('afterEgress', {
        event: 'afterEgress',
        quad: quads,
        context: engine.eventBus.createContext('egress', { format: 'json', subject, outputLength: output.length }),
        store,
        engine
      });
    }

    return result;
  },

  /**
   * RDF → Turtle
   * @param {z.ZodSchema} schema - Schema (unused for Turtle)
   * @param {import('n3').Store} store - RDF store
   * @param {RdfEngine} engine - RDF engine for serialization
   * @returns {Promise<{output:string, provenance:any}>}
   */
  async toTurtle(schema, store, engine) {
    // Emit before egress event
    await engine.eventBus.emit('beforeEgress', {
      event: 'beforeEgress',
      quad: null,
      context: engine.eventBus.createContext('egress', { format: 'turtle' }),
      store,
      engine
    });

    const out = await engine.serializeTurtle(store);
    
    const result = { 
      output: out, 
      provenance: prov('turtle') 
    };

    // Emit after egress event
    await engine.eventBus.emit('afterEgress', {
      event: 'afterEgress',
      quad: store.getQuads(null, null, null, null),
      context: engine.eventBus.createContext('egress', { format: 'turtle', outputLength: out.length }),
      store,
      engine
    });

    return result;
  },

  /**
   * RDF → CSV (basic implementation)
   * @param {z.ZodSchema} schema - Schema for validation
   * @param {import('n3').Store} store - RDF store
   * @param {Object} opts - Options
   * @param {string} [opts.subjectPattern] - Subject pattern to filter
   * @param {RdfEngine} [opts.engine] - RDF engine for event emission
   * @returns {Promise<{output:string, provenance:any}>}
   */
  async toCSV(schema, store, { subjectPattern, engine } = {}) {
    // Emit before egress event
    if (engine) {
      await engine.eventBus.emit('beforeEgress', {
        event: 'beforeEgress',
        quad: null,
        context: engine.eventBus.createContext('egress', { format: 'csv', subjectPattern }),
        store,
        engine
      });
    }

    // Get all subjects
    const subjects = new Set();
    for (const quad of store) {
      if (!subjectPattern || quad.subject.value.includes(subjectPattern)) {
        subjects.add(quad.subject.value);
      }
    }

    // Get all predicates
    const predicates = new Set();
    for (const quad of store) {
      if (subjects.has(quad.subject.value)) {
        predicates.add(quad.predicate.value);
      }
    }

    const predicateArray = Array.from(predicates);
    const header = predicateArray.map(p => p.split('/').pop() || p.split('#').pop() || 'unknown').join(',');
    
    const rows = [];
    for (const subject of subjects) {
      const row = [];
      for (const predicate of predicateArray) {
        const quads = store.getQuads(namedNode(subject), namedNode(predicate), null, null);
        const value = quads.length > 0 ? quads[0].object.value : '';
        row.push(value);
      }
      rows.push(row.join(','));
    }

    const output = [header, ...rows].join('\n');
    
    const result = { 
      output, 
      provenance: prov('csv') 
    };

    // Emit after egress event
    if (engine) {
      await engine.eventBus.emit('afterEgress', {
        event: 'afterEgress',
        quad: Array.from(store),
        context: engine.eventBus.createContext('egress', { format: 'csv', subjectPattern, outputLength: output.length }),
        store,
        engine
      });
    }

    return result;
  }
};

/**
 * Create provenance metadata
 * @param {string} format - Format name
 * @returns {Object} Provenance object
 */
function prov(format) {
  return { 
    format, 
    ts: new Date().toISOString() 
  };
}
