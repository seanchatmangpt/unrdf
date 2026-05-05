/**
 * Mock Store - Simple in-memory RDF store for testing
 * Implements minimal interface compatible with Oxigraph
 */

/**
 * Simple quad implementation
 */
class Quad {
  constructor(subject, predicate, object, graph) {
    this.subject = subject;
    this.predicate = predicate;
    this.object = object;
    this.graph = graph || { termType: 'DefaultGraph', value: '' };
  }

  equals(other) {
    return (
      this.subject.value === other.subject.value &&
      this.predicate.value === other.predicate.value &&
      this.object.value === other.object.value &&
      this.graph.value === other.graph.value
    );
  }
}

/**
 * Simple term factory
 */
export const dataFactory = {
  namedNode(value) {
    return { termType: 'NamedNode', value };
  },

  literal(value, languageOrDatatype) {
    if (typeof languageOrDatatype === 'string') {
      return { termType: 'Literal', value, language: languageOrDatatype };
    } else if (languageOrDatatype && languageOrDatatype.termType === 'NamedNode') {
      return { termType: 'Literal', value, datatype: languageOrDatatype };
    }
    return { termType: 'Literal', value };
  },

  blankNode(value) {
    return { termType: 'BlankNode', value: value || `_:b${Math.random()}` };
  },

  quad(subject, predicate, object, graph) {
    return new Quad(subject, predicate, object, graph);
  },

  defaultGraph() {
    return { termType: 'DefaultGraph', value: '' };
  },
};

/**
 * Simple in-memory store
 */
export class MockStore {
  constructor() {
    this.quads = [];
  }

  add(quad) {
    // Check if quad already exists
    const exists = this.quads.some(q => q.equals(quad));
    if (!exists) {
      this.quads.push(quad);
    }
  }

  delete(quad) {
    this.quads = this.quads.filter(q => !q.equals(quad));
  }

  *match(subject, predicate, object, graph) {
    for (const quad of this.quads) {
      if (subject && quad.subject.value !== subject.value) continue;
      if (predicate && quad.predicate.value !== predicate.value) continue;
      if (object && quad.object.value !== object.value) continue;
      if (graph && quad.graph.value !== graph.value) continue;
      yield quad;
    }
  }

  size() {
    return this.quads.length;
  }
}

/**
 * Create store function compatible with oxigraph
 */
export function createStore() {
  return new MockStore();
}
