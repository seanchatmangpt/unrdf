/**
 * @file Ontology Learner
 * @module @unrdf/hooks/ontology-learner
 * @description Learn SHACL shapes from RDF patterns
 */

/**
 * Learn ontology patterns from RDF data
 */
export class OntologyLearner {
  /**
   * Infer SHACL shapes from RDF graph
   */
  async inferShapes(store, options = {}) {
    const minSupport = options.minSupport || 0.9;
    const shapes = {};

    if (!store || store.size === 0) {
      return shapes;
    }

    // Get all classes
    const classes = this.extractClasses(store);

    for (const cls of classes) {
      // Get instances of this class
      const instances = this.getInstancesOfClass(store, cls);
      if (instances.length === 0) continue;

      // Mine properties
      const properties = this.mineProperties(store, instances);

      // Generate shapes for properties with enough support
      const shapeProperties = {};
      for (const [predicate, data] of Object.entries(properties)) {
        const supportRatio = data.count / instances.length;
        if (supportRatio >= minSupport) {
          shapeProperties[predicate] = {
            minCount: supportRatio > 0.99 ? 1 : 0,
            datatype: this.dominantDatatype(data.datatypes),
            description: `Property ${predicate} (${Math.round(supportRatio * 100)}% coverage)`,
          };
        }
      }

      if (Object.keys(shapeProperties).length > 0) {
        shapes[cls] = {
          targetClass: cls,
          properties: shapeProperties,
          instanceCount: instances.length,
        };
      }
    }

    return shapes;
  }

  /**
   * Extract all RDF classes from store
   */
  extractClasses(store) {
    const classes = new Set();

    for (const quad of store) {
      // RDF type declarations
      if (quad.predicate.value === 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type') {
        if (quad.object.value && quad.object.value.startsWith('http')) {
          classes.add(quad.object.value);
        }
      }
      // Also RDFS classes
      if (quad.subject.value && quad.subject.value.startsWith('http') &&
          quad.object.value === 'http://www.w3.org/2000/01/rdf-schema#Class') {
        classes.add(quad.subject.value);
      }
    }

    return Array.from(classes);
  }

  /**
   * Get instances of a class
   */
  getInstancesOfClass(store, className) {
    const instances = new Set();
    const typeIri = 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type';

    for (const quad of store) {
      if (quad.predicate.value === typeIri &&
          quad.object.value === className) {
        instances.add(quad.subject.value);
      }
    }

    return Array.from(instances);
  }

  /**
   * Mine property patterns from instances
   */
  mineProperties(store, instances) {
    const properties = {};

    for (const instance of instances) {
      for (const quad of store) {
        if (quad.subject.value === instance) {
          const pred = quad.predicate.value;
          const obj = quad.object.value;

          if (!properties[pred]) {
            properties[pred] = {
              count: 0,
              objects: [],
              datatypes: new Set(),
            };
          }

          properties[pred].count++;
          properties[pred].objects.push(obj);
          properties[pred].datatypes.add(this.inferDatatype(quad.object));
        }
      }
    }

    return properties;
  }

  /**
   * Infer datatype of RDF value
   */
  inferDatatype(obj) {
    if (!obj) return 'unknown';

    // Check for NamedNode/Resource
    if (obj.termType === 'NamedNode' || (obj.value && String(obj.value).startsWith('http'))) {
      return 'rdf:Resource';
    }

    // Infer from value pattern - try specific types first
    if (obj.value !== undefined && obj.value !== null) {
      const val = String(obj.value);

      // Try to infer more specific types from pattern
      if (/^\d+$/.test(val)) return 'xsd:integer';
      if (/^\d+\.\d+$/.test(val)) return 'xsd:float';
      if (/^(true|false)$/i.test(val)) return 'xsd:boolean';
      if (/^\d{4}-\d{2}-\d{2}/.test(val)) return 'xsd:dateTime';
    }

    // Fall back to explicit datatype
    if (obj.datatype && obj.datatype.value) {
      const dt = obj.datatype.value;
      if (dt.includes('integer') || dt.includes('int')) return 'xsd:integer';
      if (dt.includes('float') || dt.includes('double')) return 'xsd:float';
      if (dt.includes('boolean')) return 'xsd:boolean';
      if (dt.includes('date')) return 'xsd:dateTime';
      if (dt.includes('string')) return 'xsd:string';
    }

    return 'xsd:string';
  }

  /**
   * Find dominant datatype from set
   */
  dominantDatatype(datatypes) {
    if (datatypes.size === 0) return 'xsd:string';
    return Array.from(datatypes)[0];
  }

  /**
   * Generate SHACL shape definition
   */
  toSHACLShape(className, properties) {
    return {
      '@context': 'http://www.w3.org/ns/shacl#',
      '@type': 'NodeShape',
      targetClass: className,
      property: Object.entries(properties).map(([pred, config]) => ({
        path: pred,
        minCount: config.minCount || 0,
        datatype: config.datatype || 'xsd:string',
      })),
    };
  }
}

export default OntologyLearner;
