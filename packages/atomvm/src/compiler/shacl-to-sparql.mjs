/**
 * @file SHACL to SPARQL Compiler
 * @module atomvm/compiler/shacl-to-sparql
 */

/**
 * Compiler that transforms SHACL shapes into SPARQL queries
 */
export class SHACLToSPARQLCompiler {
  constructor(prefixes = {}) {
    this.prefixes = {
      sh: 'http://www.w3.org/ns/shacl#',
      rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
      xsd: 'http://www.w3.org/2001/XMLSchema#',
      ...prefixes
    };
  }

  /**
   * Transpile a SHACL shape to a SPARQL ASK query
   * @param {Object} shape - SHACL shape definition
   * @returns {string} SPARQL ASK query
   */
  compileToASK(shape) {
    const { targetClass, targetNode, properties, or, and, not } = shape;
    
    let patterns = [];
    let filters = [];

    // Target selection
    if (targetClass) {
      patterns.push(`?focusNode a <${targetClass}>`);
    } else if (targetNode) {
      patterns.push(`BIND(<${targetNode}> AS ?focusNode)`);
    } else {
      patterns.push(`?focusNode ?p ?o`); // Default to all nodes if no target
    }

    // Property constraints
    if (properties) {
      properties.forEach((prop, i) => {
        const { path, datatype, minCount, maxCount, minLength, maxLength, pattern } = prop;
        const valVar = `?val${i}`;
        
        // We use OPTIONAL because we want to check constraints even if the property is missing (for minCount > 0)
        // Actually, for simple ASK, we can just match.
        // If minCount > 0, it must exist.
        if (minCount > 0) {
          patterns.push(`?focusNode <${path}> ${valVar}`);
        } else {
          patterns.push(`OPTIONAL { ?focusNode <${path}> ${valVar} }`);
        }

        if (datatype) {
          filters.push(`DATATYPE(${valVar}) = <${datatype}>`);
        }
        if (minLength) {
          filters.push(`STRLEN(STR(${valVar})) >= ${minLength}`);
        }
        if (maxLength) {
          filters.push(`STRLEN(STR(${valVar})) <= ${maxLength}`);
        }
        if (pattern) {
          filters.push(`REGEX(STR(${valVar}), "${pattern}")`);
        }
      });
    }

    // Logical operators (simplified)
    if (or) {
      // SHACL or is complex to map to SPARQL perfectly in one query, but for POC:
      const orClauses = or.map(s => this.compileToASK(s).match(/WHERE \{(.+)\}/s)[1]);
      patterns.push(`{ ${orClauses.join(' } UNION { ')} }`);
    }

    const where = patterns.join(' . ');
    const filterStr = filters.length > 0 ? ` FILTER(${filters.join(' && ')})` : '';

    return `ASK WHERE { ${where}${filterStr} }`;
  }
}

export default SHACLToSPARQLCompiler;
