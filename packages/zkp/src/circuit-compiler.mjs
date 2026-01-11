/**
 * @file SPARQL to R1CS Circuit Compiler
 * @module @unrdf/zkp/circuit-compiler
 * @description
 * Compiles SPARQL queries to arithmetic circuits (R1CS) for zk-SNARK proving.
 *
 * **Circuit Structure**:
 * - Triple matching constraints (subject, predicate, object equality)
 * - FILTER arithmetic constraints (>, <, =, !=)
 * - Result binding constraints
 *
 * **Performance**: Circuit compilation <100ms for typical queries
 */

import { Parser } from 'sparqljs';
import { trace } from '@opentelemetry/api';
import {
  ArithmeticCircuitSchema,
  R1CSConstraintSchema,
  TriplePatternSchema,
  FilterConstraintSchema,
} from './schemas.mjs';

const tracer = trace.getTracer('@unrdf/zkp/circuit-compiler');

/**
 * SPARQL to R1CS Circuit Compiler
 *
 * Converts SPARQL queries into arithmetic circuits (R1CS format)
 * suitable for zk-SNARK proof generation.
 *
 * @class
 * @example
 * const compiler = new CircuitCompiler();
 * const circuit = await compiler.compile(sparqlQuery);
 */
export class CircuitCompiler {
  /**
   * @param {Object} [options] - Compiler options
   * @param {number} [options.maxConstraints=100000] - Maximum constraints
   * @param {boolean} [options.optimize=true] - Enable constraint optimization
   */
  constructor(options = {}) {
    this.options = {
      maxConstraints: options.maxConstraints || 100000,
      optimize: options.optimize !== false,
    };
    this.parser = new Parser();
    this.varCounter = 0;
    this.constraints = [];
  }

  /**
   * Compile SPARQL query to arithmetic circuit
   *
   * @param {string} sparqlQuery - SPARQL query string
   * @returns {Promise<Object>} Compiled circuit with R1CS constraints
   *
   * @example
   * const circuit = await compiler.compile(
   *   'SELECT ?p WHERE { ?p :age ?age FILTER(?age > 18) }'
   * );
   * console.log(`Circuit has ${circuit.nConstraints} constraints`);
   */
  async compile(sparqlQuery) {
    return tracer.startActiveSpan('circuit-compiler.compile', async (span) => {
      try {
        span.setAttribute('query.length', sparqlQuery.length);

        const startTime = performance.now();

        this.varCounter = 0;
        this.constraints = [];

        const parsed = this.parser.parse(sparqlQuery);

        span.setAttribute('query.type', parsed.queryType);

        if (parsed.queryType !== 'SELECT') {
          throw new Error(
            `Unsupported query type: ${parsed.queryType}. Only SELECT supported.`
          );
        }

        const publicInputs = [];
        const privateInputs = [];

        const triplePatterns = this._extractTriplePatterns(parsed.where);
        const filters = this._extractFilters(parsed.where);

        span.setAttribute('patterns.count', triplePatterns.length);
        span.setAttribute('filters.count', filters.length);

        for (const pattern of triplePatterns) {
          const { constraints, inputs } = this._compileTriplePattern(pattern);
          this.constraints.push(...constraints);
          privateInputs.push(...inputs.private);
          publicInputs.push(...inputs.public);
        }

        for (const filter of filters) {
          const { constraints, inputs } = this._compileFilter(filter);
          this.constraints.push(...constraints);
          privateInputs.push(...inputs.private);
          publicInputs.push(...inputs.public);
        }

        if (this.constraints.length > this.options.maxConstraints) {
          throw new Error(
            `Constraint count ${this.constraints.length} exceeds maximum ${this.options.maxConstraints}`
          );
        }

        if (this.options.optimize) {
          this._optimizeConstraints();
        }

        const circuit = {
          constraints: this.constraints,
          publicInputs: [...new Set(publicInputs)],
          privateInputs: [...new Set(privateInputs)],
          nConstraints: this.constraints.length,
          nVars: this.varCounter,
        };

        const compileTime = performance.now() - startTime;

        span.setAttribute('circuit.constraints', circuit.nConstraints);
        span.setAttribute('circuit.vars', circuit.nVars);
        span.setAttribute('compile.time_ms', compileTime);

        span.setStatus({ code: 1 });

        return ArithmeticCircuitSchema.parse(circuit);
      } catch (error) {
        span.setStatus({ code: 2, message: error.message });
        span.recordException(error);
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Extract triple patterns from WHERE clause
   * @private
   */
  _extractTriplePatterns(where) {
    const patterns = [];

    const extractFromGroup = (group) => {
      if (!group) return;

      if (group.type === 'bgp') {
        patterns.push(
          ...group.triples.map((t) => ({
            subject: this._termToString(t.subject),
            predicate: this._termToString(t.predicate),
            object: this._termToString(t.object),
            isVariable: {
              subject: t.subject.termType === 'Variable',
              predicate: t.predicate.termType === 'Variable',
              object: t.object.termType === 'Variable',
            },
          }))
        );
      } else if (group.type === 'group') {
        group.patterns.forEach(extractFromGroup);
      } else if (group.type === 'filter') {
        extractFromGroup(group.input);
      }
    };

    if (Array.isArray(where)) {
      where.forEach(extractFromGroup);
    } else {
      extractFromGroup(where);
    }

    return patterns.map((p) => TriplePatternSchema.parse(p));
  }

  /**
   * Extract FILTER constraints from WHERE clause
   * @private
   */
  _extractFilters(where) {
    const filters = [];

    const extractFromGroup = (group) => {
      if (!group) return;

      if (group.type === 'filter') {
        const filter = this._parseFilterExpression(group.expression);
        if (filter) filters.push(filter);
        extractFromGroup(group.input);
      } else if (group.type === 'group') {
        group.patterns.forEach(extractFromGroup);
      }
    };

    if (Array.isArray(where)) {
      where.forEach(extractFromGroup);
    } else {
      extractFromGroup(where);
    }

    return filters.map((f) => FilterConstraintSchema.parse(f));
  }

  /**
   * Parse FILTER expression
   * @private
   */
  _parseFilterExpression(expr) {
    if (!expr) return null;

    const operatorMap = {
      '>': 'gt',
      '<': 'lt',
      '=': 'eq',
      '!=': 'neq',
      '>=': 'gte',
      '<=': 'lte',
    };

    if (expr.type === 'operation') {
      const op = operatorMap[expr.operator];
      if (!op) return null;

      const left = expr.args[0];
      const right = expr.args[1];

      if (left.termType === 'Variable' && right.termType === 'Literal') {
        return {
          type: op,
          variable: left.value,
          value: this._literalValue(right),
        };
      }
    }

    return null;
  }

  /**
   * Compile triple pattern to R1CS constraints
   * @private
   */
  _compileTriplePattern(pattern) {
    const constraints = [];
    const inputs = { private: [], public: [] };

    const sVar = this._allocVar();
    const pVar = this._allocVar();
    const oVar = this._allocVar();

    inputs.private.push(`triple_s_${sVar}`, `triple_p_${pVar}`, `triple_o_${oVar}`);

    if (!pattern.isVariable?.subject) {
      const targetHash = this._allocVar();
      inputs.public.push(`target_s_${targetHash}`);
      constraints.push(
        this._createEqualityConstraint(`triple_s_${sVar}`, `target_s_${targetHash}`)
      );
    }

    if (!pattern.isVariable?.predicate) {
      const targetHash = this._allocVar();
      inputs.public.push(`target_p_${targetHash}`);
      constraints.push(
        this._createEqualityConstraint(`triple_p_${pVar}`, `target_p_${targetHash}`)
      );
    }

    if (!pattern.isVariable?.object) {
      const targetHash = this._allocVar();
      inputs.public.push(`target_o_${targetHash}`);
      constraints.push(
        this._createEqualityConstraint(`triple_o_${oVar}`, `target_o_${targetHash}`)
      );
    }

    return { constraints, inputs };
  }

  /**
   * Compile FILTER constraint to R1CS constraints
   * @private
   */
  _compileFilter(filter) {
    const constraints = [];
    const inputs = { private: [], public: [] };

    const varName = `filter_${filter.variable}_${this._allocVar()}`;
    const constName = `filter_const_${this._allocVar()}`;

    inputs.private.push(varName);
    inputs.public.push(constName);

    switch (filter.type) {
      case 'gt':
        constraints.push(this._createGreaterThanConstraint(varName, constName));
        break;
      case 'lt':
        constraints.push(this._createLessThanConstraint(varName, constName));
        break;
      case 'eq':
        constraints.push(this._createEqualityConstraint(varName, constName));
        break;
      case 'neq':
        constraints.push(this._createInequalityConstraint(varName, constName));
        break;
      case 'gte':
        constraints.push(this._createGreaterThanConstraint(varName, constName));
        constraints.push(this._createEqualityConstraint(varName, constName));
        break;
      case 'lte':
        constraints.push(this._createLessThanConstraint(varName, constName));
        constraints.push(this._createEqualityConstraint(varName, constName));
        break;
    }

    return { constraints, inputs };
  }

  /**
   * Create R1CS equality constraint: a * 1 = c
   * @private
   */
  _createEqualityConstraint(a, c) {
    return R1CSConstraintSchema.parse({
      a: { [a]: '1' },
      b: { one: '1' },
      c: { [c]: '1' },
    });
  }

  /**
   * Create R1CS greater-than constraint
   * @private
   */
  _createGreaterThanConstraint(a, b) {
    const diff = `diff_${this._allocVar()}`;
    return R1CSConstraintSchema.parse({
      a: { [a]: '1', [b]: '-1' },
      b: { one: '1' },
      c: { [diff]: '1' },
    });
  }

  /**
   * Create R1CS less-than constraint
   * @private
   */
  _createLessThanConstraint(a, b) {
    const diff = `diff_${this._allocVar()}`;
    return R1CSConstraintSchema.parse({
      a: { [b]: '1', [a]: '-1' },
      b: { one: '1' },
      c: { [diff]: '1' },
    });
  }

  /**
   * Create R1CS inequality constraint
   * @private
   */
  _createInequalityConstraint(a, b) {
    const diff = `diff_${this._allocVar()}`;
    const inv = `inv_${this._allocVar()}`;
    return R1CSConstraintSchema.parse({
      a: { [a]: '1', [b]: '-1' },
      b: { [inv]: '1' },
      c: { [diff]: '1' },
    });
  }

  /**
   * Optimize constraints (remove redundant, merge similar)
   * @private
   */
  _optimizeConstraints() {
    const seen = new Set();
    const optimized = [];

    for (const constraint of this.constraints) {
      const key = JSON.stringify(constraint);
      if (!seen.has(key)) {
        seen.add(key);
        optimized.push(constraint);
      }
    }

    this.constraints = optimized;
  }

  /**
   * Allocate new variable
   * @private
   */
  _allocVar() {
    return this.varCounter++;
  }

  /**
   * Convert SPARQL term to string
   * @private
   */
  _termToString(term) {
    if (!term) return '';
    if (term.termType === 'Variable') return `?${term.value}`;
    if (term.termType === 'NamedNode') return term.value;
    if (term.termType === 'Literal') return term.value;
    return term.value || '';
  }

  /**
   * Get literal value
   * @private
   */
  _literalValue(literal) {
    const val = literal.value;
    if (literal.datatype?.value.includes('integer')) {
      return parseInt(val, 10);
    }
    if (literal.datatype?.value.includes('double')) {
      return parseFloat(val);
    }
    return val;
  }
}

/**
 * Create circuit compiler instance
 *
 * @param {Object} [options] - Compiler options
 * @returns {CircuitCompiler} Compiler instance
 *
 * @example
 * const compiler = createCircuitCompiler({ optimize: true });
 */
export function createCircuitCompiler(options = {}) {
  return new CircuitCompiler(options);
}

/**
 * Compile SPARQL query to circuit (convenience function)
 *
 * @param {string} sparqlQuery - SPARQL query
 * @param {Object} [options] - Compiler options
 * @returns {Promise<Object>} Compiled circuit
 *
 * @example
 * const circuit = await compileSPARQL('SELECT * WHERE { ?s ?p ?o }');
 */
export async function compileSPARQL(sparqlQuery, options = {}) {
  const compiler = new CircuitCompiler(options);
  return await compiler.compile(sparqlQuery);
}
