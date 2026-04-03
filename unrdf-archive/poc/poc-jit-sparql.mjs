/**
 * POC: JIT-Compiled SPARQL Query Plans
 *
 * Pre-compiles hot SPARQL queries into optimized JavaScript functions
 * using pattern analysis and code generation.
 *
 * Expected Performance: 2-10x faster for hot queries
 *
 * @module poc-jit-sparql
 */

import { trace, SpanStatusCode } from '@opentelemetry/api';

/**
 * SPARQL Query Compiler
 * Analyzes query structure and generates optimized execution plan
 */
export class SPARQLJITCompiler {
  constructor() {
    this.compiledQueries = new Map();
    this.cspSafe = this.detectCSP();
  }

  /**
   * Detect if Content Security Policy allows new Function()
   * @returns {boolean} True if JIT compilation is available
   */
  detectCSP() {
    try {
      new Function('return true')();
      return true;
    } catch (e) {
      console.warn('CSP blocks JIT compilation, falling back to interpreter');
      return false;
    }
  }

  /**
   * Compile SPARQL SELECT query to optimized function
   * @param {string} sparql - SPARQL query string
   * @returns {Function} Compiled query executor
   */
  compile(sparql) {
    const cacheKey = sparql;
    if (this.compiledQueries.has(cacheKey)) {
      return this.compiledQueries.get(cacheKey);
    }

    const ast = this.parseSPARQL(sparql);
    const plan = this.optimizePlan(ast);
    const compiled = this.cspSafe
      ? this.compileJIT(plan)
      : this.compileInterpreted(plan);

    this.compiledQueries.set(cacheKey, compiled);
    return compiled;
  }

  /**
   * Parse SPARQL into AST
   * @param {string} sparql - SPARQL query string
   * @returns {object} Abstract syntax tree
   */
  parseSPARQL(sparql) {
    // Simplified parser (production: use sparqljs)
    const selectMatch = sparql.match(/SELECT\s+(.*?)\s+WHERE/i);
    const whereMatch = sparql.match(/WHERE\s*\{(.*)\}/is);

    return {
      type: 'SELECT',
      variables: selectMatch?.[1]?.split(/\s+/) || [],
      patterns: this.parsePatterns(whereMatch?.[1] || ''),
    };
  }

  /**
   * Parse triple patterns from WHERE clause
   * @param {string} whereClause - WHERE clause content
   * @returns {Array<object>} Triple patterns
   */
  parsePatterns(whereClause) {
    const patterns = [];
    const tripleRegex = /(\S+)\s+(\S+)\s+(\S+)/g;
    let match;

    while ((match = tripleRegex.exec(whereClause)) !== null) {
      patterns.push({
        subject: match[1],
        predicate: match[2],
        object: match[3],
      });
    }

    return patterns;
  }

  /**
   * Optimize query plan (reorder patterns, push filters, etc.)
   * @param {object} ast - Abstract syntax tree
   * @returns {object} Optimized plan
   */
  optimizePlan(ast) {
    // Simple optimization: specific patterns first
    const patterns = ast.patterns.slice();
    patterns.sort((a, b) => {
      const scoreA = this.patternSpecificity(a);
      const scoreB = this.patternSpecificity(b);
      return scoreB - scoreA; // Most specific first
    });

    return { ...ast, patterns };
  }

  /**
   * Calculate pattern specificity (higher = more specific)
   * @param {object} pattern - Triple pattern
   * @returns {number} Specificity score
   */
  patternSpecificity(pattern) {
    let score = 0;
    if (!pattern.subject.startsWith('?')) score += 100;
    if (!pattern.predicate.startsWith('?')) score += 10;
    if (!pattern.object.startsWith('?')) score += 1;
    return score;
  }

  /**
   * JIT compile to native Function (fastest)
   * @param {object} plan - Query plan
   * @returns {Function} Compiled function
   */
  compileJIT(plan) {
    // Generate JavaScript code
    const code = this.generateCode(plan);

    // Compile to native function
    try {
      return new Function('store', code);
    } catch (e) {
      console.error('JIT compilation failed:', e);
      return this.compileInterpreted(plan);
    }
  }

  /**
   * Generate optimized JavaScript code for query plan
   * @param {object} plan - Query plan
   * @returns {string} Generated JavaScript code
   */
  generateCode(plan) {
    const lines = [];
    lines.push('const results = [];');
    lines.push('const bindings = {};');

    // Generate code for each pattern
    for (let i = 0; i < plan.patterns.length; i++) {
      const pattern = plan.patterns[i];
      const iterVar = `iter${i}`;

      // Determine which store method to call
      const subject = this.resolveValue(pattern.subject, 'bindings');
      const predicate = this.resolveValue(pattern.predicate, 'bindings');
      const object = this.resolveValue(pattern.object, 'bindings');

      lines.push(`// Pattern ${i}: ${pattern.subject} ${pattern.predicate} ${pattern.object}`);
      lines.push(`const ${iterVar} = store.match(${subject}, ${predicate}, ${object});`);
      lines.push(`for (const quad of ${iterVar}) {`);

      // Bind variables
      if (pattern.subject.startsWith('?')) {
        const varName = pattern.subject.slice(1);
        lines.push(`  bindings['${varName}'] = quad.subject;`);
      }
      if (pattern.predicate.startsWith('?')) {
        const varName = pattern.predicate.slice(1);
        lines.push(`  bindings['${varName}'] = quad.predicate;`);
      }
      if (pattern.object.startsWith('?')) {
        const varName = pattern.object.slice(1);
        lines.push(`  bindings['${varName}'] = quad.object;`);
      }

      if (i === plan.patterns.length - 1) {
        // Last pattern: collect results
        lines.push(`  results.push({ ...bindings });`);
      }
    }

    // Close all loops
    for (let i = 0; i < plan.patterns.length; i++) {
      lines.push('}');
    }

    lines.push('return results;');
    return lines.join('\n');
  }

  /**
   * Resolve pattern value to code expression
   * @param {string} value - Pattern value
   * @param {string} bindingsVar - Bindings variable name
   * @returns {string} Code expression
   */
  resolveValue(value, bindingsVar) {
    if (value.startsWith('?')) {
      const varName = value.slice(1);
      return `${bindingsVar}['${varName}'] || null`;
    } else if (value.startsWith('<') && value.endsWith('>')) {
      // URI - would need dataFactory in production
      return `'${value}'`;
    } else {
      return `'${value}'`;
    }
  }

  /**
   * Interpreted execution (CSP fallback)
   * @param {object} plan - Query plan
   * @returns {Function} Interpreter function
   */
  compileInterpreted(plan) {
    return (store) => {
      const results = [];
      const bindings = {};

      const execute = (patternIndex) => {
        if (patternIndex >= plan.patterns.length) {
          results.push({ ...bindings });
          return;
        }

        const pattern = plan.patterns[patternIndex];
        const s = this.resolveBinding(pattern.subject, bindings);
        const p = this.resolveBinding(pattern.predicate, bindings);
        const o = this.resolveBinding(pattern.object, bindings);

        for (const quad of store.match(s, p, o)) {
          if (pattern.subject.startsWith('?')) {
            bindings[pattern.subject.slice(1)] = quad.subject;
          }
          if (pattern.predicate.startsWith('?')) {
            bindings[pattern.predicate.slice(1)] = quad.predicate;
          }
          if (pattern.object.startsWith('?')) {
            bindings[pattern.object.slice(1)] = quad.object;
          }

          execute(patternIndex + 1);
        }
      };

      execute(0);
      return results;
    };
  }

  /**
   * Resolve binding value
   * @param {string} value - Pattern value
   * @param {object} bindings - Current bindings
   * @returns {any} Resolved value
   */
  resolveBinding(value, bindings) {
    if (value.startsWith('?')) {
      return bindings[value.slice(1)] || null;
    }
    return value;
  }
}

/**
 * Mock RDF store for benchmarking
 */
class MockStore {
  constructor(triples = []) {
    this.triples = triples;
  }

  match(s, p, o) {
    return this.triples.filter(quad => {
      if (s !== null && quad.subject !== s) return false;
      if (p !== null && quad.predicate !== p) return false;
      if (o !== null && quad.object !== o) return false;
      return true;
    });
  }
}

/**
 * Benchmark JIT compilation vs interpretation
 * @returns {Promise<object>} Benchmark results
 */
export async function benchmarkJIT() {
  console.log('\nJIT-Compiled SPARQL Benchmark');
  console.log('=============================\n');

  // Create mock store with test data
  const triples = [];
  for (let i = 0; i < 1000; i++) {
    triples.push({
      subject: `http://ex.org/s${i}`,
      predicate: `http://ex.org/knows`,
      object: `http://ex.org/s${i + 1}`,
    });
  }
  const store = new MockStore(triples);

  const query = 'SELECT ?s ?o WHERE { ?s <http://ex.org/knows> ?o }';

  // Method 1: Interpreted (baseline)
  const compiler = new SPARQLJITCompiler();
  compiler.cspSafe = false; // Force interpreted mode
  const compiledInterpreted = compiler.compile(query);

  const iterations = 100;
  const startInterpreted = performance.now();
  for (let i = 0; i < iterations; i++) {
    const results = compiledInterpreted(store);
  }
  const timeInterpreted = performance.now() - startInterpreted;

  // Method 2: JIT-compiled
  compiler.cspSafe = true; // Enable JIT
  compiler.compiledQueries.clear(); // Clear cache
  const compiledJIT = compiler.compile(query);

  const startJIT = performance.now();
  for (let i = 0; i < iterations; i++) {
    const results = compiledJIT(store);
  }
  const timeJIT = performance.now() - startJIT;

  const results = {
    iterations,
    timeInterpreted,
    timeJIT,
    speedup: timeInterpreted / timeJIT,
    latencyInterpreted: timeInterpreted / iterations,
    latencyJIT: timeJIT / iterations,
  };

  console.log(`Iterations: ${iterations}`);
  console.log(`Interpreted: ${timeInterpreted.toFixed(2)}ms (${results.latencyInterpreted.toFixed(3)}ms/query)`);
  console.log(`JIT-compiled: ${timeJIT.toFixed(2)}ms (${results.latencyJIT.toFixed(3)}ms/query)`);
  console.log(`Speedup: ${results.speedup.toFixed(2)}x\n`);

  return results;
}

// Run benchmark if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  benchmarkJIT().catch(console.error);
}
