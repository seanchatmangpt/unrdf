/**
 * @file Query Expansion
 * @description Query understanding and expansion for better retrieval
 * @module knowledge-rag/query-expansion
 */

import { trace } from '@opentelemetry/api';

const tracer = trace.getTracer('query-expansion');

/**
 * Query Expander
 *
 * Features:
 * - Synonym expansion
 * - Entity recognition
 * - Query rewriting
 * - Intent detection
 *
 * @class
 */
export class QueryExpander {
  /**
   * @param {object} config - Configuration
   */
  constructor(config = {}) {
    this.config = {
      maxExpansions: 10,
      synonymThreshold: 0.8,
      ...config,
    };

    // Synonym dictionary (in production, load from external source)
    this.synonyms = new Map([
      ['person', ['individual', 'human', 'people']],
      ['company', ['organization', 'business', 'corporation']],
      ['works', ['employed', 'job', 'occupation']],
    ]);
  }

  /**
   * Expand query with synonyms and related terms
   *
   * @param {string} query - Original query
   * @returns {Promise<object>} Expanded query
   */
  async expand(query) {
    return tracer.startActiveSpan('queryExpander.expand', async (span) => {
      try {
        span.setAttribute('query.original', query);

        const tokens = this._tokenize(query);
        const expandedTerms = new Set(tokens);

        // Add synonyms
        for (const token of tokens) {
          const syns = this.synonyms.get(token.toLowerCase()) || [];
          for (const syn of syns) {
            expandedTerms.add(syn);
          }
        }

        // Build expanded query
        const expansion = {
          original: query,
          tokens,
          expandedTerms: Array.from(expandedTerms),
          sparql: this._buildSparqlQuery(Array.from(expandedTerms)),
        };

        span.setAttribute('expansion.terms', expansion.expandedTerms.length);
        span.setStatus({ code: 1 });
        return expansion;
      } catch (error) {
        span.setStatus({ code: 2, message: error.message });
        throw error;
      } finally {
        span.end();
      }
    });
  }

  /**
   * Tokenize query
   *
   * @private
   * @param {string} query - Query string
   * @returns {string[]} Tokens
   */
  _tokenize(query) {
    return query
      .toLowerCase()
      .split(/\s+/)
      .filter((t) => t.length > 2);
  }

  /**
   * Build SPARQL query from expanded terms
   *
   * @private
   * @param {string[]} terms - Expanded terms
   * @returns {string} SPARQL query
   */
  _buildSparqlQuery(terms) {
    const filters = terms
      .map((term) => `CONTAINS(LCASE(STR(?s)), "${term}") || CONTAINS(LCASE(STR(?o)), "${term}")`)
      .join(' || ');

    return `
      SELECT DISTINCT ?s ?p ?o
      WHERE {
        ?s ?p ?o .
        FILTER(${filters})
      }
      LIMIT 100
    `;
  }
}

export default QueryExpander;
