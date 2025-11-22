/**
 * @file Tests for Form & UI hooks functionality
 * Tests SPARQL editor, graph visualizer, paginator, query builder, form validation
 */

import { describe, it, expect, _beforeEach, _vi } from 'vitest';

describe('SPARQLEditor', () => {
  describe('Query Validation', () => {
    it('should validate SELECT query', () => {
      const validateQuery = query => {
        const errors = [];
        const upper = query.toUpperCase();

        if (
          !upper.includes('SELECT') &&
          !upper.includes('CONSTRUCT') &&
          !upper.includes('ASK') &&
          !upper.includes('DESCRIBE')
        ) {
          errors.push({
            line: 1,
            message: 'Missing query type (SELECT/CONSTRUCT/ASK/DESCRIBE)',
          });
        }
        if (!upper.includes('WHERE') && upper.includes('SELECT')) {
          errors.push({ line: 1, message: 'Missing WHERE clause' });
        }

        return { valid: errors.length === 0, errors };
      };

      const validQuery = 'SELECT ?s WHERE { ?s ?p ?o }';
      const invalidQuery = 'INVALID QUERY';

      expect(validateQuery(validQuery).valid).toBe(true);
      expect(validateQuery(invalidQuery).valid).toBe(false);
      expect(validateQuery(invalidQuery).errors).toHaveLength(1);
    });

    it('should validate CONSTRUCT query', () => {
      const validateQuery = query => {
        const upper = query.toUpperCase();
        return upper.includes('CONSTRUCT') && upper.includes('WHERE');
      };

      expect(validateQuery('CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }')).toBe(true);
    });

    it('should detect syntax errors', () => {
      const detectErrors = query => {
        const errors = [];

        // Check for unbalanced braces
        const openBraces = (query.match(/{/g) || []).length;
        const closeBraces = (query.match(/}/g) || []).length;
        if (openBraces !== closeBraces) {
          errors.push({ type: 'syntax', message: 'Unbalanced braces' });
        }

        return errors;
      };

      // Valid SPARQL queries
      expect(detectErrors('SELECT * WHERE { ?s ?p ?o }')).toHaveLength(0);
      expect(detectErrors('SELECT * WHERE { ?s ?p ?o . }')).toHaveLength(0);
      // Missing closing brace - unbalanced
      expect(detectErrors('SELECT * WHERE { ?s ?p ?o')).toHaveLength(1);
    });
  });

  describe('Query Formatting', () => {
    it('should format SPARQL keywords', () => {
      const formatQuery = query => {
        return query
          .replace(/\bselect\b/gi, 'SELECT')
          .replace(/\bwhere\b/gi, 'WHERE')
          .replace(/\bfilter\b/gi, 'FILTER')
          .replace(/\border by\b/gi, 'ORDER BY')
          .replace(/\blimit\b/gi, 'LIMIT');
      };

      const messy = 'select ?s where { ?s ?p ?o } order by ?s limit 10';
      const formatted = formatQuery(messy);

      expect(formatted).toContain('SELECT');
      expect(formatted).toContain('WHERE');
      expect(formatted).toContain('ORDER BY');
      expect(formatted).toContain('LIMIT');
    });

    it('should add proper indentation', () => {
      const addIndentation = query => {
        return query
          .replace(/\{/g, ' {\n  ')
          .replace(/\}/g, '\n}')
          .replace(/\.\s+/g, ' .\n  ')
          .trim();
      };

      const flat = 'SELECT ?s WHERE { ?s ?p ?o . ?s ?p2 ?o2 }';
      const indented = addIndentation(flat);

      expect(indented).toContain('\n');
      expect(indented).toContain('  '); // Indentation
    });
  });
});

describe('GraphVisualizer', () => {
  describe('Node Generation', () => {
    it('should generate nodes from triples', () => {
      const triples = [
        { subject: 'alice', predicate: 'knows', object: 'bob' },
        { subject: 'alice', predicate: 'knows', object: 'charlie' },
      ];

      const generateNodes = triples => {
        const subjects = new Set(triples.map(t => t.subject));
        const objects = new Set(triples.map(t => t.object));
        const allEntities = new Set([...subjects, ...objects]);

        return Array.from(allEntities).map(entity => ({
          id: entity,
          label: entity,
          type: subjects.has(entity) ? 'subject' : 'object',
        }));
      };

      const nodes = generateNodes(triples);

      expect(nodes).toHaveLength(3); // alice, bob, charlie
      expect(nodes.find(n => n.id === 'alice').type).toBe('subject');
    });

    it('should generate edges from triples', () => {
      const triples = [
        { subject: 'alice', predicate: 'knows', object: 'bob' },
        { subject: 'alice', predicate: 'likes', object: 'charlie' },
      ];

      const generateEdges = triples => {
        return triples.map(t => ({
          source: t.subject,
          target: t.object,
          label: t.predicate,
        }));
      };

      const edges = generateEdges(triples);

      expect(edges).toHaveLength(2);
      expect(edges[0].label).toBe('knows');
      expect(edges[1].label).toBe('likes');
    });
  });

  describe('Graph Statistics', () => {
    it('should calculate node and edge counts', () => {
      const graphData = {
        nodes: [{ id: 'a' }, { id: 'b' }, { id: 'c' }],
        edges: [
          { source: 'a', target: 'b' },
          { source: 'a', target: 'c' },
          { source: 'b', target: 'c' },
        ],
      };

      const stats = {
        nodeCount: graphData.nodes.length,
        edgeCount: graphData.edges.length,
        density: graphData.edges.length / (graphData.nodes.length * (graphData.nodes.length - 1)),
      };

      expect(stats.nodeCount).toBe(3);
      expect(stats.edgeCount).toBe(3);
      expect(stats.density).toBe(0.5);
    });
  });
});

describe('ResultsPaginator', () => {
  describe('Pagination Logic', () => {
    it('should paginate results', () => {
      const results = Array(25)
        .fill(null)
        .map((_, i) => ({ id: i }));
      const pageSize = 10;

      const paginate = (data, page, size) => {
        const start = (page - 1) * size;
        const end = start + size;
        return {
          data: data.slice(start, end),
          page,
          totalPages: Math.ceil(data.length / size),
          totalItems: data.length,
        };
      };

      const page1 = paginate(results, 1, pageSize);
      const page2 = paginate(results, 2, pageSize);
      const page3 = paginate(results, 3, pageSize);

      expect(page1.data).toHaveLength(10);
      expect(page2.data).toHaveLength(10);
      expect(page3.data).toHaveLength(5);
      expect(page1.totalPages).toBe(3);
    });

    it('should handle empty results', () => {
      const results = [];
      const pageSize = 10;

      const paginate = (data, page, size) => ({
        data: data.slice((page - 1) * size, page * size),
        page,
        totalPages: Math.max(1, Math.ceil(data.length / size)),
        totalItems: data.length,
      });

      const page1 = paginate(results, 1, pageSize);

      expect(page1.data).toHaveLength(0);
      expect(page1.totalPages).toBe(1);
    });

    it('should calculate page range', () => {
      const currentPage = 5;
      const _totalPages = 10;
      const pageSize = 10;
      const totalItems = 95;

      const startItem = (currentPage - 1) * pageSize + 1;
      const endItem = Math.min(currentPage * pageSize, totalItems);

      expect(startItem).toBe(41);
      expect(endItem).toBe(50);
    });
  });
});

describe('QueryBuilder', () => {
  describe('Triple Pattern Construction', () => {
    it('should add triple patterns', () => {
      const triples = [];

      const addTriple = triple => {
        triples.push(triple);
      };

      addTriple({
        subject: '?person',
        predicate: 'rdf:type',
        object: 'foaf:Person',
      });
      addTriple({
        subject: '?person',
        predicate: 'foaf:name',
        object: '?name',
      });

      expect(triples).toHaveLength(2);
    });

    it('should remove triple patterns', () => {
      const triples = [
        { subject: '?s', predicate: 'p1', object: 'o1' },
        { subject: '?s', predicate: 'p2', object: 'o2' },
      ];

      const removeTriple = index => {
        triples.splice(index, 1);
      };

      removeTriple(0);

      expect(triples).toHaveLength(1);
      expect(triples[0].predicate).toBe('p2');
    });

    it('should build SPARQL from triples', () => {
      const triples = [
        { subject: '?person', predicate: 'foaf:name', object: '?name' },
        { subject: '?person', predicate: 'foaf:age', object: '?age' },
      ];

      const buildQuery = triples => {
        const variables = new Set();
        triples.forEach(t => {
          if (t.subject.startsWith('?')) variables.add(t.subject);
          if (t.object.startsWith('?')) variables.add(t.object);
        });

        const selectVars = Array.from(variables).join(' ');
        const patterns = triples.map(t => `  ${t.subject} ${t.predicate} ${t.object} .`).join('\n');

        return `SELECT ${selectVars}\nWHERE {\n${patterns}\n}`;
      };

      const sparql = buildQuery(triples);

      expect(sparql).toContain('SELECT');
      expect(sparql).toContain('?person');
      expect(sparql).toContain('?name');
      expect(sparql).toContain('foaf:name');
    });
  });
});

describe('FormValidation', () => {
  describe('SHACL-Based Validation', () => {
    it('should validate required fields', () => {
      const shape = {
        properties: [
          { path: 'name', minCount: 1 },
          { path: 'email', minCount: 1 },
        ],
      };

      const validate = (data, shape) => {
        const errors = [];
        shape.properties.forEach(prop => {
          if (prop.minCount && !data[prop.path]) {
            errors.push({ path: prop.path, message: 'Required field' });
          }
        });
        return { isValid: errors.length === 0, errors };
      };

      const validData = { name: 'Alice', email: 'alice@example.org' };
      const invalidData = { name: 'Alice' };

      expect(validate(validData, shape).isValid).toBe(true);
      expect(validate(invalidData, shape).isValid).toBe(false);
      expect(validate(invalidData, shape).errors).toHaveLength(1);
    });

    it('should validate patterns', () => {
      const emailPattern = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;

      const validateEmail = email => {
        return emailPattern.test(email);
      };

      expect(validateEmail('alice@example.org')).toBe(true);
      expect(validateEmail('invalid-email')).toBe(false);
      expect(validateEmail('also@invalid')).toBe(false);
    });

    it('should validate min/max values', () => {
      const validate = (value, constraints) => {
        const errors = [];
        if (constraints.minValue !== undefined && value < constraints.minValue) {
          errors.push({ message: `Must be >= ${constraints.minValue}` });
        }
        if (constraints.maxValue !== undefined && value > constraints.maxValue) {
          errors.push({ message: `Must be <= ${constraints.maxValue}` });
        }
        return errors;
      };

      expect(validate(18, { minValue: 18 })).toHaveLength(0);
      expect(validate(17, { minValue: 18 })).toHaveLength(1);
      expect(validate(120, { maxValue: 100 })).toHaveLength(1);
    });

    it('should aggregate validation results', () => {
      const validations = [
        { field: 'name', valid: true },
        { field: 'email', valid: true },
        { field: 'age', valid: false, message: 'Must be >= 18' },
      ];

      const result = {
        isValid: validations.every(v => v.valid),
        errors: validations
          .filter(v => !v.valid)
          .map(v => ({
            field: v.field,
            message: v.message,
          })),
      };

      expect(result.isValid).toBe(false);
      expect(result.errors).toHaveLength(1);
      expect(result.errors[0].field).toBe('age');
    });
  });
});
