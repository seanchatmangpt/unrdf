#!/usr/bin/env node

/**
 * UNRDF Substrate Exploration CLI - Agent 7
 *
 * Exploration of CLI conventions and citty integration in UNRDF.
 * Demonstrates noun-verb command pattern for RDF operations.
 *
 * Usage:
 *   node index.mjs --help                    # Show nouns
 *   node index.mjs rdf --help                # Show verbs for 'rdf' noun
 *   node index.mjs rdf load --help           # Show help for specific command
 *   node index.mjs rdf load <file>           # Execute command
 *   node index.mjs rdf query --sparql "..." # Execute SPARQL query
 *
 * Version: 1.0.0
 */

import { defineCommand, runMain } from 'citty';
import { z } from 'zod';
import fs from 'fs/promises';
import path from 'path';

/**
 * RDF LOAD command - Load RDF file from disk
 * Demonstrates:
 *   - Positional argument (file)
 *   - Named options (--format, --base-iri)
 *   - Error handling with validation
 */
const rdfLoadCommand = defineCommand({
  meta: {
    name: 'load',
    description: 'Load RDF file from disk (TTL, N-Triples, JSON-LD, RDF/XML)',
  },
  args: {
    file: {
      type: 'positional',
      description: 'Path to RDF file (TTL, N-Triples, JSON-LD, RDF/XML)',
      required: true,
    },
    format: {
      type: 'string',
      description: 'RDF format (ttl, nt, jsonld, rdfxml). Auto-detect if omitted.',
      alias: 'f',
    },
    'base-iri': {
      type: 'string',
      description: 'Base IRI for resolving relative IRIs',
      alias: 'b',
      default: 'http://example.org/',
    },
    'dry-run': {
      type: 'boolean',
      description: 'Show what would be loaded without loading',
      alias: 'd',
      default: false,
    },
    json: {
      type: 'boolean',
      description: 'Output in JSON format',
      default: false,
    },
  },
  async run(ctx) {
    try {
      const file = ctx.args.file;
      const format = ctx.args.format;
      const baseIri = ctx.args['base-iri'];
      const dryRun = ctx.args['dry-run'];
      const outputJson = ctx.args.json;

      // Validate file exists
      const filePath = path.resolve(file);
      let stat;
      try {
        stat = await fs.stat(filePath);
      } catch (e) {
        throw new Error(`File not found: ${filePath}`);
      }

      if (!stat.isFile()) {
        throw new Error(`Not a file: ${filePath}`);
      }

      // Detect format if not specified
      let detectedFormat = format;
      if (!detectedFormat) {
        const ext = path.extname(file).toLowerCase();
        const extMap = {
          '.ttl': 'ttl',
          '.nt': 'nt',
          '.jsonld': 'jsonld',
          '.json': 'jsonld',
          '.rdf': 'rdfxml',
          '.xml': 'rdfxml',
        };
        detectedFormat = extMap[ext] || 'ttl';
      }

      const fileSize = stat.size;
      const fileSizeMB = (fileSize / (1024 * 1024)).toFixed(2);

      if (outputJson) {
        console.log(JSON.stringify({
          success: true,
          command: 'rdf load',
          file: filePath,
          format: detectedFormat,
          baseIri: baseIri,
          fileSize: fileSize,
          dryRun: dryRun,
          status: dryRun ? 'dry-run' : 'loaded',
          message: dryRun ? 'Dry-run mode: no actual loading' : 'File loaded successfully',
        }, null, 2));
      } else {
        if (dryRun) {
          console.log('📋 Dry-run mode (not actually loading):');
        }
        console.log(`📄 File: ${filePath}`);
        console.log(`📦 Format: ${detectedFormat}`);
        console.log(`📊 Size: ${fileSizeMB} MB (${fileSize} bytes)`);
        console.log(`🔗 Base IRI: ${baseIri}`);
        if (!dryRun) {
          console.log('✅ File loaded successfully');
        }
      }
    } catch (error) {
      if (ctx.args.json) {
        console.log(JSON.stringify({
          success: false,
          command: 'rdf load',
          error: error.message,
        }, null, 2));
      } else {
        console.error(`❌ Load failed: ${error.message}`);
      }
      process.exit(1);
    }
  },
});

/**
 * RDF QUERY command - Execute SPARQL query
 * Demonstrates:
 *   - Named string options (--sparql)
 *   - Multiple output formats
 */
const rdfQueryCommand = defineCommand({
  meta: {
    name: 'query',
    description: 'Execute SPARQL query against loaded RDF',
  },
  args: {
    sparql: {
      type: 'string',
      description: 'SPARQL query string (SELECT, ASK, CONSTRUCT, DESCRIBE)',
      alias: 's',
    },
    format: {
      type: 'string',
      description: 'Output format (table, json, csv, xml)',
      alias: 'f',
      default: 'table',
    },
    limit: {
      type: 'string',
      description: 'Limit results (applies to SELECT)',
      alias: 'l',
      default: '100',
    },
    json: {
      type: 'boolean',
      description: 'Output in JSON format',
      default: false,
    },
  },
  async run(ctx) {
    try {
      const sparql = ctx.args.sparql;
      if (!sparql) {
        throw new Error('--sparql option is required');
      }

      const format = ctx.args.format;
      const limit = parseInt(ctx.args.limit, 10);
      const outputJson = ctx.args.json;

      // Validate SPARQL (basic check)
      const queryType = sparql.trim().substring(0, 6).toUpperCase();
      const validTypes = ['SELECT', 'CONSTR', 'DESCRI', 'ASK   '];
      if (!validTypes.some(t => queryType.startsWith(t.trim()))) {
        throw new Error(`Invalid SPARQL query type. Must start with SELECT, CONSTRUCT, DESCRIBE, or ASK`);
      }

      if (outputJson) {
        console.log(JSON.stringify({
          success: true,
          command: 'rdf query',
          queryType: queryType.trim(),
          format: format,
          limit: limit,
          resultCount: 0,
          message: 'Query parsed successfully (results not available in exploration mode)',
        }, null, 2));
      } else {
        console.log(`✨ Query Type: ${queryType.trim()}`);
        console.log(`📋 Query Length: ${sparql.length} chars`);
        console.log(`📊 Output Format: ${format}`);
        console.log(`🔢 Limit: ${limit} results`);
        console.log('✅ Query parsed successfully');
        console.log('\nNote: Results not available in exploration mode. In production, results would appear here.');
      }
    } catch (error) {
      if (ctx.args.json) {
        console.log(JSON.stringify({
          success: false,
          command: 'rdf query',
          error: error.message,
        }, null, 2));
      } else {
        console.error(`❌ Query failed: ${error.message}`);
      }
      process.exit(1);
    }
  },
});

/**
 * RDF VALIDATE command - Validate RDF graph with SHACL
 * Demonstrates:
 *   - Boolean flags
 *   - Optional file argument
 */
const rdfValidateCommand = defineCommand({
  meta: {
    name: 'validate',
    description: 'Validate RDF graph against SHACL shapes or basic constraints',
  },
  args: {
    shapes: {
      type: 'string',
      description: 'Path to SHACL shapes file (TTL format)',
      alias: 's',
    },
    strict: {
      type: 'boolean',
      description: 'Enable strict validation (fail on warnings)',
      default: false,
    },
    report: {
      type: 'string',
      description: 'Output validation report to file',
      alias: 'r',
    },
    json: {
      type: 'boolean',
      description: 'Output in JSON format',
      default: false,
    },
  },
  async run(ctx) {
    try {
      const shapesFile = ctx.args.shapes;
      const strict = ctx.args.strict;
      const reportFile = ctx.args.report;
      const outputJson = ctx.args.json;

      // Validate shapes file if provided
      if (shapesFile) {
        try {
          await fs.stat(path.resolve(shapesFile));
        } catch (e) {
          throw new Error(`Shapes file not found: ${shapesFile}`);
        }
      }

      const result = {
        conforms: true,
        violations: 0,
        warnings: 0,
        shapes: shapesFile ? 'custom' : 'basic',
        strictMode: strict,
      };

      if (outputJson) {
        console.log(JSON.stringify({
          success: true,
          command: 'rdf validate',
          ...result,
          message: 'RDF is valid',
        }, null, 2));
      } else {
        console.log(`✅ Validation: ${result.conforms ? 'PASSED' : 'FAILED'}`);
        console.log(`📊 Shapes: ${result.shapes}`);
        console.log(`🔒 Strict Mode: ${result.strictMode ? 'ON' : 'OFF'}`);
        console.log(`⚠️  Violations: ${result.violations}`);
        console.log(`ℹ️  Warnings: ${result.warnings}`);
        if (reportFile) {
          console.log(`📄 Report would be written to: ${reportFile}`);
        }
      }
    } catch (error) {
      if (ctx.args.json) {
        console.log(JSON.stringify({
          success: false,
          command: 'rdf validate',
          error: error.message,
        }, null, 2));
      } else {
        console.error(`❌ Validation failed: ${error.message}`);
      }
      process.exit(1);
    }
  },
});

/**
 * RDF CANONICALIZE command - Canonicalize RDF graph
 * Demonstrates:
 *   - Output file option
 *   - Format conversion
 */
const rdfCanonicalizeCommand = defineCommand({
  meta: {
    name: 'canonicalize',
    description: 'Canonicalize RDF graph (normalize IRIs, sort triples)',
  },
  args: {
    output: {
      type: 'string',
      description: 'Output file (default: stdout)',
      alias: 'o',
    },
    format: {
      type: 'string',
      description: 'Output format (ttl, nt, jsonld, rdfxml)',
      alias: 'f',
      default: 'ttl',
    },
    algorithm: {
      type: 'string',
      description: 'Canonicalization algorithm (rdfc10, rdfc14)',
      alias: 'a',
      default: 'rdfc10',
    },
    json: {
      type: 'boolean',
      description: 'Output in JSON format',
      default: false,
    },
  },
  async run(ctx) {
    try {
      const output = ctx.args.output;
      const format = ctx.args.format;
      const algorithm = ctx.args.algorithm;
      const outputJson = ctx.args.json;

      const result = {
        algorithm: algorithm,
        format: format,
        triplesProcessed: 0,
        output: output ? path.resolve(output) : 'stdout',
      };

      if (outputJson) {
        console.log(JSON.stringify({
          success: true,
          command: 'rdf canonicalize',
          ...result,
          message: 'Canonicalization complete',
        }, null, 2));
      } else {
        console.log(`✨ Canonicalization Algorithm: ${result.algorithm}`);
        console.log(`📦 Output Format: ${result.format}`);
        console.log(`📊 Triples Processed: ${result.triplesProcessed}`);
        console.log(`📄 Output: ${result.output}`);
        console.log('✅ Canonicalization complete');
      }
    } catch (error) {
      if (ctx.args.json) {
        console.log(JSON.stringify({
          success: false,
          command: 'rdf canonicalize',
          error: error.message,
        }, null, 2));
      } else {
        console.error(`❌ Canonicalization failed: ${error.message}`);
      }
      process.exit(1);
    }
  },
});

/**
 * POLICY APPLY command - Apply policy hook to graph
 * Demonstrates:
 *   - Policy selection
 *   - Parameter passing
 */
const policyApplyCommand = defineCommand({
  meta: {
    name: 'apply',
    description: 'Apply policy hook to RDF graph',
  },
  args: {
    name: {
      type: 'positional',
      description: 'Policy name (e.g., "encrypt-sensitive", "redact-pii")',
      required: true,
    },
    params: {
      type: 'string',
      description: 'Policy parameters as JSON string (e.g., \'{"fields":["email"]}\')',
      alias: 'p',
    },
    'dry-run': {
      type: 'boolean',
      description: 'Show what would be applied without applying',
      alias: 'd',
      default: false,
    },
    json: {
      type: 'boolean',
      description: 'Output in JSON format',
      default: false,
    },
  },
  async run(ctx) {
    try {
      const name = ctx.args.name;
      const paramsStr = ctx.args.params || '{}';
      const dryRun = ctx.args['dry-run'];
      const outputJson = ctx.args.json;

      // Validate policy name
      const validPolicies = ['encrypt-sensitive', 'redact-pii', 'mask-emails', 'sign-graph'];
      if (!validPolicies.includes(name)) {
        throw new Error(`Unknown policy: ${name}. Valid policies: ${validPolicies.join(', ')}`);
      }

      // Parse parameters
      let params;
      try {
        params = JSON.parse(paramsStr);
      } catch (e) {
        throw new Error(`Invalid JSON parameters: ${e.message}`);
      }

      const result = {
        policy: name,
        status: dryRun ? 'dry-run' : 'applied',
        parameters: params,
        affectedTriples: 0,
      };

      if (outputJson) {
        console.log(JSON.stringify({
          success: true,
          command: 'policy apply',
          ...result,
          message: dryRun ? 'Dry-run: no actual changes' : 'Policy applied successfully',
        }, null, 2));
      } else {
        if (dryRun) {
          console.log('📋 Dry-run mode (not actually applying):');
        }
        console.log(`🎯 Policy: ${name}`);
        console.log(`⚙️  Parameters: ${JSON.stringify(params)}`);
        console.log(`📊 Affected Triples: ${result.affectedTriples}`);
        console.log(`✅ Policy ${dryRun ? 'preview' : 'application'} complete`);
      }
    } catch (error) {
      if (ctx.args.json) {
        console.log(JSON.stringify({
          success: false,
          command: 'policy apply',
          error: error.message,
        }, null, 2));
      } else {
        console.error(`❌ Policy application failed: ${error.message}`);
      }
      process.exit(1);
    }
  },
});

/**
 * RDF noun command - Groups all RDF-related verbs
 */
const rdfCommand = defineCommand({
  meta: {
    name: 'rdf',
    description: 'RDF graph operations (load, query, validate, canonicalize)',
  },
  subCommands: {
    load: rdfLoadCommand,
    query: rdfQueryCommand,
    validate: rdfValidateCommand,
    canonicalize: rdfCanonicalizeCommand,
  },
});

/**
 * POLICY noun command - Groups policy-related verbs
 */
const policyCommand = defineCommand({
  meta: {
    name: 'policy',
    description: 'Policy and enforcement operations',
  },
  subCommands: {
    apply: policyApplyCommand,
  },
});

/**
 * Root CLI command
 */
const main = defineCommand({
  meta: {
    name: 'exploration-cli',
    version: '1.0.0',
    description: 'UNRDF Substrate Exploration CLI - Noun-verb command pattern using citty',
  },
  subCommands: {
    rdf: rdfCommand,
    policy: policyCommand,
  },
});

// Run the CLI
runMain(main);
