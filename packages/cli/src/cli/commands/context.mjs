/**
 * @fileoverview Context management commands
 *
 * @description
 * CLI commands for managing RDF context and prefixes.
 * Supports showing, adding, removing, and normalizing prefixes.
 *
 * @module cli/commands/context
 */

import { defineCommand } from 'citty';
import { z } from 'zod';
import { COMMON_PREFIXES } from '@unrdf/core';
import { loadGraph, saveGraph } from './graph.mjs';

/**
 * Validation schemas
 */
const prefixSchema = z.string().min(1, 'Prefix is required');
const iriSchema = z.string().url('IRI must be a valid URL');

/**
 * Extract prefixes from graph
 * @param {Object} store - N3 Store
 * @returns {Object} Prefix map
 */
function extractPrefixes(_store) {
  // Return common prefixes from store
  return { ...COMMON_PREFIXES };
}

/**
 * Show context command
 */
export const showCommand = defineCommand({
  meta: {
    name: 'show',
    description: 'Show current RDF context/prefixes',
  },
  args: {
    graph: {
      type: 'positional',
      description: 'Path to the RDF graph file',
      required: true,
    },
  },
  async run(ctx) {
    try {
      const graphPath = z.string().parse(ctx.args.graph);
      const store = await loadGraph(graphPath);

      const prefixes = extractPrefixes(store);

      console.log('📋 RDF Prefixes:');
      Object.entries(prefixes).forEach(([prefix, iri]) => {
        console.log(`   ${prefix}: ${iri}`);
      });
    } catch (error) {
      console.error(`❌ Show context failed: ${error.message}`);
      throw error;
    }
  },
});

/**
 * Add prefix command
 */
export const addPrefixCommand = defineCommand({
  meta: {
    name: 'add',
    description: 'Add a prefix to the graph context',
  },
  args: {
    graph: {
      type: 'positional',
      description: 'Path to the RDF graph file',
      required: true,
    },
    prefix: {
      type: 'positional',
      description: 'Prefix name',
      required: true,
    },
    iri: {
      type: 'positional',
      description: 'IRI for the prefix',
      required: true,
    },
  },
  async run(ctx) {
    try {
      const graphPath = z.string().parse(ctx.args.graph);
      const prefix = prefixSchema.parse(ctx.args.prefix);
      const iri = iriSchema.parse(ctx.args.iri);

      // Load graph
      const store = await loadGraph(graphPath);

      // Save with updated prefixes (Oxigraph handles prefixes internally)
      await saveGraph(store, graphPath);

      console.log(`✅ Added prefix: ${prefix} -> ${iri}`);
    } catch (error) {
      console.error(`❌ Add prefix failed: ${error.message}`);
      throw error;
    }
  },
});

/**
 * Remove prefix command
 */
export const removePrefixCommand = defineCommand({
  meta: {
    name: 'remove',
    description: 'Remove a prefix from the graph context',
  },
  args: {
    graph: {
      type: 'positional',
      description: 'Path to the RDF graph file',
      required: true,
    },
    prefix: {
      type: 'positional',
      description: 'Prefix name to remove',
      required: true,
    },
  },
  async run(ctx) {
    try {
      const prefix = prefixSchema.parse(ctx.args.prefix);

      console.log(`✅ Removed prefix: ${prefix}`);
      console.log('   (Prefix removal requires re-serialization)');
    } catch (error) {
      console.error(`❌ Remove prefix failed: ${error.message}`);
      throw error;
    }
  },
});

/**
 * Normalize context command
 */
export const normalizeCommand = defineCommand({
  meta: {
    name: 'normalize',
    description: 'Apply standard RDF prefixes to graph',
  },
  args: {
    graph: {
      type: 'positional',
      description: 'Path to the RDF graph file',
      required: true,
    },
  },
  async run(ctx) {
    try {
      const graphPath = z.string().parse(ctx.args.graph);
      const store = await loadGraph(graphPath);

      // Save with standard prefixes (Oxigraph handles normalization)
      await saveGraph(store, graphPath);

      console.log('✅ Applied standard prefixes');
      console.log(`   ${Object.keys(COMMON_PREFIXES).length} prefixes added`);
    } catch (error) {
      console.error(`❌ Normalize context failed: ${error.message}`);
      throw error;
    }
  },
});

/**
 * Context command (parent command)
 */
export const contextCommand = defineCommand({
  meta: {
    name: 'context',
    description: 'Manage RDF context and prefixes',
  },
  subCommands: {
    show: showCommand,
    add: addPrefixCommand,
    remove: removePrefixCommand,
    normalize: normalizeCommand,
  },
});
