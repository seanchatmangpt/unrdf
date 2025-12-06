/**
 * @file Hook Execute Command - THREE-TIER ARCHITECTURE
 * @architecture CLI → Domain Service → Package
 *
 * Provides full access to hook execution for testing and validation
 */

import { defineCommand } from 'citty';
import { readFile } from 'node:fs/promises';
import { getHookService } from '../../domain/index.mjs';
import { dataFactory } from '@unrdf/oxigraph';

export const executeCommand = defineCommand({
  meta: {
    name: 'execute',
    description: 'Execute hooks on test data'
  },
  args: {
    trigger: {
      type: 'positional',
      description: 'Trigger event (before-add, after-add, etc.)',
      required: true
    },
    subject: {
      type: 'string',
      description: 'Subject IRI',
      required: true
    },
    predicate: {
      type: 'string',
      description: 'Predicate IRI',
      required: true
    },
    object: {
      type: 'string',
      description: 'Object IRI or literal value',
      required: true
    },
    graph: {
      type: 'string',
      description: 'Graph IRI (default graph if omitted)'
    },
    'dry-run': {
      type: 'boolean',
      description: 'Validate only, do not execute effects',
      default: false
    },
    verbose: {
      type: 'boolean',
      description: 'Show detailed execution information',
      default: false
    }
  },
  async run(ctx) {
    try {
      // PRESENTATION LAYER: Parse CLI arguments and construct quad
      const { trigger, subject, predicate, object: objectValue, graph } = ctx.args;

      // Create RDF quad from arguments
      const subjectNode = dataFactory.namedNode(subject);
      const predicateNode = dataFactory.namedNode(predicate);

      // Determine if object is literal or IRI
      let objectNode;
      if (objectValue.startsWith('http://') || objectValue.startsWith('https://')) {
        objectNode = dataFactory.namedNode(objectValue);
      } else {
        objectNode = dataFactory.literal(objectValue);
      }

      const graphNode = graph ? dataFactory.namedNode(graph) : dataFactory.defaultGraph();
      const quad = dataFactory.quad(subjectNode, predicateNode, objectNode, graphNode);

      console.log(`\n🔄 Executing hooks for trigger: ${trigger}`);
      console.log(`📝 Quad: <${subject}> <${predicate}> ${objectNode.termType === 'Literal' ? `"${objectValue}"` : `<${objectValue}>`}`);
      if (graph) {
        console.log(`📊 Graph: <${graph}>`);
      }

      // DOMAIN LAYER: Execute hooks via service
      const service = getHookService();
      const result = await service.executeByTrigger({
        trigger,
        data: quad,
        context: {
          dryRun: ctx.args['dry-run'],
          verbose: ctx.args.verbose
        }
      });

      // PRESENTATION LAYER: Display results
      console.log(`\n✅ Hook execution completed:`);
      console.log(`   Hooks executed: ${result.hooksExecuted || 0}`);

      if (result.passed !== undefined) {
        console.log(`   Validation: ${result.passed ? '✅ PASSED' : '❌ FAILED'}`);
      }

      if (result.transformed) {
        console.log(`   Transformation: Applied`);
        console.log(`   Modified quad: <${result.data.subject.value}> <${result.data.predicate.value}> ${result.data.object.termType === 'Literal' ? `"${result.data.object.value}"` : `<${result.data.object.value}>`}`);
      }

      if (ctx.args.verbose && result.executionDetails) {
        console.log(`\n📋 Execution Details:`);
        result.executionDetails.forEach((detail, idx) => {
          console.log(`   ${idx + 1}. ${detail.hookName}: ${detail.result} (${detail.duration}ms)`);
        });
      }

      console.log('');

    } catch (error) {
      console.error(`❌ Hook execution failed: ${error.message}`);
      process.exit(1);
    }
  }
});
