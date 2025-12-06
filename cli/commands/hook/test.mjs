/**
 * @file Hook Test Command - THREE-TIER ARCHITECTURE
 * @architecture CLI → Domain Service → Package
 *
 * Provides hook validation dry-run (wouldPassHooks)
 */

import { defineCommand } from 'citty';
import { readFile } from 'node:fs/promises';
import { getHookService } from '../../domain/index.mjs';
import { dataFactory } from '@unrdf/oxigraph';

export const testCommand = defineCommand({
  meta: {
    name: 'test',
    description: 'Test if data would pass hooks (dry-run validation)'
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
    verbose: {
      type: 'boolean',
      description: 'Show which hooks would execute',
      default: false
    }
  },
  async run(ctx) {
    try {
      // PRESENTATION LAYER: Parse CLI arguments and construct quad
      const { trigger, subject, predicate, object: objectValue, graph, verbose } = ctx.args;

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

      console.log(`\n🧪 Testing hooks for trigger: ${trigger}`);
      console.log(`📝 Test quad: <${subject}> <${predicate}> ${objectNode.termType === 'Literal' ? `"${objectValue}"` : `<${objectValue}>`}`);
      if (graph) {
        console.log(`📊 Graph: <${graph}>`);
      }

      // DOMAIN LAYER: Test via service (wouldPass functionality)
      const service = getHookService();
      const result = await service.wouldPass(trigger, quad);

      // PRESENTATION LAYER: Display results
      if (result.wouldPass) {
        console.log(`\n✅ Would PASS: Data would be accepted by all hooks`);
      } else {
        console.log(`\n❌ Would FAIL: Data would be rejected by hooks`);
      }

      if (verbose && result.hookDetails) {
        console.log(`\n📋 Hook Evaluation Details:`);
        result.hookDetails.forEach((hook, idx) => {
          const status = hook.passed ? '✅' : '❌';
          console.log(`   ${idx + 1}. ${status} ${hook.name} (${hook.trigger})`);
          if (!hook.passed && hook.reason) {
            console.log(`      Reason: ${hook.reason}`);
          }
        });
      }

      if (result.applicableHooksCount !== undefined) {
        console.log(`\n📊 Applicable hooks: ${result.applicableHooksCount}`);
      }

      console.log('');

      // Exit with code 1 if would fail (for CI/CD)
      if (!result.wouldPass) {
        process.exit(1);
      }

    } catch (error) {
      console.error(`❌ Hook test failed: ${error.message}`);
      process.exit(1);
    }
  }
});
