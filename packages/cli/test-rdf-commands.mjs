#!/usr/bin/env node
/**
 * Test script for RDF commands
 * Tests the newly implemented graph, query, context, and convert commands
 */

import { graphCommand } from './src/cli/commands/graph.mjs';
import { queryCommand } from './src/cli/commands/query.mjs';
import { contextCommand } from './src/cli/commands/context.mjs';
import { convertCommand } from './src/cli/commands/convert.mjs';
import { writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';

console.log('🧪 Testing RDF Commands\n');

const testDir = join(tmpdir(), 'unrdf-cli-test-' + Date.now());

// Create test data
const testTurtle = `
@prefix ex: <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:Alice a ex:Person ;
  rdfs:label "Alice Smith" ;
  ex:age 30 .

ex:Bob a ex:Person ;
  rdfs:label "Bob Jones" ;
  ex:age 25 .
`;

const testFile = join(testDir, 'test-data.ttl');

async function runTests() {
  try {
    // Create test directory
    await import('fs').then(fs => fs.mkdirSync(testDir, { recursive: true }));
    writeFileSync(testFile, testTurtle, 'utf-8');

    console.log('✅ Test 1: Graph create command');
    await graphCommand.subCommands.create.run({
      args: {
        name: 'test-graph',
        file: join(testDir, 'test-graph.nq'),
      },
    });

    console.log('\n✅ Test 2: Graph load command');
    await graphCommand.subCommands.load.run({
      args: {
        file: testFile,
        graph: 'test-data',
      },
    });

    console.log('\n✅ Test 3: Graph stats command');
    await graphCommand.subCommands.stats.run({
      args: {
        file: testFile,
      },
    });

    console.log('\n✅ Test 4: Context create command');
    await contextCommand.subCommands.create.run({
      args: {
        name: 'my-context',
        output: join(testDir, 'context.jsonld'),
      },
    });

    console.log('\n✅ Test 5: Context add prefix');
    await contextCommand.subCommands.add.run({
      args: {
        file: join(testDir, 'context.jsonld'),
        prefix: 'foaf',
        namespace: 'http://xmlns.com/foaf/0.1/',
      },
    });

    console.log('\n✅ Test 6: Context list prefixes');
    await contextCommand.subCommands.list.run({
      args: {
        file: join(testDir, 'context.jsonld'),
      },
    });

    console.log('\n✅ Test 7: Convert to N-Triples');
    await convertCommand.run({
      args: {
        input: testFile,
        output: join(testDir, 'output.nt'),
        to: 'N-Triples',
      },
    });

    console.log('\n✅ Test 8: Graph dump command');
    await graphCommand.subCommands.dump.run({
      args: {
        file: testFile,
        output: join(testDir, 'dump.ttl'),
        format: 'turtle',
      },
    });

    console.log('\n🎉 All RDF commands working!\n');
    console.log(`📁 Test files in: ${testDir}`);

    return true;
  } catch (error) {
    console.error('\n❌ Test failed:', error.message);
    console.error(error.stack);
    return false;
  }
}

runTests().then(success => {
  process.exit(success ? 0 : 1);
});
