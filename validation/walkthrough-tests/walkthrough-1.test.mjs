#!/usr/bin/env node
/**
 * Walkthrough 1: Building Your First Knowledge Graph Application
 *
 * Tests the complete execution of walkthrough 1
 */

import { mkdir, writeFile, rm } from 'fs/promises';
import { spawn } from 'child_process';
import { join } from 'path';
import { tmpdir } from 'os';

const TEST_DIR = join(tmpdir(), `walkthrough-1-test-${Date.now()}`);

/**
 * Execute a command and return output
 */
function exec(command, args = [], cwd = TEST_DIR) {
  return new Promise((resolve, reject) => {
    const proc = spawn(command, args, { cwd, shell: true });
    let stdout = '';
    let stderr = '';

    proc.stdout.on('data', (data) => { stdout += data.toString(); });
    proc.stderr.on('data', (data) => { stderr += data.toString(); });

    proc.on('close', (code) => {
      if (code !== 0) {
        reject(new Error(`Command failed: ${command} ${args.join(' ')}\n${stderr}`));
      } else {
        resolve({ stdout, stderr });
      }
    });

    // Timeout after 30s
    setTimeout(() => {
      proc.kill();
      reject(new Error('Command timeout'));
    }, 30000);
  });
}

async function test() {
  console.log('Setting up test directory...');
  await mkdir(TEST_DIR, { recursive: true });

  try {
    // Step 1: Create package.json
    console.log('Step 1: Creating package.json...');
    const packageJson = {
      name: 'walkthrough-1-test',
      version: '1.0.0',
      type: 'module',
      private: true
    };
    await writeFile(join(TEST_DIR, 'package.json'), JSON.stringify(packageJson, null, 2));

    // Step 2: Install @unrdf/core (use local version)
    console.log('Step 2: Installing dependencies...');
    const unrdfCorePath = join(process.cwd(), '../../packages/core');
    await exec('npm', ['install', unrdfCorePath]);

    // Step 3: Create knowledge-base.mjs
    console.log('Step 3: Creating knowledge-base.mjs...');
    const knowledgeBase = `import { createKnowledgeSubstrateCore } from '@unrdf/core';

// Initialize the core engine
const core = await createKnowledgeSubstrateCore();

// Define people and relationships in Turtle format
const data = \`
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .
  @prefix schema: <https://schema.org/> .

  # People
  ex:Alice a foaf:Person ;
    foaf:name "Alice Johnson" ;
    foaf:age 30 ;
    schema:jobTitle "Software Engineer" ;
    foaf:knows ex:Bob, ex:Charlie .

  ex:Bob a foaf:Person ;
    foaf:name "Bob Smith" ;
    foaf:age 28 ;
    schema:jobTitle "Data Scientist" ;
    foaf:knows ex:Alice, ex:Diana .

  ex:Charlie a foaf:Person ;
    foaf:name "Charlie Brown" ;
    foaf:age 35 ;
    schema:jobTitle "Product Manager" ;
    foaf:knows ex:Alice .

  ex:Diana a foaf:Person ;
    foaf:name "Diana Martinez" ;
    foaf:age 32 ;
    schema:jobTitle "UX Designer" ;
    foaf:knows ex:Bob .
\`;

// Parse the data into a store
const store = core.parseRdf(data);

console.log(\`Loaded \${store.size} triples into the knowledge graph\`);

export { core, store };
`;
    await writeFile(join(TEST_DIR, 'knowledge-base.mjs'), knowledgeBase);

    // Step 4: Create queries.mjs
    console.log('Step 4: Creating queries.mjs...');
    const queries = `import { core, store } from './knowledge-base.mjs';

/**
 * Find all people in the knowledge graph
 */
export async function findAllPeople() {
  const sparql = \`
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>

    SELECT ?name ?age ?job
    WHERE {
      ?person a foaf:Person ;
              foaf:name ?name ;
              foaf:age ?age .
      OPTIONAL { ?person <https://schema.org/jobTitle> ?job }
    }
    ORDER BY ?name
  \`;

  const results = await core.query(store, sparql);

  console.log('\\nAll People:');
  console.log('='.repeat(60));
  for (const row of results) {
    const name = row.get('name')?.value;
    const age = row.get('age')?.value;
    const job = row.get('job')?.value || 'Unknown';
    console.log(\`\${name}, \${age} years old - \${job}\`);
  }
}

/**
 * Find who knows whom
 */
export async function findRelationships() {
  const sparql = \`
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>

    SELECT ?person1Name ?person2Name
    WHERE {
      ?person1 foaf:name ?person1Name ;
               foaf:knows ?person2 .
      ?person2 foaf:name ?person2Name .
    }
    ORDER BY ?person1Name
  \`;

  const results = await core.query(store, sparql);

  console.log('\\nRelationships:');
  console.log('='.repeat(60));
  for (const row of results) {
    const p1 = row.get('person1Name')?.value;
    const p2 = row.get('person2Name')?.value;
    console.log(\`\${p1} knows \${p2}\`);
  }
}

/**
 * Find people by job title
 */
export async function findByJob(jobTitle) {
  const sparql = \`
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    PREFIX schema: <https://schema.org/>

    SELECT ?name ?age
    WHERE {
      ?person a foaf:Person ;
              foaf:name ?name ;
              foaf:age ?age ;
              schema:jobTitle ?job .
      FILTER (CONTAINS(LCASE(?job), LCASE("\${jobTitle}")))
    }
  \`;

  const results = await core.query(store, sparql);

  console.log(\`\\nPeople with "\${jobTitle}" in their job title:\`);
  console.log('='.repeat(60));
  for (const row of results) {
    const name = row.get('name')?.value;
    const age = row.get('age')?.value;
    console.log(\`\${name}, \${age} years old\`);
  }
}

/**
 * Find mutual connections
 */
export async function findMutualConnections() {
  const sparql = \`
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>

    SELECT ?person1Name ?person2Name
    WHERE {
      ?person1 foaf:name ?person1Name ;
               foaf:knows ?person2 .
      ?person2 foaf:name ?person2Name ;
               foaf:knows ?person1 .
      FILTER (?person1 != ?person2)
    }
  \`;

  const results = await core.query(store, sparql);

  console.log('\\nMutual Connections:');
  console.log('='.repeat(60));
  const seen = new Set();
  for (const row of results) {
    const p1 = row.get('person1Name')?.value;
    const p2 = row.get('person2Name')?.value;
    const key = [p1, p2].sort().join('-');
    if (!seen.has(key)) {
      console.log(\`\${p1} ↔ \${p2}\`);
      seen.add(key);
    }
  }
}
`;
    await writeFile(join(TEST_DIR, 'queries.mjs'), queries);

    // Step 5: Create app.mjs
    console.log('Step 5: Creating app.mjs...');
    const app = `import { findAllPeople, findRelationships, findByJob, findMutualConnections } from './queries.mjs';

async function main() {
  console.log('Knowledge Graph Application');
  console.log('='.repeat(60));

  await findAllPeople();
  await findRelationships();
  await findByJob('Engineer');
  await findMutualConnections();
}

main().catch(console.error);
`;
    await writeFile(join(TEST_DIR, 'app.mjs'), app);

    // Step 6: Run the application
    console.log('Step 6: Running application...');
    const { stdout } = await exec('node', ['app.mjs']);

    // Verify output
    console.log('Step 7: Verifying output...');
    const expectedPatterns = [
      'Knowledge Graph Application',
      'Loaded 16 triples',
      'Alice Johnson, 30 years old',
      'Bob Smith, 28 years old',
      'Charlie Brown, 35 years old',
      'Diana Martinez, 32 years old',
      'Alice Johnson knows Bob Smith',
      'People with "Engineer" in their job title',
      'Mutual Connections',
      'Alice Johnson ↔ Bob Smith'
    ];

    for (const pattern of expectedPatterns) {
      if (!stdout.includes(pattern)) {
        throw new Error(`Expected pattern not found in output: "${pattern}"`);
      }
    }

    console.log('✅ All validations passed');
    console.log('\nActual output:');
    console.log(stdout);

  } finally {
    // Cleanup
    console.log('Cleaning up...');
    await rm(TEST_DIR, { recursive: true, force: true });
  }
}

test()
  .then(() => {
    console.log('\n✅ Walkthrough 1 validation PASSED');
    process.exit(0);
  })
  .catch((error) => {
    console.error('\n❌ Walkthrough 1 validation FAILED');
    console.error(error);
    process.exit(1);
  });
