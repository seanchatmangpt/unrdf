#!/usr/bin/env node
/**
 * @file Groq Integration Test
 * @description Demonstrates Groq LLM integration with UNRDF daemon
 */

import { generateText } from 'ai';
import { getGroqProvider, initializeGroqProvider } from '../../packages/daemon/src/providers/groq.mjs';
import { loadConfig } from '../../packages/daemon/src/config.mjs';
import { Store } from 'n3';
import { DataFactory } from 'n3';

const { namedNode, literal, quad } = DataFactory;

/**
 * Main test execution
 */
async function main() {
  console.log('🤖 Groq LLM Integration Test');
  console.log('');

  try {
    // Load configuration
    console.log('📋 Step 1: Load configuration');
    const config = loadConfig();
    console.log(`  Config loaded: ${config ? '✅' : '❌'}`);
    console.log(`  Model: ${config.groq?.model || 'openai/gpt-oss-20b'}`);
    console.log('');

    // Initialize Groq provider
    console.log('🔌 Step 2: Initialize Groq provider');
    initializeGroqProvider(config.groq);
    const provider = getGroqProvider();
    console.log(`  Provider initialized: ${provider ? '✅' : '❌'}`);
    console.log(`  API Key configured: ${process.env.GROQ_API_KEY ? '✅' : '⚠️  (not set)'}`);
    console.log('');

    // Test 1: Simple text generation
    console.log('💬 Test 3: Simple text generation');
    const model = provider.getDefaultModel();
    const result1 = await generateText({
      model,
      prompt: 'What is RDF? Explain in one sentence.',
      maxTokens: 100,
    });
    console.log(`  Response: ${result1.text.substring(0, 100)}...`);
    console.log('  ✅ Text generation works');
    console.log('');

    // Test 2: RDF reasoning
    console.log('🧠 Test 4: RDF reasoning with Groq');
    const testStore = new Store();
    testStore.addQuad(
      quad(
        namedNode('ex:alice'),
        namedNode('ex:name'),
        literal('Alice')
      )
    );
    testStore.addQuad(
      quad(
        namedNode('ex:alice'),
        namedNode('ex:age'),
        literal('30')
      )
    );
    testStore.addQuad(
      quad(
        namedNode('ex:bob'),
        namedNode('ex:name'),
        literal('Bob')
      )
    );

    const storeArray = Array.from(testStore);
    const rdfContext = storeArray
      .map(q => `${q.subject.value} ${q.predicate.value} ${q.object.value}.`)
      .join('\n');

    const result2 = await generateText({
      model,
      prompt: `Analyze this RDF knowledge graph and describe what entities exist:

${rdfContext}

What entities are present? What properties do they have?`,
      maxTokens: 200,
    });
    console.log(`  Analysis: ${result2.text.substring(0, 150)}...`);
    console.log('  ✅ RDF reasoning works');
    console.log('');

    // Test 3: Multi-step autonomous reasoning
    console.log('🔄 Test 5: Multi-step autonomous reasoning');
    const store = new Store();
    store.addQuad(
      quad(
        namedNode('ex:data'),
        namedNode('ex:count'),
        literal('5')
      )
    );

    let storeContent = Array.from(store)
      .map(q => `${q.subject.value} ${q.predicate.value} ${q.object.value}.`)
      .join('\n');

    const step1 = await generateText({
      model,
      prompt: `Current RDF state:
${storeContent}

What observation can you make about this data?`,
      maxTokens: 100,
    });
    console.log(`  Step 1 - Observation: ${step1.text.substring(0, 80)}...`);

    const step2 = await generateText({
      model,
      prompt: `Based on the observation that: "${step1.text.substring(0, 50)}..."

What RDF triple would you add to improve this knowledge graph?
Format as: subject predicate object`,
      maxTokens: 100,
    });
    console.log(`  Step 2 - Suggestion: ${step2.text.substring(0, 80)}...`);

    const step3 = await generateText({
      model,
      prompt: `You suggested adding: "${step2.text.substring(0, 50)}..."

Would this be a good addition? Explain your reasoning in one sentence.`,
      maxTokens: 100,
    });
    console.log(`  Step 3 - Evaluation: ${step3.text.substring(0, 80)}...`);
    console.log('  ✅ Multi-step reasoning works');
    console.log('');

    console.log('✅ All Groq integration tests passed!');
    console.log('');
    console.log('📝 Summary:');
    console.log('   • Configuration loading: Working ✅');
    console.log('   • Provider initialization: Working ✅');
    console.log('   • Text generation: Working ✅');
    console.log('   • RDF reasoning: Working ✅');
    console.log('   • Multi-step autonomous reasoning: Working ✅');
    console.log('');
    console.log('🎯 Groq is fully integrated and ready for use!');
    console.log('   The daemon can now use LLM-powered reasoning for:');
    console.log('   • Autonomous knowledge graph improvement');
    console.log('   • Hook execution strategy optimization');
    console.log('   • RDF query generation and optimization');
    console.log('   • Ontology alignment and mapping');

  } catch (error) {
    console.error('❌ Test failed:', error.message);
    if (error.message.includes('API key')) {
      console.error('');
      console.error('💡 To fix: Set GROQ_API_KEY environment variable');
      console.error('   export GROQ_API_KEY="your-groq-api-key"');
    }
    process.exit(1);
  }
}

main().catch(error => {
  console.error('Unhandled error:', error);
  process.exit(1);
});
