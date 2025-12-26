/**
 * AI Code Review Workflow Example
 *
 * Demonstrates a 3-agent LangChain workflow integrated with YAWL:
 * 1. Code Analyzer - Analyzes code for patterns and complexity
 * 2. Security Reviewer - Identifies security vulnerabilities
 * 3. Suggestion Generator - Generates improvement suggestions
 *
 * Each agent uses RDF context from previous steps and stores results
 * as RDF triples for full knowledge graph integration.
 *
 * @example
 * node examples/code-review-workflow.mjs
 */

import { ChatOpenAI } from '@langchain/openai';
import { StringOutputParser } from '@langchain/core/output_parsers';
import { PromptTemplate } from '@langchain/core/prompts';
import { createStore, dataFactory } from '@unrdf/oxigraph';
import { Workflow, WorkflowEngine, TaskStatus } from '@unrdf/yawl';
import { YAWLLangChainAdapter, createLangChainTaskExecutor } from '../src/adapter.mjs';

const { quad, namedNode, literal } = dataFactory;

// =============================================================================
// Mock LangChain Agents (replace with real OpenAI when API key available)
// =============================================================================

/**
 * Create a mock LangChain agent for demonstration
 * In production, use: new ChatOpenAI({ modelName: 'gpt-4', temperature: 0 })
 */
class MockLangChainAgent {
  constructor(name, responseGenerator) {
    this.name = name;
    this.responseGenerator = responseGenerator;
  }

  async invoke(input) {
    // Simulate LLM processing time
    await new Promise(resolve => setTimeout(resolve, 100));
    return {
      output: this.responseGenerator(input),
    };
  }
}

// =============================================================================
// Agent Definitions
// =============================================================================

/**
 * Agent 1: Code Analyzer
 * Analyzes code complexity, patterns, and structure
 */
const codeAnalyzerAgent = new MockLangChainAgent(
  'code-analyzer',
  (input) => {
    const code = input.input || input.code || '';
    const lines = code.split('\n').length;
    const complexity = code.includes('for') || code.includes('while') ? 'medium' : 'low';

    return `Code Analysis Report:
- Total lines: ${lines}
- Cyclomatic complexity: ${complexity}
- Patterns detected: ${code.includes('class') ? 'OOP' : 'Functional'}
- Maintainability score: ${lines < 100 ? '8/10' : '6/10'}
- Code smells: ${code.includes('var') ? 'Legacy var usage detected' : 'None detected'}`;
  }
);

/**
 * Agent 2: Security Reviewer
 * Identifies security vulnerabilities and risks
 */
const securityReviewerAgent = new MockLangChainAgent(
  'security-reviewer',
  (input) => {
    const code = input.input || input.code || '';
    const analysis = input.previousAnalysis || '';

    const vulnerabilities = [];
    if (code.includes('eval(')) vulnerabilities.push('Code injection risk: eval() usage');
    if (code.includes('innerHTML')) vulnerabilities.push('XSS risk: innerHTML usage');
    if (code.includes('http://')) vulnerabilities.push('Insecure protocol: HTTP instead of HTTPS');

    return `Security Review Report:
- Vulnerabilities found: ${vulnerabilities.length}
${vulnerabilities.map(v => `  - ${v}`).join('\n')}
- Previous analysis context: ${analysis.includes('medium') ? 'Medium complexity requires extra scrutiny' : 'Low complexity'}
- Risk level: ${vulnerabilities.length > 0 ? 'HIGH' : 'LOW'}
- Recommendation: ${vulnerabilities.length > 0 ? 'Fix vulnerabilities before deployment' : 'Code passes security review'}`;
  }
);

/**
 * Agent 3: Suggestion Generator
 * Generates actionable improvement suggestions based on analysis and security review
 */
const suggestionGeneratorAgent = new MockLangChainAgent(
  'suggestion-generator',
  (input) => {
    const analysis = input.analysis || '';
    const security = input.security || '';

    const suggestions = [];

    if (analysis.includes('Legacy var')) {
      suggestions.push('Replace var with const/let for better scoping');
    }
    if (security.includes('eval()')) {
      suggestions.push('Remove eval() calls and use safer alternatives');
    }
    if (security.includes('innerHTML')) {
      suggestions.push('Use textContent or sanitize HTML to prevent XSS');
    }
    if (analysis.includes('6/10')) {
      suggestions.push('Break down large functions into smaller, testable units');
    }

    if (suggestions.length === 0) {
      suggestions.push('Code quality is good - consider adding unit tests');
      suggestions.push('Document public API with JSDoc comments');
    }

    return `Improvement Suggestions:
${suggestions.map((s, i) => `${i + 1}. ${s}`).join('\n')}

Priority: ${security.includes('HIGH') ? 'CRITICAL - Security fixes required' : 'NORMAL - Quality improvements'}
Estimated effort: ${suggestions.length * 30} minutes`;
  }
);

// =============================================================================
// RDF Context Setup
// =============================================================================

/**
 * Create RDF store with initial code context
 */
function createCodeContextStore(codeSnippet) {
  const store = createStore();

  // Define code artifact in RDF
  const codeUri = namedNode('http://example.org/code/snippet-1');
  const hasContent = namedNode('http://example.org/hasContent');
  const hasLanguage = namedNode('http://example.org/hasLanguage');

  store.add(quad(codeUri, hasContent, literal(codeSnippet)));
  store.add(quad(codeUri, hasLanguage, literal('javascript')));

  return store;
}

// =============================================================================
// Workflow Definition
// =============================================================================

/**
 * Create AI Code Review Workflow
 */
export async function createCodeReviewWorkflow() {
  // Create adapters for each LangChain agent
  const analyzerAdapter = new YAWLLangChainAdapter({
    taskId: 'analyze-code',
    taskName: 'Code Analysis',
    agent: codeAnalyzerAgent,
    promptTemplate: 'Analyze this code:\n{code}',
    contextQuery: 'SELECT ?code WHERE { ?s <http://example.org/hasContent> ?code }',
    rdfPredicate: 'http://example.org/codeAnalysis',
    timeout: 10000,
  });

  const securityAdapter = new YAWLLangChainAdapter({
    taskId: 'security-review',
    taskName: 'Security Review',
    agent: securityReviewerAgent,
    promptTemplate: 'Review security for code:\n{code}\n\nPrevious analysis:\n{previousAnalysis}',
    contextQuery: `
      SELECT ?code ?analysis WHERE {
        ?s <http://example.org/hasContent> ?code .
        OPTIONAL { ?task <http://example.org/codeAnalysis> ?analysis }
      }
    `,
    rdfPredicate: 'http://example.org/securityReview',
    timeout: 10000,
  });

  const suggestionAdapter = new YAWLLangChainAdapter({
    taskId: 'generate-suggestions',
    taskName: 'Generate Suggestions',
    agent: suggestionGeneratorAgent,
    promptTemplate: 'Generate suggestions based on:\nAnalysis: {analysis}\nSecurity: {security}',
    contextQuery: `
      SELECT ?analysis ?security WHERE {
        OPTIONAL { ?t1 <http://example.org/codeAnalysis> ?analysis }
        OPTIONAL { ?t2 <http://example.org/securityReview> ?security }
      }
    `,
    rdfPredicate: 'http://example.org/suggestions',
    timeout: 10000,
  });

  // Create YAWL workflow with sequential pattern
  const workflow = new Workflow({
    id: 'ai-code-review',
    name: 'AI-Powered Code Review',
    tasks: [
      analyzerAdapter.createTaskDefinition({
        inputConditions: ['start'],
        outputConditions: ['analysis-complete'],
      }),
      securityAdapter.createTaskDefinition({
        inputConditions: ['analysis-complete'],
        outputConditions: ['security-complete'],
      }),
      suggestionAdapter.createTaskDefinition({
        inputConditions: ['security-complete'],
        outputConditions: ['end'],
      }),
    ],
    flows: [
      { from: 'start', to: 'analyze-code' },
      { from: 'analyze-code', to: 'security-review' },
      { from: 'security-review', to: 'generate-suggestions' },
      { from: 'generate-suggestions', to: 'end' },
    ],
  });

  return {
    workflow,
    adapters: {
      analyzer: analyzerAdapter,
      security: securityAdapter,
      suggestions: suggestionAdapter,
    },
  };
}

// =============================================================================
// Execution Example
// =============================================================================

/**
 * Run the code review workflow
 */
async function runCodeReviewExample() {
  console.log('🤖 AI Code Review Workflow - YAWL + LangChain Integration\n');
  console.log('=' .repeat(70));

  // Sample code to review
  const sampleCode = `
function processUserData(userInput) {
  var data = eval(userInput); // Security risk!
  document.getElementById('output').innerHTML = data; // XSS risk!

  for (var i = 0; i < data.length; i++) { // Legacy var
    console.log(data[i]);
  }

  return data;
}
`.trim();

  console.log('\n📄 Code to Review:');
  console.log(sampleCode);
  console.log('\n' + '=' .repeat(70));

  // Create RDF context store
  const rdfStore = createCodeContextStore(sampleCode);

  // Create workflow
  const { workflow, adapters } = await createCodeReviewWorkflow();

  // Create workflow engine
  const engine = new WorkflowEngine();
  engine.registerWorkflow(workflow);

  // Create case
  const workflowCase = engine.createCase('ai-code-review', {
    rdfStore,
    code: sampleCode,
  });

  console.log('\n🚀 Starting AI Code Review Workflow...\n');

  // Execute each task sequentially
  const tasks = ['analyze-code', 'security-review', 'generate-suggestions'];

  for (const taskId of tasks) {
    console.log(`\n▶️  Executing: ${taskId}`);
    console.log('-'.repeat(70));

    // Get enabled work items for this task
    const workItems = workflowCase.getWorkItemsForTask(taskId);
    const workItem = workItems[0];

    if (workItem) {
      // Start task
      await workItem.start('langchain-system');

      // Execute via adapter
      let result;
      if (taskId === 'analyze-code') {
        workItem.setInputData({ code: sampleCode, rdfStore });
        result = await adapters.analyzer.execute(workItem, { rdfStore });
      } else if (taskId === 'security-review') {
        const prevOutput = adapters.analyzer.getExecutionHistory()[0]?.output || '';
        workItem.setInputData({ code: sampleCode, previousAnalysis: prevOutput, rdfStore });
        result = await adapters.security.execute(workItem, { rdfStore });
      } else {
        const analysisOutput = adapters.analyzer.getExecutionHistory()[0]?.output || '';
        const securityOutput = adapters.security.getExecutionHistory()[0]?.output || '';
        workItem.setInputData({ analysis: analysisOutput, security: securityOutput, rdfStore });
        result = await adapters.suggestions.execute(workItem, { rdfStore });
      }

      // Complete task
      await workItem.complete(result);

      console.log(`\n✅ Output:\n${result.output}`);
      console.log(`\n📊 Metadata: ${result.rdfTriples.length} RDF triples stored`);
      console.log(`⏱️  Execution time: ${Number(result.executionTime) / 1_000_000}ms`);
    }
  }

  console.log('\n' + '=' .repeat(70));
  console.log('\n🎉 Code Review Complete!\n');

  // Show RDF graph summary
  console.log('📈 RDF Knowledge Graph Summary:');
  console.log(`   - Total triples stored: ${[...adapters.analyzer.getRDFStore().quads()].length +
    [...adapters.security.getRDFStore().quads()].length +
    [...adapters.suggestions.getRDFStore().quads()].length}`);
  console.log(`   - Tasks executed: ${tasks.length}`);
  console.log(`   - Workflow status: ${workflowCase.isComplete() ? 'COMPLETED' : 'IN PROGRESS'}`);

  return {
    workflowCase,
    adapters,
    results: {
      analysis: adapters.analyzer.getExecutionHistory()[0],
      security: adapters.security.getExecutionHistory()[0],
      suggestions: adapters.suggestions.getExecutionHistory()[0],
    },
  };
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  runCodeReviewExample().catch(console.error);
}

export { runCodeReviewExample };
