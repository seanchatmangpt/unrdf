/**
 * @file REPL/Interactive Mode Command
 * @module cli-v2/commands/repl
 *
 * @description
 * Interactive SPARQL query mode with tab completion, history, and color output.
 * Provides an enhanced developer experience for exploring RDF graphs.
 */

import { defineCommand } from 'citty';
import { createInterface } from 'readline';
import { createRequire } from 'module';
import { REPLSession as REPLSessionSafeguards, validateREPLInput } from '../utils/repl-safeguards.mjs';

const require = createRequire(import.meta.url);

// ANSI color codes
const colors = {
  reset: '\x1b[0m',
  bright: '\x1b[1m',
  dim: '\x1b[2m',
  red: '\x1b[31m',
  green: '\x1b[32m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m',
  magenta: '\x1b[35m',
  cyan: '\x1b[36m',
  white: '\x1b[37m'
};

/**
 * Common RDF namespaces for tab completion
 */
const COMMON_NAMESPACES = {
  rdf: 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
  rdfs: 'http://www.w3.org/2000/01/rdf-schema#',
  owl: 'http://www.w3.org/2002/07/owl#',
  xsd: 'http://www.w3.org/2001/XMLSchema#',
  foaf: 'http://xmlns.com/foaf/0.1/',
  dc: 'http://purl.org/dc/elements/1.1/',
  dcterms: 'http://purl.org/dc/terms/',
  skos: 'http://www.w3.org/2004/02/skos/core#',
  unrdf: 'http://unrdf.org/ontology#'
};

/**
 * SPARQL keywords for syntax highlighting
 */
const SPARQL_KEYWORDS = [
  'SELECT', 'CONSTRUCT', 'ASK', 'DESCRIBE',
  'WHERE', 'FROM', 'FILTER', 'OPTIONAL', 'UNION',
  'LIMIT', 'OFFSET', 'ORDER', 'BY', 'GROUP',
  'HAVING', 'BIND', 'VALUES', 'PREFIX', 'BASE'
];

/**
 * REPL Command History Manager
 */
class REPLHistory {
  constructor(maxSize = 1000) {
    this.history = [];
    this.maxSize = maxSize;
    this.position = -1;
  }

  add(command) {
    if (command && command.trim()) {
      this.history.push(command);
      if (this.history.length > this.maxSize) {
        this.history.shift();
      }
      this.position = this.history.length;
    }
  }

  previous() {
    if (this.position > 0) {
      this.position--;
      return this.history[this.position];
    }
    return null;
  }

  next() {
    if (this.position < this.history.length - 1) {
      this.position++;
      return this.history[this.position];
    }
    this.position = this.history.length;
    return '';
  }

  search(pattern) {
    return this.history.filter(cmd => cmd.includes(pattern));
  }
}

/**
 * REPL Tab Completion Provider
 */
class TabCompletion {
  constructor(namespaces = COMMON_NAMESPACES) {
    this.namespaces = namespaces;
    this.keywords = SPARQL_KEYWORDS;
  }

  complete(line) {
    const completions = [];
    const token = this.getLastToken(line);

    // Namespace completion
    for (const [prefix, uri] of Object.entries(this.namespaces)) {
      if (prefix.startsWith(token.toLowerCase())) {
        completions.push(prefix + ':');
      }
    }

    // Keyword completion
    for (const keyword of this.keywords) {
      if (keyword.toLowerCase().startsWith(token.toLowerCase())) {
        completions.push(keyword);
      }
    }

    return completions;
  }

  getLastToken(line) {
    const tokens = line.trim().split(/\s+/);
    return tokens[tokens.length - 1] || '';
  }
}

/**
 * Syntax Highlighter for SPARQL
 */
class SyntaxHighlighter {
  constructor() {
    this.keywords = SPARQL_KEYWORDS;
  }

  highlight(query) {
    let highlighted = query;

    // Highlight keywords
    for (const keyword of this.keywords) {
      const regex = new RegExp(`\\b${keyword}\\b`, 'gi');
      highlighted = highlighted.replace(regex, `${colors.cyan}${keyword}${colors.reset}`);
    }

    // Highlight strings
    highlighted = highlighted.replace(/"([^"]*)"/g, `${colors.green}"$1"${colors.reset}`);
    highlighted = highlighted.replace(/'([^']*)'/g, `${colors.green}'$1'${colors.reset}`);

    // Highlight variables
    highlighted = highlighted.replace(/\?(\w+)/g, `${colors.yellow}?$1${colors.reset}`);

    // Highlight URIs
    highlighted = highlighted.replace(/<([^>]+)>/g, `${colors.blue}<$1>${colors.reset}`);

    // Highlight prefixed names
    highlighted = highlighted.replace(/(\w+):/g, `${colors.magenta}$1:${colors.reset}`);

    return highlighted;
  }
}

/**
 * REPL Session Manager
 */
class REPLSession {
  constructor(ctx, options = {}) {
    this.ctx = ctx;
    this.history = new REPLHistory();
    this.completion = new TabCompletion();
    this.highlighter = new SyntaxHighlighter();
    this.multilineBuffer = [];
    this.inMultilineMode = false;

    // FM-CLI-015: Initialize session safeguards
    this.safeguards = new REPLSessionSafeguards({
      maxBufferSize: options.maxBufferSize || 10 * 1024 * 1024,
      maxQueryTimeout: options.maxQueryTimeout || 30000,
      maxHistoryLines: options.maxHistoryLines || 1000,
      maxConsecutiveErrors: options.maxConsecutiveErrors || 10
    });
  }

  async start() {
    this.printBanner();
    this.printHelp();

    const rl = createInterface({
      input: process.stdin,
      output: process.stdout,
      prompt: this.getPrompt(),
      completer: (line) => {
        const completions = this.completion.complete(line);
        return [completions, line];
      }
    });

    rl.on('line', async (line) => {
      await this.processLine(line, rl);
    });

    rl.on('close', () => {
      this.printGoodbye();
      process.exit(0);
    });

    rl.prompt();
    return rl;
  }

  async processLine(line, rl) {
    const trimmed = line.trim();

    // Handle special commands
    if (trimmed.startsWith('.')) {
      await this.handleCommand(trimmed, rl);
      rl.prompt();
      return;
    }

    // FM-CLI-015: Check buffer size and validate input
    const bufferCheck = this.safeguards.addInput(trimmed);
    if (!bufferCheck.valid) {
      console.error(`${colors.red}${bufferCheck.error}${colors.reset}`);
      console.error(`${colors.yellow}${bufferCheck.suggestion}${colors.reset}`);
      rl.setPrompt(this.getPrompt());
      rl.prompt();
      return;
    }

    // Validate input for dangerous patterns
    const validation = validateREPLInput(trimmed);
    if (validation.warning) {
      console.warn(`${colors.yellow}${validation.warning}${colors.reset}`);
    }

    // Handle multiline input
    if (trimmed.endsWith('\\')) {
      this.multilineBuffer.push(trimmed.slice(0, -1));
      this.inMultilineMode = true;
      rl.setPrompt('... ');
      rl.prompt();
      return;
    }

    // Complete multiline or execute single line
    if (this.inMultilineMode) {
      this.multilineBuffer.push(trimmed);
      const query = this.multilineBuffer.join('\n');
      this.multilineBuffer = [];
      this.inMultilineMode = false;
      await this.executeQuery(query);
    } else if (trimmed) {
      await this.executeQuery(trimmed);
    }

    // Show session diagnostics if unhealthy
    const diagnostics = this.safeguards.getDiagnostics();
    if (!diagnostics.session_healthy) {
      console.warn(`${colors.yellow}⚠️  Session status: ${this.safeguards.getStatus()}${colors.reset}`);
    }

    rl.setPrompt(this.getPrompt());
    rl.prompt();
  }

  async handleCommand(command, rl) {
    const parts = command.slice(1).split(/\s+/);
    const cmd = parts[0];
    const args = parts.slice(1);

    switch (cmd) {
      case 'help':
      case 'h':
        this.printHelp();
        break;

      case 'history':
        this.printHistory();
        break;

      case 'clear':
        console.clear();
        this.printBanner();
        break;

      case 'namespaces':
      case 'ns':
        this.printNamespaces();
        break;

      case 'examples':
        this.printExamples();
        break;

      // FM-CLI-015: Show session diagnostics
      case 'status':
        this.printSessionStatus();
        break;

      case 'exit':
      case 'quit':
        rl.close();
        break;

      default:
        console.log(`${colors.red}Unknown command: ${cmd}${colors.reset}`);
        console.log('Type .help for available commands');
    }
  }

  async executeQuery(query) {
    try {
      console.log(`\n${colors.dim}Executing query...${colors.reset}`);

      // Syntax highlight the query
      console.log(this.highlighter.highlight(query));

      // FM-CLI-015: Execute with timeout protection
      const execResult = await this.safeguards.executeWithTimeout(async () => {
        // FM-006 FIX: Use store-instance directly instead of ctx.invoke()
        const { getStore } = await import('../utils/store-instance.mjs');
        const store = getStore();
        return store.query(query);
      });

      if (!execResult.success) {
        console.log(`${colors.red}Error: ${execResult.error}${colors.reset}`);
        if (execResult.sessionAbnormal) {
          console.log(`${colors.red}${execResult.suggestion}${colors.reset}`);
        }
        return;
      }

      this.printResults(execResult.result);
    } catch (error) {
      console.log(`${colors.red}Error: ${error.message}${colors.reset}`);
    }
  }

  printResults(results) {
    if (!results || results.length === 0) {
      console.log(`${colors.yellow}No results found${colors.reset}\n`);
      return;
    }

    console.log(`\n${colors.green}Results (${results.length} rows):${colors.reset}`);
    console.table(results);
    console.log();
  }

  getPrompt() {
    return `${colors.cyan}unrdf>${colors.reset} `;
  }

  printBanner() {
    console.log(`${colors.bright}${colors.cyan}
╔═══════════════════════════════════════╗
║   UNRDF Interactive SPARQL REPL      ║
║   Version 2.0.0                      ║
╚═══════════════════════════════════════╝${colors.reset}
`);
  }

  printHelp() {
    console.log(`${colors.bright}Commands:${colors.reset}
  ${colors.green}.help${colors.reset}       Show this help message
  ${colors.green}.history${colors.reset}    Show command history
  ${colors.green}.clear${colors.reset}      Clear screen
  ${colors.green}.namespaces${colors.reset} List available namespaces
  ${colors.green}.examples${colors.reset}   Show example queries
  ${colors.green}.status${colors.reset}     Show session diagnostics
  ${colors.green}.exit${colors.reset}       Exit REPL

${colors.bright}Usage:${colors.reset}
  - Type SPARQL queries directly
  - Use \\ at end of line for multiline queries
  - Press TAB for completion
  - Use UP/DOWN arrows for history
  - Ctrl+C to cancel current query
  - Ctrl+D to exit
`);
  }

  printHistory() {
    console.log(`\n${colors.bright}Command History:${colors.reset}`);
    this.history.history.forEach((cmd, i) => {
      console.log(`${colors.dim}${i + 1}.${colors.reset} ${cmd}`);
    });
    console.log();
  }

  printNamespaces() {
    console.log(`\n${colors.bright}Available Namespaces:${colors.reset}`);
    for (const [prefix, uri] of Object.entries(this.completion.namespaces)) {
      console.log(`  ${colors.magenta}${prefix}:${colors.reset} ${colors.dim}${uri}${colors.reset}`);
    }
    console.log();
  }

  printExamples() {
    console.log(`\n${colors.bright}Example Queries:${colors.reset}

${colors.cyan}1. List all triples:${colors.reset}
   SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10

${colors.cyan}2. Find all types:${colors.reset}
   SELECT DISTINCT ?type WHERE {
     ?s rdf:type ?type
   } ORDER BY ?type

${colors.cyan}3. Count triples by predicate:${colors.reset}
   SELECT ?p (COUNT(*) as ?count) WHERE {
     ?s ?p ?o
   } GROUP BY ?p ORDER BY DESC(?count)

${colors.cyan}4. Multiline query (use \\ to continue):${colors.reset}
   PREFIX foaf: <http://xmlns.com/foaf/0.1/> \\
   SELECT ?name ?email WHERE { \\
     ?person foaf:name ?name . \\
     ?person foaf:mbox ?email \\
   }
`);
  }

  printGoodbye() {
    console.log(`\n${colors.cyan}Goodbye!${colors.reset}\n`);
  }

  printSessionStatus() {
    const diag = this.safeguards.getDiagnostics();

    console.log(`\n${colors.bright}Session Diagnostics:${colors.reset}`);
    console.log(`  ${colors.cyan}Duration:${colors.reset} ${(diag.session_duration_ms / 1000).toFixed(1)}s`);
    console.log(`  ${colors.cyan}Buffer:${colors.reset} ${(diag.buffer_size_bytes / 1024).toFixed(1)}KB / ${(diag.buffer_limit_bytes / 1024 / 1024).toFixed(0)}MB`);
    console.log(`  ${colors.cyan}History:${colors.reset} ${diag.history_lines}/${diag.history_limit} lines`);
    console.log(`  ${colors.cyan}Errors:${colors.reset} ${diag.consecutive_errors}/${diag.error_limit}`);
    console.log(`  ${colors.cyan}Timeout:${colors.reset} ${diag.timeout_ms}ms`);

    if (diag.session_healthy) {
      console.log(`  ${colors.green}Status: HEALTHY ✅${colors.reset}\n`);
    } else {
      console.log(`  ${colors.red}Status: DEGRADED ⚠️${colors.reset}`);
      console.log(`  ${colors.yellow}Consider starting a new session${colors.reset}\n`);
    }
  }
}

/**
 * REPL command definition
 */
export const replCommand = defineCommand({
  meta: {
    name: 'repl',
    description: 'Start interactive SPARQL REPL'
  },
  args: {
    endpoint: {
      type: 'string',
      description: 'SPARQL endpoint URL',
      alias: 'e'
    },
    timeout: {
      type: 'string',
      description: 'Query timeout (ms)',
      default: '30000'
    }
  },
  async run(ctx) {
    const session = new REPLSession(ctx);
    await session.start();
  }
});
