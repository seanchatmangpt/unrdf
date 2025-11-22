# Agent Integration Guide

> How autonomous agents use the Playground CLI

```json-ld
{
  "@context": {
    "agent": "urn:playground:agents:",
    "cap": "urn:playground:capabilities:",
    "proto": "urn:playground:protocols:"
  },
  "@id": "urn:playground:agent-integration:v1.0.0",
  "@type": "agent:IntegrationGuide"
}
```

---

## Agent Discovery Protocol

### Step 1: Capability Discovery

Agents discover available capabilities via introspection:

```bash
playground meta introspect --format json
```

**Response:**
```json
{
  "commands": {
    "papers": {
      "subcommands": {
        "generate": {...},
        "list": {...},
        "validate": {...}
      }
    },
    "thesis": {...},
    "config": {...},
    "meta": {...}
  }
}
```

### Step 2: SPARQL-Based Discovery

```sparql
PREFIX cap: <urn:playground:capabilities:>

SELECT ?capability ?name ?description ?preconditions
WHERE {
  ?capability a cap:Capability ;
              rdfs:label ?name ;
              rdfs:comment ?description .
  OPTIONAL { ?capability cap:preconditions ?preconditions }
}
```

### Step 3: Capability Verification

```sparql
ASK {
  cap:papers-generate a cap:Capability ;
                      cap:version "1.0.0" .
}
```

---

## Capability Matching Algorithm

### Input: Task Description

```json
{
  "task": "Generate IMRAD research paper",
  "requirements": {
    "documentType": "paper",
    "family": "imrad",
    "hasTitle": true,
    "hasAuthor": true
  }
}
```

### Algorithm

```javascript
function matchCapability(task) {
  // 1. Extract task requirements
  const requirements = task.requirements;

  // 2. Query capability registry
  const capabilities = await queryCapabilities();

  // 3. Score each capability
  const scores = capabilities.map(cap => ({
    capability: cap,
    score: calculateMatchScore(cap, requirements)
  }));

  // 4. Return best match
  return scores.sort((a, b) => b.score - a.score)[0];
}

function calculateMatchScore(capability, requirements) {
  let score = 0;

  // Match document type
  if (capability.id.includes(requirements.documentType)) {
    score += 50;
  }

  // Match required inputs
  const capInputs = capability.inputs.map(i => i.name);
  const reqInputs = Object.keys(requirements).filter(k => requirements[k] === true);

  const matchedInputs = reqInputs.filter(r =>
    capInputs.some(c => c.toLowerCase().includes(r.toLowerCase()))
  );

  score += (matchedInputs.length / reqInputs.length) * 50;

  return score;
}
```

### Match Result

```json
{
  "capability": "cap:papers-generate",
  "score": 95,
  "confidence": 0.95,
  "command": "playground papers generate imrad --title <title> --author <author>"
}
```

---

## Command Invocation Patterns

### Pattern 1: Direct Invocation

```javascript
import { exec } from 'child_process';

async function invokePapersGenerate(args) {
  const command = `playground papers generate ${args.family || 'imrad'} \
    --title "${args.title}" \
    --author "${args.author}" \
    --format json`;

  return new Promise((resolve, reject) => {
    exec(command, (error, stdout, stderr) => {
      if (error) {
        reject({ error: error.message, stderr });
      } else {
        resolve(JSON.parse(stdout));
      }
    });
  });
}
```

### Pattern 2: Programmatic API

```javascript
import { papersCommand } from 'playground/cli/commands/papers.mjs';

async function generatePaper(input) {
  const result = await papersCommand.subCommands.generate.run({
    args: {
      family: input.family || 'imrad',
      title: input.title,
      author: input.author,
      format: 'json'
    }
  });

  return result;
}
```

### Pattern 3: JSON-LD Invocation

```json
{
  "@context": "urn:playground:invocation:",
  "@type": "cap:PapersGenerateInvocation",
  "capability": "cap:papers-generate",
  "arguments": {
    "family": "imrad",
    "title": "Agent-Generated Paper",
    "author": "Autonomous Agent",
    "format": "json"
  }
}
```

### Pattern 4: SPARQL-Driven Invocation

```sparql
PREFIX cap: <urn:playground:capabilities:>
PREFIX inv: <urn:playground:invocation:>

INSERT DATA {
  inv:invocation-001 a cap:Invocation ;
    cap:capability cap:papers-generate ;
    cap:arg [ cap:name "title" ; cap:value "Generated Paper" ] ;
    cap:arg [ cap:name "author" ; cap:value "Agent" ] ;
    cap:status "pending" .
}
```

---

## Result Parsing

### JSON Output Parsing

```javascript
function parseResult(stdout, format = 'json') {
  switch (format) {
    case 'json':
      return JSON.parse(stdout);

    case 'table':
      return parseTableOutput(stdout);

    case 'csv':
      return parseCSVOutput(stdout);

    default:
      return stdout;
  }
}

function parseTableOutput(stdout) {
  const lines = stdout.trim().split('\n');
  // Skip header separator
  const dataLines = lines.filter(l => !l.startsWith('-'));
  // Parse columns
  return dataLines.slice(1).map(line => {
    const cols = line.trim().split(/\s{2,}/);
    return { col1: cols[0], col2: cols[1], col3: cols[2] };
  });
}
```

### Error Parsing

```javascript
function parseError(stderr, exitCode) {
  // Extract error code
  const codeMatch = stderr.match(/E_([A-Z_]+)/);
  const code = codeMatch ? codeMatch[0] : 'E_UNKNOWN';

  // Extract message
  const msgMatch = stderr.match(/Error: (.+)/);
  const message = msgMatch ? msgMatch[1] : stderr;

  return {
    code,
    message,
    exitCode,
    recoverable: exitCode === 1
  };
}
```

### SPARQL Result Binding

```javascript
function bindSPARQLResults(results) {
  return results.bindings.map(binding => {
    const obj = {};
    for (const [key, value] of Object.entries(binding)) {
      obj[key] = value.value;
    }
    return obj;
  });
}
```

---

## Error Recovery Patterns

### Pattern 1: Retry with Backoff

```javascript
async function invokeWithRetry(command, maxRetries = 3) {
  let lastError;

  for (let i = 0; i < maxRetries; i++) {
    try {
      return await exec(command);
    } catch (error) {
      lastError = error;

      // Check if recoverable
      if (error.exitCode !== 1) {
        throw error; // Fatal error
      }

      // Exponential backoff
      await sleep(Math.pow(2, i) * 1000);
    }
  }

  throw lastError;
}
```

### Pattern 2: Error-Driven Correction

```javascript
async function invokeWithCorrection(command, args) {
  try {
    return await exec(command);
  } catch (error) {
    const parsed = parseError(error.stderr, error.exitCode);

    switch (parsed.code) {
      case 'E_UNKNOWN_FAMILY':
        // Query available families
        const families = await exec('playground papers list --format json');
        const validFamily = findClosestMatch(args.family, families);
        args.family = validFamily;
        return await invokeWithCorrection(buildCommand(args), args);

      case 'E_VALIDATION':
        // Check missing required args
        if (error.message.includes('title')) {
          args.title = await promptForTitle();
        }
        return await invokeWithCorrection(buildCommand(args), args);

      default:
        throw error;
    }
  }
}
```

### Pattern 3: Fallback Capability

```javascript
async function invokeWithFallback(primary, fallback) {
  try {
    return await exec(primary);
  } catch (error) {
    console.log(`Primary failed: ${error.message}`);
    console.log(`Trying fallback...`);
    return await exec(fallback);
  }
}
```

---

## Feedback Loop for Learning

### Execution Telemetry

```json
{
  "@type": "agent:ExecutionTelemetry",
  "invocation": "inv:invocation-001",
  "capability": "cap:papers-generate",
  "startTime": "2025-11-22T00:00:00Z",
  "endTime": "2025-11-22T00:00:00.120Z",
  "duration": 120,
  "success": true,
  "outputSize": 1024,
  "errorCode": null
}
```

### Learning from Success

```javascript
async function recordSuccess(invocation, result) {
  const telemetry = {
    capability: invocation.capability,
    args: invocation.args,
    success: true,
    duration: result.duration,
    timestamp: new Date().toISOString()
  };

  // Store for pattern learning
  await storeTelemetry(telemetry);

  // Update capability confidence
  await updateConfidence(invocation.capability, 0.01);
}
```

### Learning from Failure

```javascript
async function recordFailure(invocation, error) {
  const telemetry = {
    capability: invocation.capability,
    args: invocation.args,
    success: false,
    errorCode: error.code,
    errorMessage: error.message,
    timestamp: new Date().toISOString()
  };

  // Store for pattern learning
  await storeTelemetry(telemetry);

  // Update capability confidence
  await updateConfidence(invocation.capability, -0.05);

  // Record error pattern
  await recordErrorPattern(error.code, invocation.args);
}
```

### Pattern Recognition

```sparql
PREFIX tel: <urn:playground:telemetry:>

SELECT ?errorCode (COUNT(*) AS ?count) ?commonArgs
WHERE {
  ?execution a tel:FailedExecution ;
             tel:errorCode ?errorCode ;
             tel:args ?args .
  BIND(STR(?args) AS ?commonArgs)
}
GROUP BY ?errorCode ?commonArgs
ORDER BY DESC(?count)
LIMIT 10
```

---

## Agent Workflow Example

### Complete Autonomous Workflow

```javascript
class PlaygroundAgent {
  constructor() {
    this.capabilities = null;
    this.confidence = {};
  }

  async initialize() {
    // Discover capabilities
    const result = await exec('playground meta introspect --format json');
    this.capabilities = JSON.parse(result);

    // Initialize confidence scores
    for (const [cmd, data] of Object.entries(this.capabilities.commands)) {
      this.confidence[cmd] = 0.5;
    }
  }

  async executeTask(task) {
    // 1. Match capability
    const match = this.matchCapability(task);

    // 2. Build command
    const command = this.buildCommand(match, task);

    // 3. Execute with retry
    const startTime = Date.now();
    try {
      const result = await this.invokeWithRetry(command);
      const duration = Date.now() - startTime;

      // 4. Record success
      await this.recordSuccess(match.capability, task, duration);

      return result;
    } catch (error) {
      const duration = Date.now() - startTime;

      // 5. Record failure and attempt recovery
      await this.recordFailure(match.capability, error);

      // 6. Try error-driven correction
      return await this.attemptRecovery(task, error);
    }
  }

  matchCapability(task) {
    // Use capability matching algorithm
    return matchCapability(task);
  }

  buildCommand(match, task) {
    // Build CLI command from match
    let cmd = `playground ${match.capability.replace('cap:', '').replace('-', ' ')}`;

    for (const [key, value] of Object.entries(task.args || {})) {
      if (typeof value === 'boolean') {
        if (value) cmd += ` --${key}`;
      } else {
        cmd += ` --${key} "${value}"`;
      }
    }

    return cmd + ' --format json';
  }
}
```

---

## Integration with Claude-Flow

### Agent Spawning

```javascript
// Spawn playground-aware agent via Claude-Flow
await mcp__claude_flow__agent_spawn({
  type: 'specialist',
  name: 'playground-agent',
  capabilities: ['papers-generation', 'thesis-management', 'sparql-query']
});
```

### Task Orchestration

```javascript
// Orchestrate playground task via Claude-Flow
await mcp__claude_flow__task_orchestrate({
  task: 'Generate IMRAD paper with title "AI Research" by "Agent Team"',
  strategy: 'sequential',
  priority: 'high'
});
```

### Memory Integration

```javascript
// Store execution result in Claude-Flow memory
await mcp__claude_flow__memory_usage({
  action: 'store',
  namespace: 'playground',
  key: 'last-paper-generation',
  value: JSON.stringify(result)
});
```
