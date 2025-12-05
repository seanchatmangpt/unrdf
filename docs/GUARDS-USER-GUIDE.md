# UNRDF Poka-Yoke Guards - User Guide

**Version**: 2.0.0
**Status**: ✅ Production Ready
**Purpose**: Prevent common errors before they happen

---

## 5 Essential Guard Behaviors

### 1. **Destructive Operations Always Ask For Confirmation**

**What Changed**: Delete operations now require explicit confirmation.

#### Example: Delete a Graph
```bash
$ unrdf graph delete staging-graph

⚠️  This will delete the graph: staging-graph
   Resources dependent: 5 hooks, 3 policies

Are you sure? (type 'yes' to confirm): _
```

**Key Points**:
- ✅ Type `yes` to confirm (not just Enter)
- ✅ Shows what will be deleted
- ✅ Use `--force` flag in scripts: `unrdf graph delete staging-graph --force`
- ✅ Prevents accidental data loss

**Commands Protected**:
- `unrdf graph delete`
- `unrdf hook delete`
- `unrdf context delete`
- `unrdf policy apply`

---

### 2. **Invalid SPARQL Caught Before Execution**

**What Changed**: SPARQL queries are validated before sending to backend.

#### Example: Typo in Query
```bash
$ unrdf store query "SELECT invalid SPARQL"

❌ SPARQL Syntax Errors:
  • Query must start with SELECT, ASK, CONSTRUCT, DESCRIBE, INSERT, or DELETE

📖 Suggestions:
  • Check SPARQL syntax: https://www.w3.org/TR/sparql11-query/
```

**Key Points**:
- ✅ Errors caught immediately, not at backend
- ✅ Clear error messages with suggestions
- ✅ Helps you fix typos before execution
- ✅ Saves time debugging cryptic backend errors

---

### 3. **Policy Files Validated on Apply**

**What Changed**: Policy JSON must match schema before application.

#### Example: Invalid Policy File
```bash
$ unrdf policy apply my-policy.json

❌ Policy schema validation failed

📋 Validation Issues:
   • hooks[0].type: Invalid enum value

📖 Suggestions:
  • Fix the following issues:
    • hooks[0].type: Must be one of: sparql-ask, sparql-select, shacl, custom
```

**Valid Policy Structure**:
```json
{
  "name": "my-policy",
  "version": "1.0.0",
  "hooks": [
    {
      "type": "sparql-ask",
      "name": "my-hook",
      "condition": "optional SPARQL query"
    }
  ],
  "rules": [
    {
      "id": "rule-1",
      "pattern": "pattern-string",
      "action": "allow"
    }
  ]
}
```

**Key Points**:
- ✅ Validation happens BEFORE applying policy
- ✅ Schema errors reported with clear guidance
- ✅ Prevents corrupted policy state

---

### 4. **REPL Session Health Monitoring**

**What Changed**: Interactive REPL sessions now track health and prevent crashes.

#### New Commands:
```bash
unrdf> .status
Session Diagnostics:
  Duration: 3.2s
  Buffer: 245KB / 10MB
  History: 12/1000 lines
  Errors: 2/10
  Timeout: 30000ms
  Status: HEALTHY ✅
```

**Key Points**:
- ✅ Type `.status` to see session health
- ✅ Buffer limits prevent memory overflow
- ✅ Query timeout (30s default) prevents hanging
- ✅ Error tracking detects session degradation
- ✅ Session degrades gracefully, doesn't crash

#### Automatic Protections:
```bash
unrdf> while(true) { }

⚠️  Potential issues detected:
  • Detected infinite while loop - this would hang the REPL

unrdf> SELECT ?s WHERE { ?s ?p ?o }

Query timeout: exceeded 30000ms limit. Consider breaking the query into smaller operations.
```

---

### 5. **File Operations Validate Before Changes**

**What Changed**: Files are checked to exist before operations print success.

#### Example: File Not Found (Old vs New)
```bash
# OLD (confusing):
$ unrdf store import missing.ttl
📥 Importing missing.ttl...
Error: ENOENT: no such file or directory

# NEW (clear):
$ unrdf store import missing.ttl
❌ File not found: missing.ttl
📖 Suggestions:
  • Check the file path
  • Current directory: /home/user/projects/my-graph
  • Files in current directory: data/, src/, test/
```

**Key Points**:
- ✅ File existence checked BEFORE state changes
- ✅ No contradictory "success then error" messages
- ✅ Helpful suggestions with directory listing

---

## Advanced Features

### Context Operations Are Synchronized

**What's Protected**: Concurrent context modifications (e.g., during automation)

```bash
# If two commands try to modify context simultaneously:
$ unrdf context use context-a &
$ unrdf context use context-b &

# Both wait for lock (30s timeout)
# Only one modifies at a time
# No race conditions
```

### Project Initialization Has Rollback

**What's Protected**: Failed project creation rolls back all changes

```bash
$ unrdf init my-project

# If something fails during:
# - Template copying
# - package.json update
# - Config update
# - Git init

# ALL changes are automatically rolled back
✅ Rolled back 4 operations
```

### Path Security Prevents Traversal Attacks

**What's Protected**: File operations can't escape the project directory

```bash
# This is blocked:
$ unrdf store import ../../../../etc/passwd
❌ Path resolves outside base directory - security violation

# This is allowed:
$ unrdf store import ./data/graph.ttl
✅ File imported from /home/user/projects/my-graph/data/graph.ttl
```

---

## Automation & CI/CD

### Using --force Flag

For automated deployments, use `--force` to skip confirmations:

```bash
# Skip confirmation prompts
unrdf graph delete staging-graph --force

# In scripts:
#!/bin/bash
unrdf init my-project --no-git --no-install --force
unrdf store import data/*.ttl --force
```

### Environment Variables

Some guards respect environment variables:

```bash
# Disable timeout (not recommended):
UNRDF_QUERY_TIMEOUT=120000 unrdf repl

# Disable REPL buffer limits (not recommended):
UNRDF_REPL_MAX_BUFFER=52428800 unrdf repl
```

---

## Troubleshooting

### "Query timeout exceeded" in REPL

**Cause**: Query taking >30 seconds

**Solution**:
1. Break query into smaller parts
2. Add LIMIT to reduce results
3. Check `.status` for session health
4. Start new REPL session if degraded

### "Buffer size exceeded"

**Cause**: Too much input in REPL session

**Solution**:
1. Type `.clear` to clear screen
2. History is preserved
3. Start new REPL session if needed

### "Confirmation prompt not appearing"

**Cause**: Running in non-interactive mode (scripts, CI/CD)

**Solution**:
1. Use `--force` flag for automation
2. Or pipe confirmation: `echo yes | unrdf graph delete`

### "Lock timeout"

**Cause**: Another operation holding context lock

**Solution**:
1. Wait for lock to release
2. Default 30-second timeout
3. Kill process if truly stuck (will release lock)

---

## What Guards Protect

| Guard | Prevents | Confidence |
|-------|----------|-----------|
| **SPARQL validation** | Invalid queries at execution | 95% |
| **File checks** | "Success then error" contradictions | 88% |
| **Confirmation prompts** | Accidental data deletion | 95% |
| **Policy schema** | Corrupted policy state | 80% |
| **REPL safeguards** | Session crashes / hangs | 84% |
| **Path security** | Directory traversal attacks | 90% |
| **Context locking** | Race condition corruption | 87% |
| **Init rollback** | Partial project creation | 79% |

---

## Key Takeaway

**Guards are mistake-proofing devices**, like seatbelts:
- ✅ They prevent common errors silently and gracefully
- ✅ They provide clear error messages when something is wrong
- ✅ They never hide what's happening
- ✅ You can override them with `--force` when needed

**Use them in development** to catch errors early.
**Use `--force` in automation** when you're confident.

---

## See Also

- [FMEA Closeout Report](/docs/audit/FMEA-CLOSEOUT-80-20.md) - Technical analysis
- [Guard Implementation Details](/docs/FMEA-POKA-YOKE-IMPLEMENTATION.md) - How guards work
- [Error Message Reference](/docs/GUARD-ERROR-MESSAGES.md) - All error messages
