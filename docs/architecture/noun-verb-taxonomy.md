# UNRDF Enterprise CLI Noun-Verb Taxonomy

## Executive Summary

This document defines the complete command taxonomy for UNRDF's enterprise-grade CLI, following industry-standard noun-verb patterns similar to `kubectl`, `aws-cli`, and `docker`. The taxonomy is designed to scale from development to production with deep KGC sidecar integration.

## Design Philosophy

### Noun-First Organization
- **Resource-centric**: Commands operate on domain resources (graphs, hooks, policies)
- **Composability**: Nouns combine naturally with verbs for intuitive workflows
- **Discoverability**: `unrdf <noun> --help` reveals all operations for that resource
- **Consistency**: Same verbs across all nouns where applicable

### Industry Pattern Alignment

| CLI | Pattern | UNRDF Equivalent |
|-----|---------|------------------|
| `kubectl get pods` | `kubectl <verb> <noun>` | Alternative pattern |
| `aws s3 cp` | `aws <noun> <verb>` | **Primary pattern** |
| `docker container run` | `docker <noun> <verb>` | **Primary pattern** |

**UNRDF adopts**: `unrdf <noun> <verb> [args...]` pattern for clarity.

## Resource Nouns (Primary)

### 1. graph (Knowledge Graphs)
**Value Weight**: 25%
**Description**: RDF graph resources for knowledge representation

**Operations**:
- `unrdf graph list` - List all graphs
- `unrdf graph get <name>` - Get graph details
- `unrdf graph create <name>` - Create new graph
- `unrdf graph delete <name>` - Delete graph
- `unrdf graph import <file>` - Import data
- `unrdf graph export <name>` - Export graph
- `unrdf graph validate <name>` - Validate graph
- `unrdf graph merge <src> <dst>` - Merge graphs
- `unrdf graph diff <g1> <g2>` - Compare graphs
- `unrdf graph stats <name>` - Show statistics

**Aliases**: `g`, `graphs`

### 2. hook (Knowledge Hooks)
**Value Weight**: 20%
**Description**: Reactive knowledge hooks for policy enforcement

**Operations**:
- `unrdf hook list` - List hooks
- `unrdf hook get <id>` - Get hook details
- `unrdf hook create <name> <type>` - Create hook
- `unrdf hook update <id>` - Update hook
- `unrdf hook delete <id>` - Delete hook
- `unrdf hook eval <id> --data=<file>` - Evaluate hook
- `unrdf hook validate <id>` - Validate hook
- `unrdf hook history <id>` - Show execution history
- `unrdf hook enable <id>` - Enable hook
- `unrdf hook disable <id>` - Disable hook
- `unrdf hook test <id>` - Test hook

**Aliases**: `h`, `hooks`

### 3. policy (Policy Packs)
**Value Weight**: 15%
**Description**: Governance policy packs for compliance

**Operations**:
- `unrdf policy list` - List policy packs
- `unrdf policy get <id>` - Get policy details
- `unrdf policy apply <file>` - Apply policy pack
- `unrdf policy delete <id>` - Delete policy
- `unrdf policy validate <file>` - Validate policy
- `unrdf policy test <file> --dry-run` - Test policy
- `unrdf policy activate <id>` - Activate policy
- `unrdf policy deactivate <id>` - Deactivate policy
- `unrdf policy diff <p1> <p2>` - Compare policies
- `unrdf policy export <id>` - Export policy

**Aliases**: `p`, `pol`, `policies`

### 4. sidecar (KGC Sidecar)
**Value Weight**: 12%
**Description**: KGC sidecar management and monitoring

**Operations**:
- `unrdf sidecar status` - Check sidecar status
- `unrdf sidecar start` - Start sidecar
- `unrdf sidecar stop` - Stop sidecar
- `unrdf sidecar restart` - Restart sidecar
- `unrdf sidecar logs [--follow]` - View logs
- `unrdf sidecar config get` - Get configuration
- `unrdf sidecar config set <key>=<val>` - Set config
- `unrdf sidecar health` - Health check
- `unrdf sidecar metrics` - Show metrics
- `unrdf sidecar version` - Show version

**Aliases**: `sc`, `side`

### 5. store (Triple Store)
**Value Weight**: 10%
**Description**: RDF triple store operations

**Operations**:
- `unrdf store stats` - Show statistics
- `unrdf store import <file>` - Import data
- `unrdf store export <output>` - Export data
- `unrdf store query <sparql>` - Execute query
- `unrdf store clear` - Clear store
- `unrdf store backup <file>` - Create backup
- `unrdf store restore <file>` - Restore backup
- `unrdf store validate` - Validate store
- `unrdf store optimize` - Optimize storage
- `unrdf store compact` - Compact store

**Aliases**: `s`, `storage`

### 6. context (CLI Contexts)
**Value Weight**: 8%
**Description**: CLI context management (like kubeconfig)

**Operations**:
- `unrdf context list` - List contexts
- `unrdf context get <name>` - Get context
- `unrdf context use <name>` - Switch context
- `unrdf context create <name>` - Create context
- `unrdf context delete <name>` - Delete context
- `unrdf context current` - Show current
- `unrdf context rename <old> <new>` - Rename
- `unrdf context set <key>=<val>` - Set property

**Aliases**: `ctx`, `contexts`

### 7. transaction (Transactions)
**Value Weight**: 5%
**Description**: Transaction management and audit

**Operations**:
- `unrdf transaction list` - List transactions
- `unrdf transaction get <id>` - Get transaction
- `unrdf transaction history` - Show history
- `unrdf transaction rollback <id>` - Rollback
- `unrdf transaction verify <id>` - Verify integrity
- `unrdf transaction export <id>` - Export receipt
- `unrdf transaction stats` - Show statistics

**Aliases**: `tx`, `txn`, `transactions`

### 8. lockchain (Audit Trail)
**Value Weight**: 3%
**Description**: Cryptographic audit trail

**Operations**:
- `unrdf lockchain list` - List receipts
- `unrdf lockchain get <id>` - Get receipt
- `unrdf lockchain verify` - Verify chain
- `unrdf lockchain export` - Export chain
- `unrdf lockchain stats` - Show statistics
- `unrdf lockchain audit` - Generate audit

**Aliases**: `lc`, `chain`

### 9. config (Configuration)
**Value Weight**: 2%
**Description**: Global configuration management

**Operations**:
- `unrdf config get <key>` - Get config value
- `unrdf config set <key>=<val>` - Set config
- `unrdf config list` - List all config
- `unrdf config unset <key>` - Unset value
- `unrdf config edit` - Edit config file
- `unrdf config view` - View config
- `unrdf config reset` - Reset to defaults

**Aliases**: `cfg`

## Action Verbs (Standard)

### Primary Verbs (CRUD + Execute)
| Verb | Description | Applies To | Example |
|------|-------------|------------|---------|
| `list` | List resources | All nouns | `unrdf graph list` |
| `get` | Get resource details | All nouns | `unrdf hook get hook-1` |
| `create` | Create new resource | Most nouns | `unrdf policy create p1` |
| `update` | Update resource | Most nouns | `unrdf hook update h1` |
| `delete` | Delete resource | All nouns | `unrdf graph delete g1` |
| `apply` | Apply from file | graph, policy | `unrdf policy apply rules.json` |
| `describe` | Detailed description | All nouns | `unrdf sidecar describe` |

### Operational Verbs
| Verb | Description | Applies To | Example |
|------|-------------|------------|---------|
| `start` | Start service | sidecar | `unrdf sidecar start` |
| `stop` | Stop service | sidecar | `unrdf sidecar stop` |
| `restart` | Restart service | sidecar | `unrdf sidecar restart` |
| `enable` | Enable resource | hook, policy | `unrdf hook enable h1` |
| `disable` | Disable resource | hook, policy | `unrdf policy disable p1` |
| `activate` | Activate resource | policy | `unrdf policy activate p1` |
| `deactivate` | Deactivate resource | policy | `unrdf policy deactivate p1` |

### Query Verbs
| Verb | Description | Applies To | Example |
|------|-------------|------------|---------|
| `query` | Execute query | store | `unrdf store query "SELECT..."` |
| `search` | Search resources | graph, hook | `unrdf graph search "term"` |
| `find` | Find resources | All nouns | `unrdf hook find --name=x` |
| `filter` | Filter list | All nouns | `unrdf policy list --filter=active` |

### Transformation Verbs
| Verb | Description | Applies To | Example |
|------|-------------|------------|---------|
| `import` | Import data | graph, store | `unrdf graph import data.ttl` |
| `export` | Export data | All nouns | `unrdf store export out.nq` |
| `convert` | Convert format | graph | `unrdf graph convert --to=turtle` |
| `merge` | Merge resources | graph | `unrdf graph merge g1 g2` |
| `diff` | Compare resources | graph, policy | `unrdf graph diff g1 g2` |
| `patch` | Apply patch | graph | `unrdf graph patch delta.json` |

### Validation Verbs
| Verb | Description | Applies To | Example |
|------|-------------|------------|---------|
| `validate` | Validate resource | All nouns | `unrdf hook validate h1` |
| `verify` | Verify integrity | transaction, lockchain | `unrdf lockchain verify` |
| `check` | Check status/health | sidecar | `unrdf sidecar check` |
| `test` | Test resource | hook, policy | `unrdf policy test --dry-run` |
| `lint` | Lint resource | policy | `unrdf policy lint rules.json` |

### Monitoring Verbs
| Verb | Description | Applies To | Example |
|------|-------------|------------|---------|
| `stats` | Show statistics | All nouns | `unrdf store stats` |
| `metrics` | Show metrics | sidecar | `unrdf sidecar metrics` |
| `logs` | View logs | sidecar | `unrdf sidecar logs --follow` |
| `watch` | Watch changes | All nouns | `unrdf hook list --watch` |
| `history` | Show history | hook, transaction | `unrdf hook history h1` |
| `audit` | Generate audit | lockchain | `unrdf lockchain audit` |

### Administrative Verbs
| Verb | Description | Applies To | Example |
|------|-------------|------------|---------|
| `backup` | Create backup | store | `unrdf store backup backup.nq` |
| `restore` | Restore backup | store | `unrdf store restore backup.nq` |
| `optimize` | Optimize resource | store | `unrdf store optimize` |
| `compact` | Compact storage | store | `unrdf store compact` |
| `rollback` | Rollback change | transaction | `unrdf transaction rollback tx1` |
| `reset` | Reset to defaults | config | `unrdf config reset` |

## Command Matrix

### graph × verbs
```
✓ = Supported, ○ = Planned, ✗ = Not Applicable

              list get create update delete apply describe
graph           ✓    ✓    ✓      ✓      ✓     ✓      ✓
              import export convert merge diff patch validate
graph           ✓      ✓      ✓     ✓    ✓     ✓       ✓
              stats search watch
graph           ✓     ✓      ✓
```

### hook × verbs
```
              list get create update delete apply describe
hook            ✓    ✓    ✓      ✓      ✓     ✓      ✓
              enable disable eval test validate history
hook            ✓      ✓      ✓    ✓      ✓       ✓
              watch stats
hook            ✓     ✓
```

### policy × verbs
```
              list get create update delete apply describe
policy          ✓    ✓    ○      ○      ✓     ✓      ✓
              activate deactivate validate test diff
policy            ✓        ✓         ✓      ✓    ✓
              export lint watch
policy            ✓     ✓     ✓
```

### sidecar × verbs
```
              status start stop restart health
sidecar         ✓      ✓     ✓     ✓       ✓
              logs metrics version describe
sidecar         ✓      ✓       ✓       ✓
              config[get/set]
sidecar         ✓
```

### store × verbs
```
              stats import export query clear
store           ✓     ✓      ✓      ✓     ✓
              backup restore validate optimize compact
store           ✓      ✓        ✓        ✓       ✓
```

### context × verbs
```
              list get use create delete current
context         ✓    ✓   ✓    ✓      ✓      ✓
              rename set
context         ✓     ✓
```

### transaction × verbs
```
              list get history rollback verify
transaction     ✓    ✓    ✓       ✓       ✓
              export stats
transaction     ✓      ✓
```

### lockchain × verbs
```
              list get verify export stats audit
lockchain       ✓    ✓    ✓      ✓      ✓     ✓
```

## Command Aliases

### Global Aliases
- `unrdf ls` → `unrdf graph list` (most common list)
- `unrdf run <query>` → `unrdf store query <query>`
- `unrdf eval <hook>` → `unrdf hook eval <hook>`
- `unrdf apply <file>` → Auto-detect (policy/graph)
- `unrdf status` → `unrdf sidecar status`
- `unrdf logs` → `unrdf sidecar logs`

### Noun Aliases
```bash
unrdf g list     # graph list
unrdf h eval x   # hook eval x
unrdf p apply y  # policy apply y
unrdf sc logs    # sidecar logs
unrdf s stats    # store stats
unrdf ctx use x  # context use x
unrdf tx list    # transaction list
unrdf lc verify  # lockchain verify
```

### Verb Shortcuts
```bash
unrdf graph ls   # list (common shortcut)
unrdf hook rm x  # delete (Unix-style)
unrdf policy cp  # copy (future)
unrdf store mv   # move (future)
```

## Command Categories

### 1. Resource Management (40%)
Primary workflow: Create, read, update, delete resources
```bash
unrdf graph create my-graph
unrdf graph import my-graph data.ttl
unrdf graph validate my-graph
unrdf graph export my-graph --format=turtle
```

### 2. Knowledge Hooks (25%)
Reactive intelligence and policy enforcement
```bash
unrdf hook create health-check sparql-ask
unrdf hook eval health-check --data=./graphs/
unrdf hook history health-check --limit=10
```

### 3. Policy Governance (15%)
Compliance and governance workflows
```bash
unrdf policy apply compliance-pack.json
unrdf policy validate compliance-pack.json
unrdf policy test compliance-pack.json --dry-run
```

### 4. Sidecar Operations (10%)
Service lifecycle and monitoring
```bash
unrdf sidecar status
unrdf sidecar logs --follow
unrdf sidecar config set validation.strict=true
```

### 5. Data Operations (5%)
Query and transformation
```bash
unrdf store query "SELECT * WHERE { ?s ?p ?o } LIMIT 10"
unrdf store import data.nq
unrdf store stats
```

### 6. Context Management (3%)
Multi-environment workflows
```bash
unrdf context create production --sidecar=prod:50051
unrdf context use production
unrdf context list
```

### 7. Audit & Compliance (2%)
Transaction and audit trail
```bash
unrdf transaction history --limit=20
unrdf lockchain verify
unrdf lockchain audit --export=audit-report.json
```

## Command Completion

### Bash Completion Specification
```bash
# /etc/bash_completion.d/unrdf
_unrdf_completion() {
  local cur prev nouns verbs
  COMPREPLY=()
  cur="${COMP_WORDS[COMP_CWORD]}"
  prev="${COMP_WORDS[COMP_CWORD-1]}"

  nouns="graph hook policy sidecar store context transaction lockchain config"

  case "${prev}" in
    unrdf)
      COMPREPLY=( $(compgen -W "${nouns}" -- ${cur}) )
      return 0
      ;;
    graph)
      COMPREPLY=( $(compgen -W "list get create delete import export validate merge diff stats" -- ${cur}) )
      ;;
    hook)
      COMPREPLY=( $(compgen -W "list get create update delete eval validate history enable disable test" -- ${cur}) )
      ;;
    # ... more cases
  esac
}

complete -F _unrdf_completion unrdf
```

### Fish Completion
```fish
# ~/.config/fish/completions/unrdf.fish
complete -c unrdf -f

# Nouns
complete -c unrdf -n "__fish_use_subcommand" -a "graph" -d "Knowledge graphs"
complete -c unrdf -n "__fish_use_subcommand" -a "hook" -d "Knowledge hooks"
complete -c unrdf -n "__fish_use_subcommand" -a "policy" -d "Policy packs"
# ... more nouns

# graph verbs
complete -c unrdf -n "__fish_seen_subcommand_from graph" -a "list" -d "List graphs"
complete -c unrdf -n "__fish_seen_subcommand_from graph" -a "get" -d "Get graph"
# ... more verbs
```

### Zsh Completion
```zsh
# ~/.zsh/completions/_unrdf
#compdef unrdf

local -a nouns verbs

nouns=(
  'graph:Knowledge graphs'
  'hook:Knowledge hooks'
  'policy:Policy packs'
  'sidecar:KGC sidecar'
  'store:Triple store'
  'context:CLI contexts'
  'transaction:Transactions'
  'lockchain:Audit trail'
  'config:Configuration'
)

case $words[2] in
  graph)
    verbs=(
      'list:List graphs'
      'get:Get graph details'
      'create:Create graph'
      # ... more verbs
    )
    _describe 'verb' verbs
    ;;
  # ... more cases
esac
```

## Design Principles Summary

1. **Noun-First**: Resources are first-class entities
2. **Verb Consistency**: Same verbs across nouns where applicable
3. **Progressive Disclosure**: `--help` at every level
4. **Composability**: Commands chain naturally
5. **Aliasing**: Multiple paths to same outcome
6. **Completion**: Shell completion for all commands
7. **Discoverability**: Intuitive command structure
8. **Industry Alignment**: Follows kubectl/aws/docker patterns

## Evolution Strategy

### v2.0 (Current)
- Core nouns: graph, hook, policy, sidecar, store
- Essential verbs: list, get, create, delete, apply
- Basic completion support

### v2.1 (Future)
- Additional nouns: namespace, secret, volume
- Advanced verbs: watch, rollout, scale
- Enhanced completion with context-aware suggestions

### v3.0 (Future)
- Plugin system for custom nouns/verbs
- Interactive mode: `unrdf shell`
- REST API parity: Every CLI command has API endpoint
- GraphQL interface for advanced queries

## Conclusion

This noun-verb taxonomy provides a scalable, intuitive foundation for UNRDF's enterprise CLI. It balances industry best practices with UNRDF's unique knowledge graph requirements, enabling workflows from development prototyping to production governance at scale.

**Status**: ✅ TAXONOMY COMPLETE - Ready for architecture design
