# UNRDF v2 CLI - Enterprise Noun-Verb Interface

Production-ready CLI implementing kubectl/docker-style noun-verb pattern for UNRDF knowledge graph operations.

## Architecture

```
src/cli-v2/
├── index.mjs              # Main CLI entry point
├── core/                  # Core CLI infrastructure
│   ├── router.mjs         # Noun-verb routing
│   ├── context.mjs        # Context management (like kubeconfig)
│   ├── plugin-loader.mjs  # Plugin system
│   ├── config.mjs         # Configuration hierarchy
│   └── completion.mjs     # Shell completion
├── commands/              # Resource commands
│   ├── graph/            # Graph operations
│   ├── hook/             # Knowledge hook management
│   ├── policy/           # Policy pack operations
│   ├── sidecar/          # Sidecar management
│   ├── store/            # Store operations
│   └── context/          # Context management
├── formatters/            # Output formatters
│   ├── json.mjs
│   ├── yaml.mjs
│   ├── table.mjs
│   └── tree.mjs
└── middleware/            # CLI middleware
    ├── auth.mjs           # Authentication
    ├── telemetry.mjs      # Observability
    ├── validation.mjs     # Input validation
    └── error-handler.mjs  # Error handling
```

## Usage Examples

### Graph Operations
```bash
# List all graphs
unrdf graph list

# Create a new graph
unrdf graph create my-graph --base-iri=http://example.org/

# Validate graph against policy
unrdf graph validate my-graph --policy=compliance

# Export graph
unrdf graph export my-graph --format=turtle --output=graph.ttl
```

### Hook Operations
```bash
# List knowledge hooks
unrdf hook list

# Create a hook
unrdf hook create health --type=sparql-ask --file=health.rq

# Evaluate hook
unrdf hook eval health --data=./graphs/

# Show hook history
unrdf hook history health --limit=20
```

### Policy Operations
```bash
# List policy packs
unrdf policy list

# Apply policy pack
unrdf policy apply compliance.json

# Validate policy
unrdf policy validate --dry-run

# Test policy
unrdf policy test compliance.json --data=./test-data/
```

### Sidecar Operations
```bash
# Check sidecar status
unrdf sidecar status

# View logs
unrdf sidecar logs --follow --tail=100

# Get/set config
unrdf sidecar config get validation.strictMode
unrdf sidecar config set validation.strictMode=true

# Health check
unrdf sidecar health
```

### Store Operations
```bash
# Import data
unrdf store import data.ttl --graph=default

# Execute SPARQL query
unrdf store query --file=query.rq --format=json

# Show statistics
unrdf store stats

# Backup store
unrdf store backup --output=backup.nq

# Restore from backup
unrdf store restore backup.nq
```

### Context Operations
```bash
# List contexts
unrdf context list

# Create context
unrdf context create dev --sidecar=localhost:50051

# Switch context
unrdf context use production

# Show current context
unrdf context current

# Delete context
unrdf context delete staging
```

## Enterprise Features

### Output Formatting
All commands support multiple output formats:
```bash
unrdf graph list --output=json    # JSON format
unrdf graph list --output=yaml    # YAML format
unrdf graph list --output=table   # ASCII table (default)
unrdf graph list --output=tree    # Tree structure
```

### Dry-Run Mode
Preview changes without executing:
```bash
unrdf graph create test --dry-run
unrdf policy apply pack.json --dry-run
```

### Watch Mode
Monitor resources for changes:
```bash
unrdf hook list --watch
unrdf sidecar logs --follow
```

### Batch Operations
Execute operations in batch:
```bash
unrdf hook eval --batch hooks.json
```

### Shell Completion
Generate completion scripts:
```bash
# Bash
unrdf completion bash > /etc/bash_completion.d/unrdf

# Zsh
unrdf completion zsh > /usr/local/share/zsh/site-functions/_unrdf

# Fish
unrdf completion fish > ~/.config/fish/completions/unrdf.fish
```

## Configuration

Configuration is loaded hierarchically from:
1. Command-line flags
2. Environment variables
3. `./unrdf.config.json` (current directory)
4. `~/.unrdf/config.json` (user home)
5. `/etc/unrdf/config.json` (system-wide)

Example `unrdf.config.json`:
```json
{
  "baseIRI": "http://example.org/",
  "sidecar": {
    "endpoint": "http://localhost:50051",
    "timeout": 30000,
    "retries": 3
  },
  "output": {
    "format": "table",
    "color": true
  },
  "telemetry": {
    "enabled": true
  }
}
```

## Environment Variables

- `UNRDF_BASE_IRI` - Default base IRI
- `UNRDF_SIDECAR_ENDPOINT` - Sidecar endpoint
- `UNRDF_OUTPUT_FORMAT` - Default output format
- `UNRDF_TELEMETRY_ENABLED` - Enable telemetry
- `UNRDF_API_KEY` - API authentication key
- `UNRDF_DEBUG` - Enable debug mode

## Plugin System

Extend CLI with custom plugins:

```javascript
// ~/.unrdf/plugins/my-plugin/index.mjs
export default {
  name: 'my-plugin',
  version: '1.0.0',

  async init(config) {
    console.log('Plugin initialized');
  },

  commands: {
    'custom': customCommand
  },

  async cleanup() {
    console.log('Plugin cleanup');
  }
};
```

## Integration Points

- **Knowledge Engine**: Full integration with knowledge-engine composables
- **KGC Sidecar**: gRPC client for sidecar operations
- **Store Context**: Unified store context system
- **OpenTelemetry**: Built-in observability and tracing

## Development

Run CLI locally:
```bash
node src/cli-v2/index.mjs graph list
```

Add CLI to path:
```bash
npm link
unrdf graph list
```
