# UNRDF Templates

This directory contains templates for accelerating UNRDF v3 development.

## Directory Structure

```
templates/
├── hooks/                  # Knowledge Hook templates (SPARQL, SHACL)
├── policy-packs/          # Policy pack templates (governance, compliance)
├── projects/              # Complete project scaffolding
│   ├── starter/           # Basic UNRDF project
│   ├── governance/        # Governance-focused project
│   └── analytics/         # Analytics-focused project
└── README.md              # This file
```

## Quick Start

### Initialize a new project

```bash
# Interactive mode (recommended)
npx unrdf init

# With arguments
npx unrdf init my-project --template=starter --base-iri=http://example.org/
```

### Use hook templates

1. Copy a hook template:
   ```bash
   cp templates/hooks/ask-hook.ttl my-project/queries/my-condition.rq
   ```

2. Customize the query

3. Calculate SHA-256 hash:
   ```bash
   sha256sum my-project/queries/my-condition.rq
   ```

4. Reference in Knowledge Hook definition

### Use policy pack templates

1. Copy a policy pack template:
   ```bash
   cp templates/policy-packs/governance-policy.yaml \
      my-project/policy-packs/my-policy.yaml
   ```

2. Customize metadata, hooks, and governance settings

3. Load with PolicyPackManager

## Available Templates

### Hook Templates (7)

1. **ask-hook.ttl**: Boolean condition checks (SPARQL ASK)
2. **select-hook.rq**: Pattern matching with results (SPARQL SELECT)
3. **shacl-hook.ttl**: Structural validation (SHACL)
4. **delta-hook.rq**: Change detection (delta view)
5. **threshold-hook.rq**: Numeric threshold monitoring
6. **count-hook.rq**: Count aggregation and quotas
7. **window-hook.rq**: Time-window analysis

### Policy Pack Templates (4)

1. **basic-policy.yaml**: Fundamental governance
2. **governance-policy.yaml**: Comprehensive data stewardship
3. **security-policy.yaml**: Security controls and threat detection
4. **compliance-policy.yaml**: Regulatory compliance (GDPR, SOX, HIPAA)

### Project Templates (3)

1. **starter/**: Basic UNRDF project for learning and prototyping
2. **governance/**: Data governance and compliance monitoring
3. **analytics/**: Knowledge graph analytics and insights

## Template Features

### Hook Templates
- Comprehensive inline documentation
- Customization instructions
- Example patterns for common use cases
- Ready for content-addressed referencing

### Policy Pack Templates
- Complete governance frameworks
- Role-based access control
- Compliance monitoring
- Audit trail configuration
- Environment-specific overrides

### Project Templates
- Complete project scaffolding
- Sample data and queries
- Test suite setup
- Configuration examples
- README and documentation

## 80/20 Principle

These templates follow the 80/20 principle:
- **80%** of use cases covered by templates
- **20%** customization for specific needs

Start with templates, customize as needed.

## Documentation

See `docs/templates.md` for comprehensive guide:
- Hook template customization
- Policy pack configuration
- Project template usage
- Best practices
- Troubleshooting

## Examples

### Create a threshold monitoring hook

```javascript
import { defineHook } from 'unrdf';

export const thresholdHook = defineHook({
  meta: {
    name: 'threshold-monitor',
    description: 'Monitor transaction amounts'
  },
  when: {
    kind: 'sparql-select',
    ref: {
      uri: 'file://queries/threshold.rq',  // From threshold-hook.rq template
      sha256: '<calculated-hash>',
      mediaType: 'application/sparql-query'
    }
  },
  run: async ({ payload }) => {
    console.log(`Threshold exceeded: ${payload.amount}`);
    return { alert: true };
  }
});
```

### Load a governance policy pack

```javascript
import { PolicyPackManager } from 'unrdf';

const manager = new PolicyPackManager();

// Load from template (customized)
await manager.loadPolicyPack('./policy-packs/governance-policy.yaml');

// Activate
manager.activatePolicyPack('governance-policy-pack');

// Get active hooks
const hooks = manager.getActiveHooks();
console.log(`Loaded ${hooks.length} governance hooks`);
```

## Contributing

To add new templates:

1. Create template in appropriate directory
2. Add comprehensive comments and examples
3. Update this README
4. Add to documentation in `docs/templates.md`
5. Test with `unrdf init` (for project templates)

## License

MIT
