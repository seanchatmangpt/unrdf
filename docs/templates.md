# UNRDF Templates Guide

This guide explains how to use UNRDF templates to accelerate development of knowledge graph applications, governance systems, and analytics platforms.

## Table of Contents

- [Overview](#overview)
- [Hook Templates](#hook-templates)
- [Policy Pack Templates](#policy-pack-templates)
- [Project Templates](#project-templates)
- [Init Command](#init-command)
- [Customization Guide](#customization-guide)
- [Best Practices](#best-practices)

## Overview

UNRDF provides three types of templates:

1. **Hook Templates**: Pre-built SPARQL and SHACL patterns for common Knowledge Hook scenarios
2. **Policy Pack Templates**: Ready-to-use governance and compliance policy packs
3. **Project Templates**: Complete project scaffolding for different use cases

These templates follow the 80/20 principle, providing the most commonly needed patterns and structures to get you productive quickly.

## Hook Templates

Hook templates are located in `templates/hooks/` and provide content-addressed condition files for Knowledge Hooks.

### Available Hook Templates

#### 1. ASK Hook Template (`ask-hook.ttl`)

**Use Case**: Simple boolean conditions to detect specific patterns

**Example**: Detect when a resource has a specific property value

```sparql
PREFIX ex: <http://example.org/>

ASK WHERE {
  ?subject ex:hasProperty ex:someValue .
}
```

**When to Use**:
- Simple trigger conditions
- Existence checks
- Boolean validation rules

**Customization**:
1. Replace the WHERE clause with your condition
2. Add FILTER clauses for complex logic
3. Calculate SHA-256 hash
4. Reference in your Knowledge Hook definition

#### 2. SELECT Hook Template (`select-hook.rq`)

**Use Case**: Return specific data for analysis or transformation

**Example**: Find all transactions above a threshold

```sparql
PREFIX ex: <http://example.org/>

SELECT ?transaction ?amount ?timestamp WHERE {
  ?transaction rdf:type ex:Transaction ;
               ex:amount ?amount ;
               ex:timestamp ?timestamp .
  FILTER(?amount > 10000)
}
ORDER BY DESC(?amount)
```

**When to Use**:
- Data extraction and analysis
- Pattern matching with results
- Aggregation and grouping

**Customization**:
1. Customize SELECT variables
2. Update WHERE clause patterns
3. Add FILTER, GROUP BY, ORDER BY as needed
4. Calculate hash and reference in hook

#### 3. SHACL Hook Template (`shacl-hook.ttl`)

**Use Case**: Structural and value validation

**Example**: Validate that Person resources have required properties

```turtle
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix ex: <http://example.org/> .

ex:PersonShape
  a sh:NodeShape ;
  sh:targetClass ex:Person ;
  sh:property [
    sh:path ex:name ;
    sh:minCount 1 ;
    sh:datatype xsd:string ;
  ] .
```

**When to Use**:
- Data quality validation
- Schema enforcement
- Structural constraints

**Customization**:
1. Define your NodeShape and target class
2. Add property constraints (cardinality, datatype, ranges)
3. Include custom SPARQL constraints if needed
4. Calculate hash and reference

#### 4. Delta Hook Template (`delta-hook.rq`)

**Use Case**: Detect changes in graph deltas

**Example**: Detect high-value transaction additions

```sparql
ASK WHERE {
  ?transaction rdf:type ex:Transaction ;
               ex:amount ?amount .
  FILTER(?amount > 10000)
}
```

**When to Use**:
- Change detection
- Reactive processing
- Event-driven workflows

**Customization**:
1. Set `channel.view` to 'delta' in hook definition
2. Customize detection pattern
3. Consider additions vs. removals

#### 5. Threshold Hook Template (`threshold-hook.rq`)

**Use Case**: Monitor numeric thresholds

**Example**: Detect when total exceeds limit

```sparql
SELECT ?category (SUM(?amount) AS ?total) WHERE {
  ?transaction ex:category ?category ;
               ex:amount ?amount .
}
GROUP BY ?category
HAVING(SUM(?amount) > 50000)
```

**When to Use**:
- Compliance monitoring
- Alerting and notifications
- Capacity management

**Customization**:
1. Define threshold values
2. Add aggregation logic (SUM, AVG, COUNT)
3. Configure time windows if needed

#### 6. Count Hook Template (`count-hook.rq`)

**Use Case**: Count resources and trigger on thresholds

**Example**: Alert when resource count exceeds capacity

```sparql
SELECT (COUNT(?resource) AS ?count) WHERE {
  ?resource rdf:type ex:MonitoredResource .
}
HAVING(COUNT(?resource) > 100)
```

**When to Use**:
- Quota enforcement
- Inventory management
- Capacity planning

**Customization**:
1. Define what to count
2. Add GROUP BY for per-category counts
3. Set threshold values

#### 7. Window Hook Template (`window-hook.rq`)

**Use Case**: Time-window analysis and rate limiting

**Example**: Detect unusual activity in recent period

```sparql
SELECT (COUNT(?event) AS ?eventCount) WHERE {
  ?event rdf:type ex:Event ;
         ex:timestamp ?timestamp .
  BIND((NOW() - ?timestamp) AS ?age)
  FILTER(?age < 3600)  # Last hour
}
HAVING(COUNT(?event) > 100)
```

**When to Use**:
- Rate limiting
- Trend detection
- Temporal anomaly detection

**Customization**:
1. Adjust time window size
2. Define aggregation logic
3. Compare against baseline or threshold

## Policy Pack Templates

Policy pack templates are located in `templates/policy-packs/` and provide complete governance frameworks.

### Available Policy Pack Templates

#### 1. Basic Policy Pack (`basic-policy.yaml`)

**Use Case**: Fundamental governance and compliance

**Features**:
- Data quality validation
- Access control basics
- Audit trail creation
- Simple compliance checks

**Configuration**:
```yaml
metadata:
  name: "basic-policy-pack"
  version: "1.0.0"

hooks:
  - path: "./hooks/data-quality-validation.mjs"
    enabled: true
    priority: 100
```

**Customization**:
1. Update metadata (name, version, description)
2. Add/remove hooks as needed
3. Configure priority and strictMode
4. Define environment-specific overrides

#### 2. Governance Policy Pack (`governance-policy.yaml`)

**Use Case**: Comprehensive data stewardship and governance

**Features**:
- Data ownership validation
- Access control enforcement
- Data classification
- Audit trail with cryptographic proofs
- Data retention policies
- Lineage tracking

**Roles**:
- Data Steward
- Data Owner
- Data Consumer

**Customization**:
1. Define roles and responsibilities
2. Configure classification scheme
3. Set up monitoring and alerting
4. Customize reporting schedules

#### 3. Security Policy Pack (`security-policy.yaml`)

**Use Case**: Security controls and threat detection

**Features**:
- Authentication and authorization
- Threat detection and anomaly detection
- Data protection and encryption
- Vulnerability prevention
- Security audit trail
- Rate limiting and brute force protection

**Security Controls**:
- Authentication (JWT, OAuth2, API Keys)
- Authorization (RBAC)
- Encryption (AES-256, TLS 1.3)
- Threat detection
- Input validation

**Customization**:
1. Configure authentication methods
2. Define roles and permissions
3. Set rate limiting thresholds
4. Configure threat detection algorithms
5. Define PII patterns for detection

#### 4. Compliance Policy Pack (`compliance-policy.yaml`)

**Use Case**: Regulatory compliance (GDPR, SOX, HIPAA)

**Features**:
- GDPR compliance (consent, data minimization, right to erasure)
- SOX financial controls and audit trail
- HIPAA PHI protection
- Data retention enforcement
- Consent management
- Compliance reporting

**Regulatory Frameworks**:
- **GDPR**: EU data protection regulation
- **SOX**: Financial reporting compliance
- **HIPAA**: Healthcare data protection

**Customization**:
1. Enable applicable compliance frameworks
2. Configure retention periods
3. Define consent purposes
4. Set up breach notification
5. Configure reporting schedules

## Project Templates

Project templates are located in `templates/projects/` and provide complete project scaffolding.

### Available Project Templates

#### 1. Starter Template

**Use Case**: Basic UNRDF project for getting started

**Structure**:
```
starter/
├── src/
│   ├── index.mjs
│   └── hooks/
├── data/
│   └── sample.ttl
├── queries/
│   └── sample.rq
├── shapes/
│   └── sample-shape.ttl
├── test/
│   └── hooks.test.mjs
├── package.json
├── unrdf.config.mjs
└── README.md
```

**Features**:
- Sample data and queries
- Basic Knowledge Hooks
- Test suite setup
- Configuration examples

**Use Cases**:
- Learning UNRDF
- Prototyping
- Small projects

#### 2. Governance Template

**Use Case**: Data governance and compliance projects

**Structure**:
```
governance/
├── src/
│   ├── governance/
│   │   ├── audit-runner.mjs
│   │   ├── compliance-report.mjs
│   │   └── policy-manager.mjs
│   └── hooks/
├── policy-packs/
│   ├── governance/
│   └── compliance/
├── data/
│   ├── policies.ttl
│   └── roles.ttl
└── test/
    └── governance/
```

**Features**:
- Policy pack management
- Compliance monitoring
- Audit trail generation
- Data stewardship workflows

**Use Cases**:
- Enterprise data governance
- Regulatory compliance
- Policy enforcement

#### 3. Analytics Template

**Use Case**: Knowledge graph analytics and insights

**Structure**:
```
analytics/
├── src/
│   ├── analytics/
│   │   ├── run-analysis.mjs
│   │   ├── metrics-collector.mjs
│   │   └── analyzers/
│   └── hooks/
├── queries/
│   ├── metrics/
│   └── insights/
└── test/
    └── analytics/
```

**Features**:
- Graph metrics and analytics
- Trend detection
- Anomaly detection
- Real-time monitoring

**Use Cases**:
- Graph analytics
- Metrics collection
- Insight generation
- Monitoring dashboards

## Init Command

The `unrdf init` command provides interactive project scaffolding.

### Usage

#### Interactive Mode (Recommended)

```bash
npx unrdf init
```

The CLI will prompt you for:
1. Project name
2. Template type (starter, governance, analytics)
3. Base IRI
4. Git initialization preference
5. Dependency installation preference

#### Command-Line Arguments

```bash
npx unrdf init my-project \
  --template=governance \
  --base-iri=http://example.org/my-project/ \
  --no-git
```

**Arguments**:
- `name`: Project name (positional)
- `--template`: Template type (starter, governance, analytics)
- `--base-iri`: Base IRI for the project
- `--no-git`: Skip git initialization
- `--no-install`: Skip npm install

### What It Does

1. **Creates project directory** with chosen template
2. **Copies template files** including:
   - Source code
   - Configuration
   - Sample data
   - Tests
   - Documentation
3. **Customizes files**:
   - Updates `package.json` with project name
   - Sets base IRI in `unrdf.config.mjs`
4. **Initializes git** (optional)
5. **Installs dependencies** (optional)
6. **Displays next steps**

### Example Session

```bash
$ npx unrdf init

🚀 UNRDF Project Initialization

Project name (my-unrdf-project): knowledge-platform

Select project template:
❯ 1. starter
  2. governance
  3. analytics
Select (1-3): 2

Base IRI (http://example.org/knowledge-platform/): http://myorg.com/kb/

📋 Configuration:
   Project name: knowledge-platform
   Template: governance
   Base IRI: http://myorg.com/kb/
   Initialize git: yes
   Install dependencies: yes

Continue? (yes): yes

📦 Creating project at /path/to/knowledge-platform...

✅ Copied governance template
✅ Updated package.json
✅ Updated configuration
✅ Initialized git repository
📦 Installing dependencies...
✅ Dependencies installed

🎉 Project created successfully!

📚 Next steps:

   cd knowledge-platform
   npm install
   npm run dev
```

## Customization Guide

### Customizing Hook Templates

1. **Copy the template**:
   ```bash
   cp templates/hooks/threshold-hook.rq my-project/queries/my-threshold.rq
   ```

2. **Modify the query**:
   - Update PREFIX declarations
   - Change WHERE clause patterns
   - Adjust FILTER conditions
   - Set threshold values

3. **Calculate SHA-256 hash**:
   ```bash
   sha256sum my-project/queries/my-threshold.rq
   ```

4. **Reference in Knowledge Hook**:
   ```javascript
   import { defineHook } from 'unrdf';

   export const myHook = defineHook({
     meta: {
       name: 'my-threshold-monitor',
       description: 'Custom threshold monitoring'
     },
     when: {
       kind: 'sparql-select',
       ref: {
         uri: 'file://queries/my-threshold.rq',
         sha256: '<calculated-hash>',
         mediaType: 'application/sparql-query'
       }
     },
     run: async (event) => {
       // Your custom logic
     }
   });
   ```

### Customizing Policy Packs

1. **Copy the template**:
   ```bash
   cp templates/policy-packs/governance-policy.yaml \
      my-project/policy-packs/my-policy.yaml
   ```

2. **Update metadata**:
   ```yaml
   metadata:
     name: "my-custom-policy-pack"
     version: "1.0.0"
     description: "Custom governance for my domain"
   ```

3. **Configure hooks**:
   ```yaml
   hooks:
     - path: "./hooks/my-custom-hook.mjs"
       enabled: true
       priority: 100
   ```

4. **Customize governance settings**:
   - Define roles and permissions
   - Set compliance requirements
   - Configure monitoring and alerting
   - Define retention policies

### Customizing Project Templates

After running `unrdf init`:

1. **Update configuration** (`unrdf.config.mjs`):
   ```javascript
   export default {
     baseIRI: 'http://your-domain.com/',
     prefixes: {
       'myorg': 'http://your-domain.com/ontology/'
     }
   };
   ```

2. **Add your data** in `data/`:
   - Replace sample.ttl with your RDF data
   - Add multiple data files as needed

3. **Create custom hooks** in `src/hooks/`:
   ```javascript
   import { defineHook } from 'unrdf';

   export const myCustomHook = defineHook({
     // Your hook definition
   });
   ```

4. **Write tests** in `test/`:
   ```javascript
   import { describe, it, expect } from 'vitest';
   import { myCustomHook } from '../src/hooks/my-custom-hook.mjs';

   describe('My Custom Hook', () => {
     it('should execute successfully', async () => {
       // Your tests
     });
   });
   ```

## Best Practices

### Hook Templates

1. **Start with templates**: Use provided templates as starting points
2. **Content-address everything**: Always calculate SHA-256 hashes
3. **Comment your queries**: Explain what each pattern detects
4. **Test thoroughly**: Validate queries against sample data
5. **Version control**: Track template modifications in git

### Policy Packs

1. **Modular design**: Create focused policy packs for specific domains
2. **Environment overrides**: Use environment-specific configurations
3. **Priority ordering**: Set hook priorities carefully
4. **Monitor performance**: Track policy pack execution metrics
5. **Document requirements**: Clearly explain compliance requirements

### Project Structure

1. **Follow conventions**: Stick to established directory structure
2. **Separation of concerns**: Keep data, queries, hooks, and tests separate
3. **Configuration management**: Use `unrdf.config.mjs` for project settings
4. **Documentation**: Maintain README and inline comments
5. **Testing**: Write tests for all custom hooks and analyzers

### Development Workflow

1. **Start with init**:
   ```bash
   npx unrdf init my-project --template=starter
   ```

2. **Explore examples**:
   - Review sample data and queries
   - Understand hook patterns
   - Run provided tests

3. **Customize gradually**:
   - Start with configuration
   - Add your data
   - Create custom hooks
   - Implement domain logic

4. **Test continuously**:
   ```bash
   npm test
   ```

5. **Iterate and refine**:
   - Monitor hook execution
   - Optimize queries
   - Add policy packs as needed
   - Expand test coverage

## Template Structure Reference

### Hook Template Anatomy

```sparql
# 1. Header comment explaining purpose
# 2. Usage instructions
# 3. Customization notes

# 4. PREFIX declarations
PREFIX ex: <http://example.org/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

# 5. Query type (ASK or SELECT)
ASK WHERE {
  # 6. Pattern matching logic
  # 7. FILTER clauses for conditions
  # 8. Comments explaining each section
}
```

### Policy Pack Template Anatomy

```yaml
# 1. Metadata
metadata:
  name: "policy-pack-name"
  version: "1.0.0"

# 2. Configuration
config:
  enabled: true
  priority: 90

# 3. Hooks
hooks:
  - path: "./hooks/hook-name.mjs"
    enabled: true

# 4. Domain-specific sections
compliance:
  standards: []

# 5. Environment overrides
environments:
  production: {}
```

### Project Template Anatomy

```
project-name/
├── src/                  # Source code
│   ├── index.mjs         # Entry point
│   └── hooks/            # Custom hooks
├── data/                 # RDF data
├── queries/              # SPARQL queries
├── shapes/               # SHACL shapes (if applicable)
├── policy-packs/         # Policy packs (governance template)
├── test/                 # Tests
├── package.json          # Dependencies and scripts
├── unrdf.config.mjs      # Configuration
└── README.md             # Documentation
```

## Advanced Customization

### Creating Custom Templates

1. **Create template directory**:
   ```bash
   mkdir -p templates/projects/my-custom-template
   ```

2. **Add required files**:
   - `package.json`
   - `README.md`
   - `unrdf.config.mjs`
   - Source code structure

3. **Test the template**:
   ```bash
   npx unrdf init test-project --template=my-custom-template
   ```

### Extending Policy Packs

1. **Create policy pack**:
   ```yaml
   # my-policy-pack.yaml
   metadata:
     name: "my-custom-pack"

   dependencies:
     - name: "basic-policy-pack"
       version: ">=1.0.0"

   hooks:
     # Your custom hooks
   ```

2. **Load with PolicyPackManager**:
   ```javascript
   import { PolicyPackManager } from 'unrdf';

   const manager = new PolicyPackManager();
   await manager.loadPolicyPack('./my-policy-pack.yaml');
   manager.activatePolicyPack('my-custom-pack');
   ```

## Troubleshooting

### Template Issues

**Problem**: Template not found
```
Error: Template "my-template" not found
```

**Solution**: Check template name and location:
```bash
ls templates/projects/
```

**Problem**: Hash mismatch
```
Error: SHA-256 hash mismatch for file://...
```

**Solution**: Recalculate hash after modifying file:
```bash
sha256sum path/to/file.rq
```

### Policy Pack Issues

**Problem**: Policy pack won't load
```
Error: Invalid policy pack manifest
```

**Solution**: Validate YAML syntax:
```bash
npm run validate:policies
```

**Problem**: Hook execution fails
```
Error: Hook "my-hook" failed: ...
```

**Solution**: Check hook definition and logs:
```javascript
// Enable debug mode
process.env.DEBUG = 'unrdf:*';
```

## Resources

- [UNRDF Documentation](https://github.com/unrdf/unrdf)
- [Knowledge Hooks Guide](https://github.com/unrdf/unrdf/blob/main/docs/knowledge-hooks.md)
- [Policy Packs Reference](https://github.com/unrdf/unrdf/blob/main/docs/policy-packs.md)
- [SPARQL 1.1 Specification](https://www.w3.org/TR/sparql11-query/)
- [SHACL Specification](https://www.w3.org/TR/shacl/)

## Contributing

To contribute new templates:

1. Create template in appropriate directory
2. Add comprehensive comments and documentation
3. Test with `unrdf init`
4. Submit pull request with:
   - Template files
   - Documentation
   - Examples
   - Tests

---

**Remember**: Templates are starting points, not constraints. Customize them to fit your specific needs and domain requirements. The 80/20 principle means we provide the most common patterns, but you can extend and modify as needed.
