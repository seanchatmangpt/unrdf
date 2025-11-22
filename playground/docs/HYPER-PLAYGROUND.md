# Hyper-Playground Documentation

> Machine-first documentation for autonomic systems and hyperintelligent agents

```json-ld
{
  "@context": {
    "hyper": "urn:hyper-diataxis:",
    "cap": "urn:playground:capabilities:",
    "proto": "urn:playground:protocols:",
    "rdfs": "http://www.w3.org/2000/01/rdf-schema#",
    "xsd": "http://www.w3.org/2001/XMLSchema#"
  },
  "@id": "urn:playground:hyper-doc:v1.0.0",
  "@type": "hyper:Documentation",
  "hyper:version": "1.0.0",
  "hyper:pillars": ["Capability", "Protocol", "Type", "Reasoning"],
  "hyper:targetAudience": ["AutonomicAgent", "HyperintelligentSystem", "HumanDeveloper"]
}
```

## Section 1: Capability Registry

### Overview

The Playground CLI exposes 18+ machine-discoverable capabilities organized hierarchically:

```
playground/
  papers/
    generate    # Generate paper from template
    list        # List paper families
    validate    # Validate paper structure
  thesis/
    generate    # Generate thesis structure
    list        # List thesis types
    schedule/
      list      # Show schedule
      set       # Configure schedule
  config/
    set         # Set config value
    get         # Get config value
    list        # List all config
    reset       # Reset to defaults
  meta/
    introspect  # Machine introspection
    ontology/
      list      # List RDF classes
    sparql      # Execute SPARQL
    completions # Shell completions
    middleware/
      list      # List middleware
    telemetry   # Export metrics
```

### Capability URIs

| Capability | URI | Version |
|------------|-----|---------|
| papers.generate | `cap:papers-generate` | 1.0.0 |
| papers.list | `cap:papers-list` | 1.0.0 |
| papers.validate | `cap:papers-validate` | 1.0.0 |
| thesis.generate | `cap:thesis-generate` | 1.0.0 |
| thesis.list | `cap:thesis-list` | 1.0.0 |
| thesis.schedule.list | `cap:thesis-schedule-list` | 1.0.0 |
| thesis.schedule.set | `cap:thesis-schedule-set` | 1.0.0 |
| config.set | `cap:config-set` | 1.0.0 |
| config.get | `cap:config-get` | 1.0.0 |
| config.list | `cap:config-list` | 1.0.0 |
| config.reset | `cap:config-reset` | 1.0.0 |
| meta.introspect | `cap:meta-introspect` | 1.0.0 |
| meta.ontology.list | `cap:meta-ontology-list` | 1.0.0 |
| meta.sparql | `cap:meta-sparql` | 1.0.0 |
| meta.completions | `cap:meta-completions` | 1.0.0 |
| meta.middleware.list | `cap:meta-middleware-list` | 1.0.0 |
| meta.telemetry | `cap:meta-telemetry` | 1.0.0 |

### Machine Discovery

```sparql
PREFIX cap: <urn:playground:capabilities:>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?capability ?name ?preconditions ?postconditions
WHERE {
  ?capability a cap:Capability ;
              rdfs:label ?name ;
              cap:preconditions ?preconditions ;
              cap:postconditions ?postconditions .
}
```

## Section 2: Protocol Specifications

### Papers Generation Protocol

**State Machine:**

```
[INIT] --> [VALIDATE_INPUT] --> [LOAD_TEMPLATE] --> [RENDER] --> [WRITE_OUTPUT] --> [DONE]
                |                    |                  |               |
                v                    v                  v               v
           [ERROR]              [ERROR]            [ERROR]         [ERROR]
```

**States:**
- `INIT`: Parse CLI arguments
- `VALIDATE_INPUT`: Zod schema validation
- `LOAD_TEMPLATE`: Load Nunjucks template by family
- `RENDER`: Execute template rendering
- `WRITE_OUTPUT`: Write to file system
- `DONE`: Success state
- `ERROR`: Terminal error state

### SPARQL Query Protocol

**State Machine:**

```
[PARSE_QUERY] --> [VALIDATE_SYNTAX] --> [LOAD_ONTOLOGY] --> [EXECUTE] --> [FORMAT_RESULTS]
                          |                   |                 |
                          v                   v                 v
                      [SYNTAX_ERROR]    [LOAD_ERROR]     [EXECUTION_ERROR]
```

### Configuration Protocol

**Atomicity Guarantees:**
- Config reads are atomic
- Config writes use copy-on-write semantics
- Reset operations are transactional

## Section 3: Type System (RDF Ontology)

### Core Classes

```turtle
@prefix paper: <http://example.org/ontology/paper#> .
@prefix thesis: <http://example.org/ontology/thesis#> .

# Paper Domain
paper:AcademicWork    # Base class
paper:Paper           # Research paper (subclass)
paper:IMRADPaper      # IMRAD structure
paper:DSRPaper        # Design Science Research
paper:ArgumentPaper   # Argument-based
paper:ContributionPaper # Contribution-based

# Thesis Domain
thesis:Thesis         # Base thesis
thesis:PhDThesis      # Doctoral thesis
thesis:MastersThesis  # Masters thesis
thesis:BachelorsThesis # Undergraduate thesis

# Supporting Classes
paper:Section         # Document section
paper:Author          # Document author
thesis:Schedule       # Thesis timeline
thesis:Milestone      # Schedule milestone
thesis:DefenseEvent   # Defense event
```

### SHACL Constraints

See `playground/ontologies/papers-thesis.ttl` for embedded SHACL shapes:

- `paper:PaperShape` - Paper must have author and title
- `paper:IMRADPaperShape` - IMRAD must have 4+ sections
- `thesis:ThesisShape` - Thesis must have advisor and institution
- `thesis:DefenseEventShape` - Defense must have date and 3+ committee members

### Type Coercion Rules

| Input Type | CLI Arg Type | RDF Type | Coercion |
|------------|--------------|----------|----------|
| String | `--title` | `xsd:string` | Identity |
| Date | `--defense` | `xsd:date` | Parse YYYY-MM-DD |
| Boolean | `--strict` | `xsd:boolean` | Parse true/false |
| JSON | `--sections` | N3 List | JSON.parse |
| Enum | `--family` | `skos:Concept` | Validate against enum |

## Section 4: Reasoning Rules (N3)

### Capability Matching Rules

```n3
@prefix cap: <urn:playground:capabilities:> .
@prefix log: <http://www.w3.org/2000/10/swap/log#> .

# Rule: Match capability by task type
{
  ?task a cap:PaperGenerationTask ;
        cap:requiresFamily ?family .
} => {
  ?task cap:matchedCapability cap:papers-generate .
} .

# Rule: Validate preconditions
{
  ?capability cap:preconditions ?pre .
  ?pre cap:requiresConfig ?configKey .
  ?config cap:hasKey ?configKey .
} => {
  ?capability cap:preconditionsMet true .
} .
```

### Protocol Validation Rules

```n3
# Rule: Papers generation requires title and author
{
  ?invocation a cap:PapersGenerateInvocation .
  ?invocation cap:hasArg [ cap:name "title" ; cap:value ?title ] .
  ?invocation cap:hasArg [ cap:name "author" ; cap:value ?author ] .
} => {
  ?invocation cap:valid true .
} .
```

### Performance Prediction Rules

```n3
# Rule: Predict latency based on operation type
{
  ?op a cap:PapersGenerateOperation .
  ?op cap:family "imrad" .
} => {
  ?op cap:predictedLatencyP50 "120ms" .
  ?op cap:predictedLatencyP99 "450ms" .
} .

{
  ?op a cap:SPARQLOperation .
  ?op cap:resultCount ?count .
  ?count log:lessThan 100 .
} => {
  ?op cap:predictedLatencyP50 "50ms" .
} .
```

### Error Recovery Rules

```n3
# Rule: Config not found - suggest creation
{
  ?error a cap:ConfigNotFoundError ;
         cap:key ?key .
} => {
  ?error cap:recoveryAction [
    a cap:CreateConfigAction ;
    cap:command "playground config set" ;
    cap:args [ cap:key ?key ; cap:value "" ]
  ] .
} .

# Rule: Template not found - suggest list
{
  ?error a cap:TemplateNotFoundError ;
         cap:family ?family .
} => {
  ?error cap:recoveryAction [
    a cap:ListFamiliesAction ;
    cap:command "playground papers list --verbose"
  ] .
} .
```

## Cross-References

- **CAPABILITY-REGISTRY.jsonld** - Full JSON-LD capability registry
- **PROTOCOL-SPECIFICATIONS.md** - Detailed state machines
- **COMMAND-REFERENCE.md** - Complete command reference
- **SPARQL-QUERY-GUIDE.md** - Named queries catalog
- **RDF-ONTOLOGY-GUIDE.md** - Ontology documentation
- **AGENT-INTEGRATION.md** - Agent usage patterns
- **PERFORMANCE-PROFILE.md** - Latency profiles
- **FAILURE-MODES.md** - FMEA analysis

## SPARQL Verification Queries

### Verify Capability Discovery

```sparql
ASK {
  cap:papers-generate a cap:Capability ;
                      cap:version "1.0.0" .
}
```

### Verify Protocol Compliance

```sparql
ASK {
  ?state a proto:State ;
         proto:protocol proto:PapersGeneration .
  FILTER(?state IN (proto:INIT, proto:VALIDATE_INPUT, proto:LOAD_TEMPLATE,
                    proto:RENDER, proto:WRITE_OUTPUT, proto:DONE, proto:ERROR))
}
```

### Verify Type System

```sparql
ASK {
  paper:Paper rdfs:subClassOf paper:AcademicWork .
  thesis:Thesis rdfs:subClassOf paper:AcademicWork .
}
```
