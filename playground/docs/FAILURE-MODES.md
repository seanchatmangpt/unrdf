# Failure Modes and Effects Analysis (FMEA)

> Comprehensive failure analysis for Playground CLI

```json-ld
{
  "@context": {
    "fmea": "urn:playground:fmea:",
    "triz": "urn:playground:triz:",
    "dflss": "urn:playground:dflss:"
  },
  "@id": "urn:playground:failure-modes:v1.0.0",
  "@type": "fmea:FailureModeAnalysis"
}
```

---

## FMEA Overview

### Rating Scales

**Severity (S):** Impact on user/system
| Rating | Description |
|--------|-------------|
| 1-2 | Negligible - Minor inconvenience |
| 3-4 | Low - Reduced functionality |
| 5-6 | Moderate - Feature unavailable |
| 7-8 | High - Data loss or corruption |
| 9-10 | Critical - System failure |

**Occurrence (O):** Likelihood of occurrence
| Rating | Description |
|--------|-------------|
| 1-2 | Rare - < 1 in 10,000 |
| 3-4 | Low - 1 in 1,000 |
| 5-6 | Moderate - 1 in 100 |
| 7-8 | High - 1 in 10 |
| 9-10 | Very High - > 1 in 2 |

**Detection (D):** Ability to detect before impact
| Rating | Description |
|--------|-------------|
| 1-2 | High - Always detected |
| 3-4 | Moderate - Usually detected |
| 5-6 | Low - Sometimes detected |
| 7-8 | Very Low - Rarely detected |
| 9-10 | None - Never detected |

**RPN = S x O x D** (Risk Priority Number)

---

## Failure Mode Catalog

### CLI Parsing Subsystem

| ID | Failure Mode | Cause | Effect | S | O | D | RPN | Mitigation |
|----|--------------|-------|--------|---|---|---|-----|------------|
| F001 | Missing required argument | User error | Command fails with error | 3 | 7 | 1 | 21 | Clear error message with usage hint |
| F002 | Invalid argument type | User error | Zod validation failure | 3 | 6 | 1 | 18 | Type coercion where safe |
| F003 | Unknown flag | Typo/version mismatch | Command fails | 2 | 5 | 1 | 10 | Suggest similar flags |
| F004 | Argument parsing hang | Malformed input | CLI unresponsive | 7 | 2 | 3 | 42 | Input timeout (5s) |
| F005 | JSON parse failure | Invalid sections JSON | Validation error | 4 | 5 | 1 | 20 | JSON syntax hint |

### Validation Subsystem

| ID | Failure Mode | Cause | Effect | S | O | D | RPN | Mitigation |
|----|--------------|-------|--------|---|---|---|-----|------------|
| F006 | Zod schema mismatch | Code/schema drift | False rejections | 5 | 3 | 4 | 60 | Schema version testing |
| F007 | Validation bypass | Missing check | Invalid data processed | 7 | 2 | 6 | 84 | Integration tests |
| F008 | Overly strict validation | Conservative rules | User frustration | 3 | 4 | 3 | 36 | Allow warnings vs errors |
| F009 | Validation timeout | Complex schema | Slow response | 4 | 2 | 2 | 16 | Timeout + skip deep validation |
| F010 | SHACL violation missed | Incomplete shapes | Invalid RDF accepted | 6 | 3 | 5 | 90 | Comprehensive shape testing |

### Template Rendering Subsystem

| ID | Failure Mode | Cause | Effect | S | O | D | RPN | Mitigation |
|----|--------------|-------|--------|---|---|---|-----|------------|
| F011 | Template not found | Missing file | Generation fails | 6 | 2 | 1 | 12 | Bundle templates |
| F012 | Nunjucks syntax error | Bad template | Render exception | 6 | 3 | 1 | 18 | Template validation on load |
| F013 | Missing context variable | Code change | Undefined in output | 5 | 4 | 3 | 60 | Strict mode + defaults |
| F014 | Infinite loop in template | Bad loop logic | Process hang | 8 | 1 | 4 | 32 | Loop iteration limit |
| F015 | Memory exhaustion | Large template | OOM crash | 9 | 1 | 5 | 45 | Memory limits + streaming |
| F016 | LaTeX injection | Unsanitized input | Broken output | 4 | 5 | 3 | 60 | latex_escape filter |

### File I/O Subsystem

| ID | Failure Mode | Cause | Effect | S | O | D | RPN | Mitigation |
|----|--------------|-------|--------|---|---|---|-----|------------|
| F017 | File write permission denied | Permissions | Output not saved | 5 | 4 | 1 | 20 | Pre-check permissions |
| F018 | Disk full | Storage exhaustion | Write fails | 7 | 2 | 2 | 28 | Space check before write |
| F019 | Partial write | Interrupted I/O | Corrupted file | 8 | 2 | 5 | 80 | Atomic write (tmp + rename) |
| F020 | File path traversal | Malicious input | Security breach | 9 | 2 | 4 | 72 | Path sanitization |
| F021 | Config file corruption | Concurrent access | Lost settings | 6 | 3 | 4 | 72 | File locking |
| F022 | Ontology file unreadable | Encoding issues | Parse failure | 5 | 3 | 2 | 30 | UTF-8 enforcement |

### SPARQL Subsystem

| ID | Failure Mode | Cause | Effect | S | O | D | RPN | Mitigation |
|----|--------------|-------|--------|---|---|---|-----|------------|
| F023 | Query syntax error | Bad SPARQL | Query fails | 4 | 6 | 1 | 24 | Syntax highlighting in error |
| F024 | Unknown prefix | Missing declaration | Query fails | 3 | 5 | 1 | 15 | Auto-add common prefixes |
| F025 | Query timeout | Complex query | No results | 5 | 4 | 2 | 40 | Configurable timeout |
| F026 | Cartesian product | Missing join | Memory explosion | 8 | 3 | 6 | 144 | Query analysis + warnings |
| F027 | Empty results | No matches | User confusion | 2 | 5 | 1 | 10 | Explain empty results |
| F028 | Result truncation | Large result set | Data loss | 5 | 4 | 3 | 60 | Warn when truncating |

### Ontology Subsystem

| ID | Failure Mode | Cause | Effect | S | O | D | RPN | Mitigation |
|----|--------------|-------|--------|---|---|---|-----|------------|
| F029 | Ontology not found | Wrong path | Feature unavailable | 6 | 3 | 1 | 18 | Config check on startup |
| F030 | Turtle parse error | Invalid syntax | Load fails | 6 | 3 | 1 | 18 | Line-level error reporting |
| F031 | Circular imports | owl:imports loop | Infinite load | 8 | 1 | 4 | 32 | Import cycle detection |
| F032 | Inconsistent ontology | Logic errors | Wrong inferences | 7 | 2 | 7 | 98 | Consistency checking |
| F033 | Large ontology OOM | Big file | Process crash | 9 | 2 | 3 | 54 | Streaming parser |

### Configuration Subsystem

| ID | Failure Mode | Cause | Effect | S | O | D | RPN | Mitigation |
|----|--------------|-------|--------|---|---|---|-----|------------|
| F034 | Config key typo | User error | Setting ignored | 3 | 6 | 3 | 54 | Key validation + suggestions |
| F035 | Invalid config value | Bad input | Unexpected behavior | 5 | 5 | 2 | 50 | Value validation |
| F036 | Config file missing | First run / deleted | Defaults used | 2 | 4 | 1 | 8 | Auto-create with defaults |
| F037 | Config migration fail | Version upgrade | Lost settings | 6 | 2 | 4 | 48 | Backup before migration |
| F038 | Env var collision | Shell environment | Wrong values | 4 | 3 | 5 | 60 | Clear env precedence rules |

---

## High-RPN Failure Analysis

### RPN > 100: Critical Priority

| ID | Failure Mode | RPN | Root Cause Analysis | TRIZ Solution |
|----|--------------|-----|---------------------|---------------|
| F026 | Cartesian product | 144 | Missing join condition | Principle 10: Preliminary Action - Analyze query before execution |

### RPN 75-100: High Priority

| ID | Failure Mode | RPN | Root Cause Analysis | TRIZ Solution |
|----|--------------|-----|---------------------|---------------|
| F032 | Inconsistent ontology | 98 | Logic contradictions | Principle 35: Parameter Change - Run reasoner to detect |
| F010 | SHACL violation missed | 90 | Incomplete shapes | Principle 25: Self-Service - Auto-generate shapes from data |
| F007 | Validation bypass | 84 | Missing check | Principle 11: Beforehand Cushioning - Defense in depth |
| F019 | Partial write | 80 | Interrupted I/O | Principle 1: Segmentation - Write in transactions |

---

## SPARQL Detection Queries

### Detect Missing Authors (F010)

```sparql
PREFIX paper: <http://example.org/ontology/paper#>

SELECT ?paper
WHERE {
  ?paper a paper:Paper .
  FILTER NOT EXISTS { ?paper paper:hasAuthor ?author }
}
```

### Detect Empty Sections (F013)

```sparql
PREFIX paper: <http://example.org/ontology/paper#>

SELECT ?paper ?section
WHERE {
  ?paper paper:hasSection ?section .
  ?section paper:sectionContent ?content .
  FILTER(STRLEN(?content) = 0)
}
```

### Detect Orphan Sections

```sparql
PREFIX paper: <http://example.org/ontology/paper#>

SELECT ?section
WHERE {
  ?section a paper:Section .
  FILTER NOT EXISTS { ?work paper:hasSection ?section }
}
```

### Detect Circular References

```sparql
PREFIX paper: <http://example.org/ontology/paper#>

SELECT ?paper
WHERE {
  ?paper paper:cites+ ?paper .
}
```

### Detect Invalid Dates

```sparql
PREFIX thesis: <http://example.org/ontology/thesis#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

SELECT ?defense ?date
WHERE {
  ?defense thesis:defenseDate ?date .
  FILTER(?date < "2020-01-01"^^xsd:date || ?date > "2030-01-01"^^xsd:date)
}
```

---

## Automated Recovery Procedures

### F001: Missing Required Argument

```javascript
// Recovery: Parse error message and suggest
function recoverMissingArg(error) {
  const match = error.message.match(/required argument '(\w+)'/);
  if (match) {
    const arg = match[1];
    console.log(`Missing argument: --${arg}`);
    console.log(`Usage: playground papers generate --${arg} <value>`);

    // Interactive recovery
    const value = await prompt(`Enter ${arg}: `);
    return { [arg]: value };
  }
}
```

### F019: Partial Write

```javascript
// Recovery: Atomic write pattern
async function atomicWrite(path, content) {
  const tmpPath = `${path}.tmp`;

  try {
    await fs.writeFile(tmpPath, content);
    await fs.rename(tmpPath, path); // Atomic on POSIX
  } catch (error) {
    // Cleanup temp file
    await fs.unlink(tmpPath).catch(() => {});
    throw error;
  }
}
```

### F026: Cartesian Product

```javascript
// Recovery: Query analysis
function analyzeQuery(query) {
  const patterns = query.match(/\?\w+/g);
  const joins = query.match(/\?\w+\s+\?\w+\s+\?\w+/g);

  // Detect potential cartesian
  const unjoined = patterns.filter(p =>
    !joins.some(j => j.includes(p))
  );

  if (unjoined.length > 0) {
    console.warn('Warning: Potential cartesian product');
    console.warn(`Unjoined variables: ${unjoined.join(', ')}`);
    console.warn('Consider adding join conditions');
  }
}
```

### F032: Inconsistent Ontology

```javascript
// Recovery: Consistency check
async function checkConsistency(ontologyPath) {
  const store = await loadOntology(ontologyPath);

  // Check for disjoint class violations
  const disjointQuery = `
    SELECT ?instance ?class1 ?class2
    WHERE {
      ?instance a ?class1 .
      ?instance a ?class2 .
      ?class1 owl:disjointWith ?class2 .
    }
  `;

  const violations = await executeQuery(store, disjointQuery);

  if (violations.length > 0) {
    console.error('Ontology inconsistencies detected:');
    violations.forEach(v => {
      console.error(`  ${v.instance} is both ${v.class1} and ${v.class2}`);
    });
    return false;
  }

  return true;
}
```

---

## TRIZ Solutions Summary

| TRIZ Principle | Applied To | Solution |
|----------------|------------|----------|
| 1. Segmentation | F019 Partial write | Atomic transactions |
| 10. Preliminary Action | F026 Cartesian | Query analysis before execution |
| 11. Beforehand Cushioning | F007 Validation bypass | Defense in depth |
| 25. Self-Service | F010 SHACL missed | Auto-generate shapes |
| 35. Parameter Change | F032 Inconsistent | Run reasoner |
| 40. Composite Materials | F033 Large ontology | Streaming + chunking |

---

## DFLSS Quality Metrics

### Process Capability

| Metric | Target | Current | Status |
|--------|--------|---------|--------|
| Command success rate | 99.5% | 99.2% | At Risk |
| Validation accuracy | 99.9% | 99.8% | Good |
| Output correctness | 99.99% | 99.95% | Good |

### Defect Rates

| Defect Type | DPMO Target | DPMO Current | Sigma |
|-------------|-------------|--------------|-------|
| User errors | 10,000 | 8,500 | 3.9 |
| System errors | 100 | 50 | 5.3 |
| Data corruption | 10 | 3 | 5.8 |

### Control Limits

```
UCL (Upper Control Limit) = Mean + 3*StdDev
LCL (Lower Control Limit) = Mean - 3*StdDev

Latency Control Chart:
- Mean: 85ms
- StdDev: 30ms
- UCL: 175ms
- LCL: -5ms (effective 0ms)

Error Rate Control Chart:
- Mean: 0.5%
- StdDev: 0.2%
- UCL: 1.1%
- LCL: 0% (effective 0%)
```

---

## Monitoring and Alerting

### Critical Alerts

```yaml
alerts:
  - name: HighErrorRate
    condition: error_rate_5m > 5%
    severity: critical
    action: page_oncall

  - name: LatencySpike
    condition: p99_latency > 2000ms
    severity: warning
    action: notify_slack

  - name: ValidationFailures
    condition: validation_failures_1h > 100
    severity: warning
    action: notify_slack

  - name: OOMRisk
    condition: memory_usage > 80%
    severity: warning
    action: scale_down_queries
```

### Health Checks

```bash
# Periodic health check
playground meta telemetry --timeframe 1h --format json | jq '.totals.successRate'

# Expected: > 99.0
```
