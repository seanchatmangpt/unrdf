# How-To: Assess Data Quality

**Problem**: You need to evaluate RDF graph quality, find data issues, and get recommendations for improvements.

## Solution

UNRDF provides quality utilities to assess completeness, consistency, and correctness. Use `assessDataQuality()` for overall scoring, then apply specific checks and fixes.

### Overall Quality Assessment

Get comprehensive quality metrics:

```javascript
import { assessDataQuality } from 'unrdf/utils';
import { parseTurtle } from 'unrdf';

const store = parseTurtle(`
  @prefix ex: <http://example.org/> .
  @prefix schema: <http://schema.org/> .

  ex:alice a schema:Person ;
    schema:name "Alice" ;
    schema:email "alice@example.org" .

  ex:bob a schema:Person ;
    schema:name "Bob" .
`);

const quality = assessDataQuality(store);

console.log(`Overall Score: ${quality.score}/100`);
console.log(`Completeness: ${quality.completeness}%`);
console.log(`Consistency: ${quality.consistency}%`);
console.log(`Correctness: ${quality.correctness}%`);
console.log(`Issues Found: ${quality.issues.length}`);

quality.issues.forEach(issue => {
  console.log(`- ${issue.severity}: ${issue.message}`);
});
```

### Find Broken Links

Detect references to non-existent resources:

```javascript
import { findBrokenLinks } from 'unrdf/utils';

const broken = findBrokenLinks(store);

if (broken.length > 0) {
  console.log(`Found ${broken.length} broken links:`);

  broken.forEach(link => {
    console.log(`IRI: ${link.iri}`);
    console.log(`Referenced by: ${link.referencedBy.length} quads`);
    console.log(`Type: ${link.type}`);  // 'subject', 'predicate', 'object'
  });
}

// Fix broken links
broken.forEach(link => {
  // Remove quads with broken links
  const quads = store.getQuads(null, null, link.node);
  quads.forEach(quad => store.removeQuad(quad));
});
```

### Find Dangling References

Detect orphaned resources:

```javascript
import { findDanglingReferences } from 'unrdf/utils';

const dangling = findDanglingReferences(store);

console.log(`Dangling references: ${dangling.length}`);

dangling.forEach(ref => {
  console.log(`Resource: ${ref.resource.value}`);
  console.log(`Referenced in: ${ref.properties.join(', ')}`);
  console.log(`No definition found`);
});

// Clean up dangling references
dangling.forEach(ref => {
  const quads = store.getQuads(null, null, ref.resource);
  quads.forEach(quad => {
    console.log(`Removing dangling reference: ${quad.subject.value}`);
    store.removeQuad(quad);
  });
});
```

### Quality Improvement Suggestions

Get actionable recommendations:

```javascript
import { suggestImprovements } from 'unrdf/utils';

const suggestions = suggestImprovements(store);

suggestions.forEach(suggestion => {
  console.log(`\nSuggestion: ${suggestion.title}`);
  console.log(`Priority: ${suggestion.priority}`);
  console.log(`Description: ${suggestion.description}`);
  console.log(`Impact: ${suggestion.impact}`);

  if (suggestion.examples) {
    console.log('Examples:');
    suggestion.examples.forEach(ex => console.log(`  - ${ex}`));
  }

  if (suggestion.fix) {
    console.log(`Fix: ${suggestion.fix}`);
  }
});

// Example output:
// Suggestion: Add missing labels
// Priority: high
// Description: 15 resources have no rdfs:label or schema:name
// Impact: Improves human readability
// Fix: Add rdfs:label or schema:name to all resources
```

### Completeness Checks

Verify required properties exist:

```javascript
import { select } from 'unrdf';

function checkCompleteness(store, requiredProps) {
  const issues = [];

  // Find resources missing required properties
  requiredProps.forEach(prop => {
    const query = `
      PREFIX schema: <http://schema.org/>

      SELECT ?resource WHERE {
        ?resource a ${prop.class} .
        FILTER NOT EXISTS { ?resource ${prop.property} ?value }
      }
    `;

    const missing = select(store, query);

    if (missing.length > 0) {
      issues.push({
        property: prop.property,
        class: prop.class,
        count: missing.length,
        resources: missing.map(r => r.resource.value)
      });
    }
  });

  return issues;
}

// Check required properties
const required = [
  { class: 'schema:Person', property: 'schema:name' },
  { class: 'schema:Person', property: 'schema:email' },
  { class: 'schema:Organization', property: 'schema:name' }
];

const issues = checkCompleteness(store, required);
console.log(`Completeness issues: ${issues.length}`);
```

### Consistency Checks

Find inconsistent data patterns:

```javascript
function checkConsistency(store) {
  const issues = [];

  // Check for duplicate names
  const duplicates = select(store, `
    PREFIX schema: <http://schema.org/>

    SELECT ?name (COUNT(?person) as ?count) WHERE {
      ?person schema:name ?name .
    }
    GROUP BY ?name
    HAVING (COUNT(?person) > 1)
  `);

  if (duplicates.length > 0) {
    issues.push({
      type: 'duplicate_names',
      count: duplicates.length,
      details: duplicates
    });
  }

  // Check for type inconsistencies
  const typeIssues = select(store, `
    SELECT ?resource (COUNT(?type) as ?typeCount) WHERE {
      ?resource a ?type .
    }
    GROUP BY ?resource
    HAVING (COUNT(?type) > 3)
  `);

  if (typeIssues.length > 0) {
    issues.push({
      type: 'multiple_types',
      count: typeIssues.length,
      details: typeIssues
    });
  }

  return issues;
}

const consistency = checkConsistency(store);
console.log(`Consistency issues: ${consistency.length}`);
```

### Correctness Validation

Validate data types and formats:

```javascript
function validateCorrectness(store) {
  const errors = [];

  // Check email format
  const invalidEmails = select(store, `
    PREFIX schema: <http://schema.org/>

    SELECT ?person ?email WHERE {
      ?person schema:email ?email .
      FILTER (!REGEX(str(?email), "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"))
    }
  `);

  if (invalidEmails.length > 0) {
    errors.push({
      type: 'invalid_email',
      count: invalidEmails.length,
      items: invalidEmails
    });
  }

  // Check URL format
  const invalidURLs = select(store, `
    PREFIX schema: <http://schema.org/>

    SELECT ?resource ?url WHERE {
      ?resource schema:url ?url .
      FILTER (!REGEX(str(?url), "^https?://"))
    }
  `);

  if (invalidURLs.length > 0) {
    errors.push({
      type: 'invalid_url',
      count: invalidURLs.length,
      items: invalidURLs
    });
  }

  return errors;
}

const correctness = validateCorrectness(store);
console.log(`Correctness errors: ${correctness.length}`);
```

## Variations

### Automated Quality Reports

Generate comprehensive quality reports:

```javascript
async function generateQualityReport(store) {
  const report = {
    timestamp: new Date().toISOString(),
    overall: assessDataQuality(store),
    completeness: checkCompleteness(store, requiredProps),
    consistency: checkConsistency(store),
    correctness: validateCorrectness(store),
    brokenLinks: findBrokenLinks(store),
    danglingRefs: findDanglingReferences(store),
    suggestions: suggestImprovements(store)
  };

  // Calculate overall grade
  const score = report.overall.score;
  report.grade = score >= 90 ? 'A' :
                 score >= 80 ? 'B' :
                 score >= 70 ? 'C' :
                 score >= 60 ? 'D' : 'F';

  return report;
}

const report = await generateQualityReport(store);
console.log(JSON.stringify(report, null, 2));
```

### Quality Gates

Enforce minimum quality standards:

```javascript
class QualityGate {
  constructor(minScore = 80) {
    this.minScore = minScore;
  }

  check(store) {
    const quality = assessDataQuality(store);

    if (quality.score < this.minScore) {
      throw new Error(
        `Quality gate failed: ${quality.score} < ${this.minScore}\n` +
        `Issues: ${quality.issues.map(i => i.message).join(', ')}`
      );
    }

    return { passed: true, score: quality.score };
  }
}

// Use in CI/CD
const gate = new QualityGate(80);

try {
  const result = gate.check(store);
  console.log(`✓ Quality gate passed: ${result.score}/100`);
} catch (err) {
  console.error(`✗ Quality gate failed: ${err.message}`);
  process.exit(1);
}
```

### Progressive Quality Improvement

Track quality over time:

```javascript
class QualityTracker {
  constructor() {
    this.history = [];
  }

  measure(store, label) {
    const quality = assessDataQuality(store);

    this.history.push({
      timestamp: Date.now(),
      label,
      score: quality.score,
      completeness: quality.completeness,
      consistency: quality.consistency,
      correctness: quality.correctness,
      issueCount: quality.issues.length
    });

    return quality;
  }

  getTrend() {
    if (this.history.length < 2) {
      return { trend: 'insufficient_data' };
    }

    const recent = this.history.slice(-5);
    const avg = recent.reduce((sum, m) => sum + m.score, 0) / recent.length;
    const first = recent[0].score;
    const last = recent[recent.length - 1].score;

    return {
      trend: last > first ? 'improving' : last < first ? 'degrading' : 'stable',
      averageScore: avg,
      change: last - first
    };
  }
}

const tracker = new QualityTracker();

// Measure over time
tracker.measure(store, 'v1.0');
// ... make improvements ...
tracker.measure(store, 'v1.1');

const trend = tracker.getTrend();
console.log(`Quality trend: ${trend.trend} (${trend.change > 0 ? '+' : ''}${trend.change})`);
```

### Quality-Based Filtering

Filter data by quality score:

```javascript
function filterByQuality(store, minQuality = 80) {
  const highQuality = new Store();

  // Assess each resource's quality
  const resources = select(store, `SELECT DISTINCT ?s WHERE { ?s ?p ?o }`);

  resources.forEach(row => {
    const resourceQuads = store.getQuads(row.s, null, null);
    const resourceStore = new Store(resourceQuads);

    const quality = assessDataQuality(resourceStore);

    if (quality.score >= minQuality) {
      resourceQuads.forEach(quad => highQuality.addQuad(quad));
    }
  });

  return highQuality;
}

// Keep only high-quality data
const filtered = filterByQuality(store, 80);
console.log(`Filtered: ${store.size} → ${filtered.size} quads`);
```

## Related Guides

- [How-To: Validate RDF Data](./validate-rdf-data.md) - SHACL validation
- [How-To: Query with SPARQL](./query-with-sparql.md) - Quality queries
- [How-To: Optimize Queries](./optimize-queries.md) - Quality vs performance
- [Reference: Quality Utilities](../reference/api/utilities.md#quality) - Complete API
