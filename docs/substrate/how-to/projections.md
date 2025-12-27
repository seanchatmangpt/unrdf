# How to Create Surface Projections

Projections transform universe state into surface-specific views: `UI = Π_ui(μ(O))`, `CLI = Π_cli(μ(O))`. Missing features become missing projections.

## Problem

You need to:

- Display universe state in different surfaces (CLI, UI, API, docs)
- Transform RDF quads into usable formats (JSON, Markdown, HTML)
- Keep projections in sync with universe state
- Calculate drift between surfaces
- Register custom transformations

## Solution

Use the projection module to define surface-specific views with SPARQL queries and custom transforms.

## Step-by-Step

### 1. Register a Simple Projection

```javascript
import { registerProjection } from '@unrdf/kgc-claude';

registerProjection({
  name: 'run-history-cli',
  surface: 'cli',
  query: `
    PREFIX kgcc: <http://kgc.io/claude/>
    SELECT ?run ?status ?hash ?time
    WHERE {
      GRAPH <http://kgc.io/RunCapsules> {
        ?run kgcc:runStatus ?status ;
             kgcc:runHash ?hash ;
             kgcc:timestamp ?time .
      }
    }
    ORDER BY DESC(?time)
    LIMIT 10
  `,
  format: 'json',
});

console.log('Projection registered: run-history-cli');
```

### 2. Execute a Projection

```javascript
import { project } from '@unrdf/kgc-claude';

const result = await project(store, 'run-history-cli');

console.log('Projection result:');
console.log('ID:', result.id);
console.log('Content hash:', result.contentHash);
console.log('Source hash:', result.sourceHash);
console.log('Timestamp:', result.timestamp_iso);
console.log('Data:', result.content);
```

### 3. Register Custom Transform

```javascript
import { registerTransform } from '@unrdf/kgc-claude';

// Register a transform function
registerTransform('format-cli-table', data => {
  if (!Array.isArray(data)) return data;

  // Convert SPARQL results to CLI table
  const headers = Object.keys(data[0] || {});
  const rows = data.map(row => headers.map(h => row[h]?.value || ''));

  let table = headers.join(' | ') + '\n';
  table += headers.map(() => '---').join(' | ') + '\n';
  table += rows.map(row => row.join(' | ')).join('\n');

  return table;
});

console.log('Transform registered: format-cli-table');
```

### 4. Use Transform in Projection

```javascript
registerProjection({
  name: 'run-history-table',
  surface: 'cli',
  query: `
    PREFIX kgcc: <http://kgc.io/claude/>
    SELECT ?run ?status ?time
    WHERE {
      GRAPH <http://kgc.io/RunCapsules> {
        ?run kgcc:runStatus ?status ;
             kgcc:timestamp ?time .
      }
    }
    ORDER BY DESC(?time)
  `,
  transform: 'format-cli-table',
  format: 'text',
});

const tableResult = await project(store, 'run-history-table');
console.log(tableResult.content);
// Output:
// run | status | time
// --- | --- | ---
// uuid-1 | completed | 2024-01-15T10:30:00Z
// uuid-2 | completed | 2024-01-15T10:25:00Z
```

### 5. Project from Multiple Graphs

```javascript
import { GRAPHS } from '@unrdf/kgc-claude';

registerProjection({
  name: 'system-overview',
  surface: 'ui',
  sourceGraphs: [GRAPHS.UNIVERSE, GRAPHS.RUN_CAPSULES, GRAPHS.WORK_ITEMS],
  query: `
    PREFIX kgcc: <http://kgc.io/claude/>
    SELECT ?type (COUNT(?item) AS ?count)
    WHERE {
      {
        GRAPH <http://kgc.io/RunCapsules> {
          ?item a ?type .
        }
      } UNION {
        GRAPH <http://kgc.io/WorkItems> {
          ?item a ?type .
        }
      }
    }
    GROUP BY ?type
  `,
  format: 'json',
});

const overview = await project(store, 'system-overview');
console.log('System overview:', overview.content);
```

### 6. Create Documentation Projection

```javascript
registerTransform('format-markdown-docs', data => {
  let md = '# API Documentation\n\n';

  data.forEach(item => {
    md += `## ${item.name.value}\n\n`;
    md += `**Type**: ${item.type.value}\n\n`;
    md += `${item.description.value}\n\n`;
    md += `---\n\n`;
  });

  return md;
});

registerProjection({
  name: 'api-docs',
  surface: 'doc',
  query: `
    PREFIX kgcc: <http://kgc.io/claude/>
    SELECT ?name ?type ?description
    WHERE {
      GRAPH <http://kgc.io/Universe> {
        ?api a kgcc:APIEndpoint ;
             kgcc:name ?name ;
             kgcc:type ?type ;
             kgcc:description ?description .
      }
    }
    ORDER BY ?name
  `,
  transform: 'format-markdown-docs',
  format: 'markdown',
});

const docs = await project(store, 'api-docs');
// Write to file
fs.writeFileSync('API.md', docs.content);
```

### 7. Calculate Projection Drift

```javascript
import { calculateProjectionDrift } from '@unrdf/kgc-claude';

// Project current state
const current = await project(store, 'run-history-cli');

// ... state changes occur ...

// Project new state
const updated = await project(store, 'run-history-cli');

// Calculate drift
const drift = calculateProjectionDrift(current, updated);

console.log('Projection drift:', drift);
// 0 = no change
// >0 = state divergence

if (drift > 0) {
  console.log('Projection needs refresh');
}
```

### 8. Get All Projections

```javascript
import { getProjections, getProjectionsForSurface } from '@unrdf/kgc-claude';

// All projections
const allProjections = getProjections();
console.log('Total projections:', allProjections.length);

// CLI-specific projections
const cliProjections = getProjectionsForSurface('cli');
console.log('CLI projections:', cliProjections.length);

cliProjections.forEach(proj => {
  console.log(`- ${proj.name} (${proj.format})`);
});
```

## Advanced Patterns

### Live Projection Updates

```javascript
class LiveProjection {
  constructor(projectionName, updateInterval = 5000) {
    this.name = projectionName;
    this.interval = updateInterval;
    this.currentResult = null;
    this.listeners = [];
  }

  start(store) {
    this.timer = setInterval(async () => {
      const newResult = await project(store, this.name);

      // Check if changed
      if (!this.currentResult || newResult.contentHash !== this.currentResult.contentHash) {
        this.currentResult = newResult;
        this.notify(newResult);
      }
    }, this.interval);
  }

  stop() {
    if (this.timer) {
      clearInterval(this.timer);
    }
  }

  onChange(callback) {
    this.listeners.push(callback);
  }

  notify(result) {
    this.listeners.forEach(cb => cb(result));
  }
}

// Usage
const liveRuns = new LiveProjection('run-history-cli', 2000);

liveRuns.onChange(result => {
  console.log('Runs updated:', result.content.length);
  displayInCLI(result.content);
});

liveRuns.start(store);
```

### Multi-Format Projection

```javascript
class MultiFormatProjector {
  constructor(baseConfig) {
    this.baseConfig = baseConfig;
  }

  registerFormats(formats) {
    formats.forEach(format => {
      registerProjection({
        ...this.baseConfig,
        name: `${this.baseConfig.name}-${format}`,
        format: format,
        transform: `format-${format}`,
      });
    });
  }

  async projectAll(store) {
    const formats = ['json', 'yaml', 'markdown', 'html'];
    const results = {};

    for (const format of formats) {
      const projName = `${this.baseConfig.name}-${format}`;
      try {
        results[format] = await project(store, projName);
      } catch (error) {
        console.error(`Failed to project ${format}:`, error.message);
      }
    }

    return results;
  }
}

// Register transforms
registerTransform('format-yaml', data => jsyaml.dump(data));
registerTransform('format-html', data => `<pre>${JSON.stringify(data, null, 2)}</pre>`);

const projector = new MultiFormatProjector({
  name: 'run-summary',
  surface: 'api',
  query: 'SELECT ?run ?status WHERE { ?run kgcc:status ?status }',
});

projector.registerFormats(['json', 'yaml', 'markdown', 'html']);

const allFormats = await projector.projectAll(store);
```

### Cached Projections

```javascript
class ProjectionCache {
  constructor(ttl = 60000) {
    this.cache = new Map();
    this.ttl = ttl; // milliseconds
  }

  async get(store, projectionName) {
    const cached = this.cache.get(projectionName);

    if (cached && Date.now() - cached.timestamp < this.ttl) {
      console.log('Cache hit:', projectionName);
      return cached.result;
    }

    console.log('Cache miss:', projectionName);
    const result = await project(store, projectionName);

    this.cache.set(projectionName, {
      result,
      timestamp: Date.now(),
    });

    return result;
  }

  invalidate(projectionName) {
    this.cache.delete(projectionName);
  }

  clear() {
    this.cache.clear();
  }
}

const cache = new ProjectionCache(30000); // 30s TTL

// Use cached projection
const result = await cache.get(store, 'run-history-cli');
```

### Projection Composition

```javascript
async function composeProjections(store, projectionNames) {
  const results = await Promise.all(projectionNames.map(name => project(store, name)));

  return {
    id: crypto.randomUUID(),
    projections: projectionNames,
    data: results.reduce((acc, result, index) => {
      acc[projectionNames[index]] = result.content;
      return acc;
    }, {}),
    compositeHash: await blake3(JSON.stringify(results.map(r => r.contentHash))),
  };
}

// Compose multiple views
const composite = await composeProjections(store, [
  'run-history-cli',
  'work-items-summary',
  'system-overview',
]);

console.log('Composite projection:', composite.projections);
```

### Incremental Projection Updates

```javascript
class IncrementalProjection {
  constructor(projectionConfig) {
    this.config = projectionConfig;
    this.lastSourceHash = null;
    this.lastResult = null;
  }

  async update(store) {
    // Get current source hash
    const tempResult = await project(store, this.config);

    // Check if source changed
    if (this.lastSourceHash === tempResult.sourceHash) {
      console.log('Source unchanged, returning cached result');
      return this.lastResult;
    }

    // Source changed, update
    console.log('Source changed, recomputing projection');
    this.lastSourceHash = tempResult.sourceHash;
    this.lastResult = tempResult;

    return tempResult;
  }
}

const incrementalProj = new IncrementalProjection({ name: 'run-history-cli' });

// First call: computes
const result1 = await incrementalProj.update(store);

// Second call: returns cached if source unchanged
const result2 = await incrementalProj.update(store);
```

## Best Practices

1. **Use specific SPARQL queries**: Filter at source, not in transform
2. **Register transforms once**: Don't re-register on every projection
3. **Cache expensive projections**: Reduce computation overhead
4. **Version projection schemas**: Track breaking changes
5. **Monitor drift**: Detect stale views
6. **Batch updates**: Don't project after every delta
7. **Test transforms**: Ensure correct output format

## Common Issues

**Issue**: Projection returns empty content

- **Cause**: SPARQL query doesn't match any data
- **Fix**: Verify graph URIs, test query in SPARQL playground

**Issue**: Transform not found

- **Cause**: Transform not registered before projection
- **Fix**: Call registerTransform before registerProjection

**Issue**: Projection slow

- **Cause**: Large result set or expensive transform
- **Fix**: Add LIMIT to query, optimize transform, use caching

**Issue**: Content hash changes despite same data

- **Cause**: Non-deterministic ordering in query
- **Fix**: Add ORDER BY to SPARQL query

## See Also

- [API Reference: Projection](../reference.md#projection)
- [Explanation: Why Surface Projections](../explanation.md#surface-projections)
- [How-To: Verify Receipt Chains](./verify-receipts.md)
