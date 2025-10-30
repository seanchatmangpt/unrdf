# UNRDF Playground - Hooks Runtime

This playground provides a web-based runtime for managing and executing UNRDF Knowledge Hooks.

## 🚀 Quick Start

### Start the Server (Nitro)
```bash
cd playground
pnpm server
```

Nitro dev will start on `http://localhost:3000`.

### Web Interface
Open `http://localhost:3000` in your browser to access the web interface for:
- Creating and managing hooks
- Evaluating hooks with real-time results
- Managing data sources
- Monitoring runtime status

## 🔧 API Endpoints

### Hooks Management
- `GET /api/hooks` - List all hooks
- `POST /api/hooks` - Create a new hook
- `GET /api/hooks/:id` - Get specific hook details
- `POST /api/hooks/:id/evaluate` - Evaluate a hook
- `POST /api/hooks/:id/plan` - Plan hook execution
- `DELETE /api/hooks/:id` - Delete a hook

### Data Management
- `GET /api/data` - List all data sources
- `POST /api/data` - Create a new data source
- `POST /api/data/:id/query` - Query a data source

### Runtime Status
- `GET /api/runtime/status` - Get runtime status

## 📝 Example Usage

### Create a Hook
```bash
curl -X POST http://localhost:3000/api/hooks \
  -H "Content-Type: application/json" \
  -d '{
    "id": "ex:ServiceHealthMonitor",
    "name": "Service Health Monitor",
    "select": "SELECT ?service ?errorRate WHERE { ?service <http://example.org/errorRate> ?errorRate }",
    "predicates": [
      {
        "kind": "THRESHOLD",
        "spec": {
          "var": "errorRate",
          "op": ">",
          "value": 0.02
        }
      }
    ],
    "combine": "OR"
  }'
```

### Evaluate a Hook
```bash
curl -X POST http://localhost:3000/api/hooks/ex:ServiceHealthMonitor/evaluate \
  -H "Content-Type: application/json" \
  -d '{}'
```

### Create Data Source
```bash
curl -X POST http://localhost:3000/api/data \
  -H "Content-Type: application/json" \
  -d '{
    "id": "sample-data",
    "name": "Sample Services",
    "content": "@prefix ex: <http://example.org/> .\nex:service1 a ex:Service ;\n  ex:errorRate 0.05 .",
    "format": "Turtle"
  }'
```

## 🎯 Features

- **Real-time Hook Evaluation**: Execute hooks with live results
- **Web Interface**: User-friendly interface for hook management
- **Data Source Management**: Load and query RDF data
- **Custom Predicates**: Register custom predicate types
- **Runtime Monitoring**: Track system status and performance
- **Composable Architecture**: Uses UNRDF's composable system

## 🔍 Available Predicates

- **ASK**: Boolean SPARQL queries
- **THRESHOLD**: Numeric comparisons on variables
- **DELTA**: Change detection based on hash digests
- **SHACL**: Shape validation (stub implementation)
- **WINDOW**: Aggregation operations (count, sum, avg)
- **Custom**: Register your own predicate types

## 🛠️ Development

### Run Examples
```bash
pnpm hooks          # Run hooks runtime demo
pnpm examples       # Run basic usage examples
pnpm sparql         # Run SPARQL examples
pnpm validation     # Run validation examples
```

### Testing
```bash
pnpm test           # Run all tests
pnpm test:watch     # Run tests in watch mode
pnpm test:ui        # Run tests with UI
```

## 📊 Architecture

The hooks runtime uses:
- **Express.js** for the web server
- **UNRDF Composables** for RDF operations
- **N3.js** for RDF store management
- **Comunica** for SPARQL query execution
- **In-memory storage** for hooks and data (development)

## 🔗 Integration

The runtime integrates seamlessly with:
- UNRDF's composable architecture
- Existing hook definitions from front-matter
- CLI tools and examples
- Custom predicate implementations

## 📈 Performance

- Fast hook evaluation (typically 20-50ms)
- Efficient memory usage
- Real-time status monitoring
- Deterministic hash-based provenance

---

**Ready to manage Knowledge Hooks!** 🎉