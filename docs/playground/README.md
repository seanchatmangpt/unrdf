# 🎮 unrdf Playground

**Interactive web-based runtime environment for Knowledge Hooks development and testing**

## Overview

The unrdf Playground is a modern web application that provides a complete development environment for Knowledge Hooks. It offers both a REST API and a beautiful web interface for creating, evaluating, and monitoring hooks in real-time.

## Features

- 🖥️ **Modern Web Interface**: Clean, intuitive UI for hook management
- 🚀 **Real-time Evaluation**: Live hook execution with instant results
- 📊 **Visual Monitoring**: Real-time charts and metrics visualization
- 🔗 **REST API**: Complete API for programmatic hook management
- 💾 **Data Management**: Built-in RDF data source handling
- 📋 **Audit Trails**: Complete provenance tracking for all operations
- 🔒 **Cryptographic Security**: All evaluations are cryptographically signed
- ⚡ **High Performance**: Sub-millisecond hook evaluation

## Quick Start

### Using the Web Interface

1. **Start the Playground**
   ```bash
   cd playground
   pnpm install
   pnpm dev
   ```

2. **Open Your Browser**
   Navigate to `http://localhost:3000`

3. **Create Your First Hook**
   - Click "New Hook" in the interface
   - Use the form to define your hook
   - Test with sample data

### Using the REST API

The playground provides a complete REST API for programmatic access:

#### Create a Hook
```bash
curl -X POST http://localhost:3000/api/hooks \
  -H "Content-Type: application/json" \
  -d '{
    "id": "ex:ServiceHealthMonitor",
    "name": "Service Health Monitor",
    "description": "Monitors service error rates",
    "select": "SELECT ?service ?errorRate WHERE { ?service ex:errorRate ?errorRate }",
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

#### Evaluate a Hook
```bash
curl -X POST http://localhost:3000/api/hooks/ex:ServiceHealthMonitor/evaluate \
  -H "Content-Type: application/json" \
  -d '{
    "data": "@prefix ex: <http://example.org/> . ex:service1 ex:errorRate 0.05 ."
  }'
```

## Architecture

### Technology Stack

- **Frontend**: Vanilla HTML/CSS/JavaScript with modern UI components
- **Backend**: Express.js server with full RDF support
- **RDF Engine**: N3.js store with Comunica SPARQL engine
- **Real-time**: Server-Sent Events for live updates
- **Data Format**: Native support for Turtle, N-Quads, JSON-LD

### System Components

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   Web Interface │    │   REST API      │    │   Hooks Engine  │
│                 │    │                 │    │                 │
│ • Hook Creation │◄──►│ • /api/hooks    │◄──►│ • Predicates    │
│ • Evaluation    │    │ • /api/data     │    │ • Composables   │
│ • Monitoring    │    │ • /api/runtime  │    │ • Store Context │
└─────────────────┘    └─────────────────┘    └─────────────────┘
```

## Web Interface Guide

### Dashboard

The main dashboard provides an overview of:
- Active hooks and their status
- Recent evaluations and results
- System performance metrics
- Quick actions for common tasks

### Hook Management

#### Creating Hooks
1. Click "New Hook" or use the hook creation form
2. Provide basic information (ID, name, description)
3. Define the SPARQL query to monitor
4. Configure predicates and combination logic
5. Set up output formatting and delivery
6. Save the hook

#### Hook Editor
The visual hook editor provides:
- Syntax highlighting for SPARQL queries
- Real-time validation of hook definitions
- Preview of query results
- Predicate configuration with live feedback
- Output schema definition with Zod validation

### Data Management

#### Loading Data
- Upload RDF files (Turtle, N-Quads, JSON-LD)
- Paste RDF data directly into the interface
- Import from URLs or local files
- Built-in sample datasets for testing

#### Data Browser
- Explore loaded RDF data visually
- Execute ad-hoc SPARQL queries
- View data statistics and graphs
- Export data in various formats

### Real-time Monitoring

#### Hook Evaluation
- Execute hooks on demand or automatically
- View detailed evaluation results
- Monitor performance metrics
- Track evaluation history

#### Live Updates
- Real-time hook status updates
- Live evaluation results streaming
- Performance monitoring charts
- Audit trail visualization

## REST API Reference

### Authentication

The playground uses simple API key authentication:

```bash
curl -H "Authorization: Bearer your-api-key" \
  http://localhost:3000/api/hooks
```

### Endpoints

#### Hooks Management

**GET /api/hooks**
- List all hooks
- Query parameters: `limit`, `offset`, `status`

**POST /api/hooks**
- Create a new hook
- Body: Hook definition object

**GET /api/hooks/:id**
- Get hook by ID
- Returns: Hook definition and metadata

**PUT /api/hooks/:id**
- Update hook definition
- Body: Updated hook object

**DELETE /api/hooks/:id**
- Delete hook
- Returns: Success confirmation

**POST /api/hooks/:id/evaluate**
- Evaluate hook with optional data
- Body: `{ data?: string, options?: object }`
- Returns: HookReceipt object

**GET /api/hooks/:id/receipts**
- Get hook evaluation history
- Query parameters: `limit`, `since`, `until`

#### Data Management

**POST /api/data**
- Load RDF data into the store
- Body: `{ data: string, format?: 'turtle'|'nquads'|'jsonld' }`

**GET /api/data**
- Query current data
- Query parameters: `limit`, `format`

**POST /api/data/query**
- Execute SPARQL query
- Body: `{ query: string, format?: 'json'|'turtle' }`

**DELETE /api/data**
- Clear all data from store

#### Runtime Monitoring

**GET /api/runtime/status**
- Get system status and metrics
- Returns: CPU, memory, active hooks, etc.

**GET /api/runtime/metrics**
- Get detailed performance metrics
- Query parameters: `period`, `metric`

**GET /api/runtime/events**
- Stream real-time events (Server-Sent Events)
- Use for live monitoring

### API Response Formats

#### Hook Object
```json
{
  "id": "ex:ServiceHealthMonitor",
  "name": "Service Health Monitor",
  "description": "Monitors service error rates",
  "select": "SELECT ?service ?errorRate WHERE { ?service ex:errorRate ?errorRate }",
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
  "combine": "OR",
  "createdAt": "2025-09-25T10:00:00Z",
  "lastEvaluated": "2025-09-25T10:05:00Z",
  "status": "active"
}
```

#### HookReceipt Object
```json
{
  "id": "ex:ServiceHealthMonitor",
  "fired": true,
  "predicates": [
    {
      "kind": "THRESHOLD",
      "ok": true,
      "meta": {
        "matched": 2,
        "threshold": 0.02,
        "maxValue": 0.05
      },
      "duration": 12
    }
  ],
  "durations": {
    "totalMs": 24,
    "queryMs": 8,
    "predicateMs": 12,
    "canonicalizationMs": 4
  },
  "provenance": {
    "hookHash": "sha256:...",
    "queryHash": "sha256:...",
    "graphHash": "sha256:...",
    "receiptHash": "sha256:..."
  },
  "at": "2025-09-25T10:05:00Z",
  "input": {
    "bindings": 5,
    "variables": ["service", "errorRate"]
  }
}
```

## Configuration

### Environment Variables

```bash
# Server configuration
PORT=3000
HOST=localhost

# Security
API_KEY=your-secure-api-key
JWT_SECRET=your-jwt-secret

# Performance
MAX_CONCURRENT_EVALUATIONS=10
EVALUATION_TIMEOUT=5000
MAX_QUERY_RESULTS=10000

# Data storage
DATA_DIR=./data
MAX_STORE_SIZE=100MB

# Logging
LOG_LEVEL=info
LOG_FILE=./logs/playground.log
```

### Configuration Files

The playground can be configured via `playground.config.mjs`:

```javascript
export default {
  server: {
    port: 3000,
    host: 'localhost',
    cors: {
      origin: ['http://localhost:3000'],
      credentials: true
    }
  },
  security: {
    apiKey: process.env.API_KEY,
    jwtSecret: process.env.JWT_SECRET
  },
  performance: {
    maxConcurrentEvaluations: 10,
    evaluationTimeout: 5000,
    maxQueryResults: 10000
  },
  data: {
    maxStoreSize: '100MB',
    persistence: {
      enabled: false,
      path: './data'
    }
  },
  logging: {
    level: 'info',
    file: './logs/playground.log'
  }
}
```

## Development

### Project Structure

```
playground/
├── server/                 # Backend server code
│   ├── api/               # API route handlers
│   ├── core/              # Core business logic
│   └── utils/             # Utility functions
├── public/                # Frontend static files
│   ├── css/               # Stylesheets
│   ├── js/                # JavaScript modules
│   └── assets/            # Images and other assets
├── config/                # Configuration files
├── data/                  # Sample data and storage
└── test/                  # Tests and fixtures
```

### Adding New Features

1. **Frontend Features**
   - Add HTML templates to `public/`
   - Add JavaScript modules to `public/js/`
   - Add styles to `public/css/`

2. **Backend Features**
   - Add API routes to `server/api/`
   - Add business logic to `server/core/`
   - Add utilities to `server/utils/`

3. **Testing**
   - Add tests to `test/`
   - Update fixtures in `test/fixtures/`
   - Add integration tests for new features

### Building for Production

```bash
cd playground
pnpm build

# Preview production build
pnpm preview

# Deploy to production
pnpm start
```

## Monitoring and Debugging

### Performance Monitoring

The playground provides built-in performance monitoring:

- **Hook Evaluation Times**: Track individual hook performance
- **System Metrics**: CPU, memory, and storage usage
- **Query Performance**: SPARQL query execution times
- **Network Metrics**: API response times and throughput

### Debugging Tools

1. **Browser Developer Tools**: Full access to frontend debugging
2. **Server Logs**: Detailed backend logging with configurable levels
3. **API Testing**: Built-in API testing interface
4. **Query Debugger**: Visual SPARQL query debugging
5. **Hook Inspector**: Step-through hook evaluation debugging

### Health Checks

The playground includes comprehensive health checks:

```bash
# Check system health
curl http://localhost:3000/api/runtime/health

# Monitor system status
curl http://localhost:3000/api/runtime/status

# Check specific components
curl http://localhost:3000/api/runtime/health?component=hooks
curl http://localhost:3000/api/runtime/health?component=data
```

## Security Considerations

### Authentication and Authorization

- API key authentication for programmatic access
- JWT tokens for user sessions
- Role-based access control for hook management
- Input validation and sanitization

### Data Security

- All RDF data is processed in-memory
- Optional data persistence with encryption
- Secure file upload handling
- CORS configuration for cross-origin requests

### Network Security

- HTTPS enforcement in production
- Rate limiting for API endpoints
- Request size limits
- SQL injection protection for SPARQL queries

## Troubleshooting

### Common Issues

#### Hook Not Firing
1. Check SPARQL query syntax
2. Verify data is loaded correctly
3. Check predicate thresholds
4. Review evaluation logs

#### Performance Issues
1. Optimize SPARQL queries
2. Check system resource usage
3. Review hook evaluation frequency
4. Consider query result limits

#### API Errors
1. Verify API key authentication
2. Check request format and headers
3. Review server logs for detailed errors
4. Test with simple requests first

### Getting Help

1. **Check the Logs**: Server logs provide detailed error information
2. **Use the Debugger**: Built-in debugging tools for hook evaluation
3. **Test API**: Use the web interface to test API endpoints
4. **Review Documentation**: Complete API reference available in the interface
5. **Community Support**: GitHub issues and discussions for additional help

## Examples

### Monitoring Service Health

```javascript
// Create a service health monitoring hook
const hook = {
  id: 'ex:ServiceHealthMonitor',
  name: 'Service Health Monitor',
  description: 'Monitors error rates and latency',
  select: 'SELECT ?service ?errorRate ?latency WHERE { ?service ex:errorRate ?errorRate ; ex:latency ?latency }',
  predicates: [
    { kind: 'THRESHOLD', spec: { var: 'errorRate', op: '>', value: 0.02 } },
    { kind: 'THRESHOLD', spec: { var: 'latency', op: '>', value: 2000 } }
  ],
  combine: 'OR'
};

// Load service data
const data = `
@prefix ex: <http://example.org/> .
ex:service1 ex:errorRate 0.01 ; ex:latency 1500 .
ex:service2 ex:errorRate 0.05 ; ex:latency 2500 .
`;

// Evaluate and get results
const receipt = await evaluateHook(hook, { data });
console.log('Services with issues:', receipt.fired ? 'Found' : 'None');
```

### Compliance Monitoring

```javascript
// Create a compliance monitoring hook
const complianceHook = {
  id: 'ex:GDPRCompliance',
  name: 'GDPR Compliance Monitor',
  description: 'Ensures GDPR compliance',
  select: 'SELECT ?resource WHERE { ?resource ex:sensitive true }',
  predicates: [
    { kind: 'SHACL', spec: { shapes: 'ex:GDPRShape', strict: true } }
  ],
  combine: 'AND'
};

// Check compliance violations
const receipt = await evaluateHook(complianceHook);
if (receipt.fired) {
  console.log('Compliance violations found:', receipt.predicates);
}
```

## Contributing

The playground is built with modern web technologies and follows best practices for maintainable, scalable code. Contributions are welcome!

### Development Setup

1. Fork the repository
2. Clone your fork: `git clone <your-fork>`
3. Install dependencies: `cd playground && pnpm install`
4. Start development server: `pnpm dev`
5. Make your changes
6. Test thoroughly
7. Submit a pull request

### Code Style

- Use modern JavaScript (ES2020+)
- Follow consistent naming conventions
- Add JSDoc comments for public APIs
- Write tests for new features
- Maintain backward compatibility

## License

MIT License - see the main project LICENSE for details.

## Support

- **Documentation**: Complete API reference in the web interface
- **Examples**: Sample hooks and data for common use cases
- **Community**: GitHub issues for bug reports and feature requests
- **Discussions**: GitHub discussions for questions and ideas
