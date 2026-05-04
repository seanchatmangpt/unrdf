# @unrdf/yawl-api

High-performance REST API framework that exposes YAWL workflows as RESTful APIs with automatic OpenAPI documentation, HATEOAS hypermedia controls, and Zod-based validation.

## Features

- **Auto-generated REST Endpoints**: Automatically creates REST APIs from YAWL workflow definitions
- **OpenAPI 3.1 Documentation**: Full OpenAPI specification with interactive Swagger UI
- **HATEOAS Hypermedia**: Dynamic links based on workflow state guide API consumers
- **Zod Validation**: Type-safe request/response validation using Zod schemas
- **FastifyPowered**: Built on Fastify for maximum performance
- **Full Workflow Lifecycle**: Create cases, execute tasks, query state via REST

## Installation

```bash
pnpm add @unrdf/yawl-api
```

## Quick Start

```javascript
import { createYAWLAPIServer } from '@unrdf/yawl-api';
import { createWorkflowEngine, SPLIT_TYPE, JOIN_TYPE } from '@unrdf/yawl';

// Create workflow engine
const engine = createWorkflowEngine();

// Register a workflow
engine.registerWorkflow({
  id: 'approval',
  name: 'Approval Workflow',
  tasks: [
    { id: 'submit', name: 'Submit', splitType: SPLIT_TYPE.AND, joinType: JOIN_TYPE.XOR },
    { id: 'review', name: 'Review', splitType: SPLIT_TYPE.XOR, joinType: JOIN_TYPE.XOR },
    { id: 'approve', name: 'Approve', splitType: SPLIT_TYPE.AND, joinType: JOIN_TYPE.XOR },
  ],
  flows: [
    { from: 'submit', to: 'review' },
    { from: 'review', to: 'approve', condition: '(ctx) => ctx.approved === true' },
  ],
  startTaskId: 'submit',
  endTaskIds: ['approve'],
});

// Create API server
const server = await createYAWLAPIServer({
  engine,
  baseUrl: 'http://localhost:3000',
});

// Start server
await server.listen({ port: 3000 });
console.log('Server running at http://localhost:3000');
console.log('Swagger UI at http://localhost:3000/docs');
```

## API Endpoints

### Workflow Management

| Method | Endpoint | Description |
|--------|----------|-------------|
| `POST` | `/api/workflows` | Register a new workflow definition |
| `GET` | `/api/workflows` | List all registered workflows |
| `GET` | `/api/workflows/:workflowId` | Get workflow details |

### Case Management

| Method | Endpoint | Description |
|--------|----------|-------------|
| `POST` | `/api/workflows/:workflowId/cases` | Create a new case instance |
| `GET` | `/api/cases` | List all cases (with filters) |
| `GET` | `/api/cases/:caseId` | Get case details with HATEOAS links |

### Task Execution

| Method | Endpoint | Description |
|--------|----------|-------------|
| `POST` | `/api/cases/:caseId/tasks/:workItemId/start` | Start a work item |
| `POST` | `/api/cases/:caseId/tasks/:workItemId/complete` | Complete a work item |
| `POST` | `/api/cases/:caseId/tasks/:workItemId/cancel` | Cancel a work item |

### Health & Documentation

| Method | Endpoint | Description |
|--------|----------|-------------|
| `GET` | `/health` | Health check endpoint |
| `GET` | `/docs` | Swagger UI (interactive API documentation) |
| `GET` | `/docs/json` | OpenAPI 3.1 specification (JSON) |

## HATEOAS Hypermedia Controls

The API follows HATEOAS principles. Every case response includes `_links` that show available actions based on the current workflow state.

### Example Response

```json
{
  "id": "case-123",
  "workflowId": "approval",
  "status": "running",
  "data": { "amount": 1500 },
  "_links": {
    "self": {
      "href": "http://localhost:3000/api/cases/case-123",
      "method": "GET"
    },
    "workflow": {
      "href": "http://localhost:3000/api/workflows/approval",
      "method": "GET"
    },
    "enabledTasks": [
      {
        "taskId": "review",
        "workItemId": "wi-456",
        "name": "Review",
        "actions": {
          "start": {
            "href": "http://localhost:3000/api/cases/case-123/tasks/wi-456/start",
            "method": "POST"
          },
          "cancel": {
            "href": "http://localhost:3000/api/cases/case-123/tasks/wi-456/cancel",
            "method": "POST"
          }
        }
      }
    ]
  }
}
```

## Complete Example: Purchase Order API

```javascript
// See: src/examples/purchase-order-api.mjs

import { createYAWLAPIServer } from '@unrdf/yawl-api';
import { createWorkflowEngine, SPLIT_TYPE, JOIN_TYPE } from '@unrdf/yawl';

const engine = createWorkflowEngine();

// Define purchase order workflow
engine.registerWorkflow({
  id: 'purchase-order',
  name: 'Purchase Order Approval',
  tasks: [
    { id: 'submit', name: 'Submit PO', splitType: SPLIT_TYPE.AND, joinType: JOIN_TYPE.XOR },
    { id: 'review', name: 'Review PO', splitType: SPLIT_TYPE.XOR, joinType: JOIN_TYPE.XOR },
    { id: 'approve', name: 'Approve', splitType: SPLIT_TYPE.AND, joinType: JOIN_TYPE.XOR },
    { id: 'reject', name: 'Reject', splitType: SPLIT_TYPE.AND, joinType: JOIN_TYPE.XOR },
  ],
  flows: [
    { from: 'submit', to: 'review' },
    { from: 'review', to: 'approve', condition: '(ctx) => ctx.decision === "approve"' },
    { from: 'review', to: 'reject', condition: '(ctx) => ctx.decision === "reject"', isDefault: true },
  ],
  startTaskId: 'submit',
  endTaskIds: ['approve', 'reject'],
});

const server = await createYAWLAPIServer({ engine });
await server.listen({ port: 3000 });
```

### Usage

```bash
# 1. Create a purchase order case
curl -X POST http://localhost:3000/api/workflows/purchase-order/cases \
  -H "Content-Type: application/json" \
  -d '{
    "initialData": {
      "amount": 1500,
      "vendor": "Acme Corp",
      "description": "Office supplies"
    }
  }'

# Response includes caseId and HATEOAS links

# 2. Get case details (follow self link)
curl http://localhost:3000/api/cases/<caseId>

# Response shows enabledTasks with action links

# 3. Start review task (follow start link)
curl -X POST http://localhost:3000/api/cases/<caseId>/tasks/<workItemId>/start \
  -H "Content-Type: application/json" \
  -d '{"actor": "manager@example.com"}'

# 4. Complete review with decision
curl -X POST http://localhost:3000/api/cases/<caseId>/tasks/<workItemId>/complete \
  -H "Content-Type: application/json" \
  -d '{
    "actor": "manager@example.com",
    "output": {
      "decision": "approve",
      "reviewedBy": "manager@example.com"
    }
  }'

# Workflow automatically enables approve task based on decision
```

## API Conventions

### Request Format

All POST/PUT requests accept JSON payloads:

```json
{
  "initialData": { /* case data */ },
  "actor": "user@example.com",
  "output": { /* task output */ }
}
```

### Response Format

All responses follow a consistent structure:

```json
{
  "case": { /* case data */ },
  "receipt": { /* YAWL receipt with proofchain */ },
  "_links": { /* HATEOAS hypermedia controls */ }
}
```

### Error Handling

Errors return appropriate HTTP status codes with details:

```json
{
  "error": "Case not found",
  "statusCode": 404
}
```

| Status Code | Description |
|-------------|-------------|
| `200` | Success |
| `201` | Created |
| `400` | Bad Request (validation error) |
| `404` | Not Found |
| `500` | Internal Server Error |

## Configuration Options

```javascript
const server = await createYAWLAPIServer({
  // YAWL engine instance (optional, creates default if not provided)
  engine: createWorkflowEngine(),

  // Base URL for HATEOAS links
  baseUrl: 'http://localhost:3000',

  // Enable/disable Swagger UI
  enableSwagger: true,

  // Fastify server options
  fastifyOptions: {
    logger: true,
    requestTimeout: 30000,
  },

  // Swagger customization
  swaggerOptions: {
    openapi: {
      info: {
        title: 'My Workflow API',
        version: '1.0.0',
      },
    },
    ui: {
      routePrefix: '/docs',
    },
  },
});
```

## OpenAPI Documentation

Access interactive API documentation at `/docs` when the server is running. The OpenAPI specification is automatically generated from:

- Workflow definitions
- Zod validation schemas
- YAWL task schemas
- HATEOAS link structures

## Testing

```bash
# Run all tests
pnpm test

# Run with coverage
pnpm test:coverage

# Run in watch mode
pnpm test:watch

# Lint code
pnpm lint
```

## Architecture

```
┌─────────────────┐
│  Fastify HTTP   │ ← REST API Layer
├─────────────────┤
│  YAWL API       │ ← Endpoint Generation + HATEOAS
│  Server         │
├─────────────────┤
│  YAWL Engine    │ ← Workflow Execution Engine
├─────────────────┤
│  YAWL Core      │ ← Petri Net Semantics
└─────────────────┘
```

### Key Components

1. **YAWLAPIServer**: Main server class that auto-generates REST endpoints
2. **HATEOAS Generator**: Creates hypermedia links based on workflow state
3. **Zod Validators**: Request/response validation schemas
4. **OpenAPI Generator**: Automatic OpenAPI 3.1 spec generation
5. **Workflow Engine**: YAWL execution engine with receipt verification

## Performance

Built on Fastify, one of the fastest Node.js web frameworks:

- Up to 30,000 req/sec for simple endpoints
- Sub-millisecond latency for case queries
- Efficient HATEOAS link generation
- Streaming support for large result sets

## Integration with YAWL Ecosystem

This package integrates seamlessly with other YAWL packages:

- `@unrdf/yawl`: Core workflow engine
- `@unrdf/yawl-viz`: Workflow visualization
- `@unrdf/yawl-observability`: OpenTelemetry tracing
- `@unrdf/yawl-durable`: Persistent workflow execution

## License

MIT

## Contributing

Contributions welcome! Please read the contributing guidelines before submitting PRs.

## Support

- Documentation: https://unrdf.dev/docs/yawl-api
- Issues: https://github.com/unrdf/unrdf/issues
- Discord: https://discord.gg/unrdf
