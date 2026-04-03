# MCP (Model Context Protocol) Server

This package provides a Model Context Protocol (MCP) server implementation for the UNRDF Daemon, enabling LLM-based clients to interact with RDF graphs and SPARQL endpoints through a standardized interface.

## Overview

The MCP server exposes three main categories of capabilities:

1. **Tools** - Executable operations for interacting with RDF data
2. **Resources** - Static or dynamic resources providing access to configuration and metadata
3. **Prompts** - Pre-defined prompts that guide LLMs in common RDF tasks

## Tools

### list_endpoints

Lists all available SPARQL endpoints.

**Input Schema:**
```json
{
  "type": "object",
  "properties": {},
  "required": []
}
```

**Example:**
```javascript
import { createMCPServer } from '@unrdf/daemon/mcp';

const server = createMCPServer();
// Tool will return available endpoints
```

### execute_sparql

Execute a SPARQL query against a specified endpoint.

**Input Schema:**
```json
{
  "type": "object",
  "properties": {
    "endpoint": {
      "type": "string",
      "description": "SPARQL endpoint URL"
    },
    "query": {
      "type": "string",
      "description": "SPARQL query string"
    },
    "format": {
      "type": "string",
      "enum": ["json", "xml", "csv"],
      "description": "Result format",
      "default": "json"
    }
  },
  "required": ["endpoint", "query"]
}
```

**Example:**
```javascript
// Tool invocation
{
  "name": "execute_sparql",
  "arguments": {
    "endpoint": "http://localhost:8080/sparql",
    "query": "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10",
    "format": "json"
  }
}
```

### get_graph_stats

Get statistics about an RDF graph.

**Input Schema:**
```json
{
  "type": "object",
  "properties": {
    "graph_iri": {
      "type": "string",
      "description": "IRI of the RDF graph"
    }
  },
  "required": ["graph_iri"]
}
```

**Example:**
```javascript
{
  "name": "get_graph_stats",
  "arguments": {
    "graph_iri": "http://example.org/default"
  }
}
```

**Returns:**
```json
{
  "graph_iri": "http://example.org/default",
  "triple_count": 1000,
  "subject_count": 100,
  "predicate_count": 50,
  "object_count": 200
}
```

### load_rdf_data

Load RDF data from a file.

**Input Schema:**
```json
{
  "type": "object",
  "properties": {
    "file_path": {
      "type": "string",
      "description": "Path to the RDF file"
    },
    "format": {
      "type": "string",
      "enum": ["turtle", "rdfxml", "ntriples", "jsonld"],
      "description": "RDF format"
    },
    "target_graph": {
      "type": "string",
      "description": "Target named graph IRI"
    }
  },
  "required": ["file_path", "format"]
}
```

**Example:**
```javascript
{
  "name": "load_rdf_data",
  "arguments": {
    "file_path": "/path/to/data.ttl",
    "format": "turtle",
    "target_graph": "http://example.org/imported"
  }
}
```

## Resources

Resources provide access to configuration, metadata, and templates.

### sparql://endpoints/config

Configuration for SPARQL endpoints.

**MIME Type:** `application/json`

**Content Example:**
```json
{
  "endpoints": [
    { "name": "default", "url": "http://localhost:8080/sparql" },
    { "name": "public", "url": "http://dbpedia.org/sparql" }
  ]
}
```

### rdf://ontologies/catalog

Catalog of available RDF ontologies.

**MIME Type:** `application/json`

**Content Example:**
```json
{
  "ontologies": [
    { "name": "SKOS", "iri": "http://www.w3.org/2004/02/skos/core#" },
    { "name": "FOAF", "iri": "http://xmlns.com/foaf/0.1/" }
  ]
}
```

### graphs://metadata

Metadata about available RDF graphs.

**MIME Type:** `application/json`

**Content Example:**
```json
{
  "graphs": [
    { "iri": "http://example.org/default", "triples": 5000 },
    { "iri": "http://example.org/public", "triples": 10000 }
  ]
}
```

### queries://templates

Reusable SPARQL query templates.

**MIME Type:** `application/json`

**Content Example:**
```json
{
  "templates": [
    {
      "name": "all_subjects",
      "query": "SELECT DISTINCT ?s WHERE { ?s ?p ?o }"
    },
    {
      "name": "all_predicates",
      "query": "SELECT DISTINCT ?p WHERE { ?s ?p ?o }"
    }
  ]
}
```

## Prompts

Prompts guide LLMs in common RDF tasks.

### sparql_builder

Build SPARQL queries for RDF graph exploration.

**Arguments:**
- `task` (required): The query task to accomplish
- `graph` (optional): Target RDF graph IRI

**Example:**
```javascript
{
  "name": "sparql_builder",
  "arguments": {
    "task": "Find all subjects in the default graph",
    "graph": "http://example.org/default"
  }
}
```

### graph_analysis

Analyze RDF graph structure and content.

**Arguments:**
- `analysis_type` (required): Type of analysis (structure, patterns, quality, coverage)
- `graph_iri` (required): IRI of the graph to analyze

**Example:**
```javascript
{
  "name": "graph_analysis",
  "arguments": {
    "analysis_type": "structure",
    "graph_iri": "http://example.org/default"
  }
}
```

### data_transform

Transform RDF data between formats.

**Arguments:**
- `source_format` (required): Source RDF format
- `target_format` (required): Target RDF format
- `mapping` (optional): Optional transformation mapping

**Example:**
```javascript
{
  "name": "data_transform",
  "arguments": {
    "source_format": "turtle",
    "target_format": "jsonld"
  }
}
```

### ontology_doc

Generate documentation for RDF ontologies.

**Arguments:**
- `ontology_iri` (required): IRI of the ontology
- `detail_level` (optional): Detail level (brief, standard, comprehensive)

**Example:**
```javascript
{
  "name": "ontology_doc",
  "arguments": {
    "ontology_iri": "http://www.w3.org/2004/02/skos/core#",
    "detail_level": "standard"
  }
}
```

## Server Startup

### Using Stdio Transport

```javascript
import { startMCPServer } from '@unrdf/daemon/mcp';

await startMCPServer();
```

This connects the MCP server to stdin/stdout for communication with the client.

### Using SSE Transport (HTTP)

```javascript
import { startMCPServerSSE } from '@unrdf/daemon/mcp';

const httpServer = await startMCPServerSSE(8765);
```

This starts an HTTP server with Server-Sent Events transport on the specified port.

## CLI Usage

### Start the MCP server

```bash
unrdf mcp start --transport stdio
unrdf mcp start --transport sse --port 8765
```

### Inspect server capabilities

```bash
unrdf mcp inspect
```

### Check server status

```bash
unrdf mcp status
```

## Integration with LLMs

The MCP server can be used with LLM clients that support the Model Context Protocol:

1. **Tools** are invoked by the LLM when it needs to execute operations
2. **Resources** are accessed when the LLM needs to read configuration or metadata
3. **Prompts** guide the LLM in structuring requests for common RDF tasks

Example LLM interaction:

```
User: "What are the top predicates in the default graph?"

LLM (thinking):
1. I need graph statistics, so I'll call get_graph_stats
2. The response shows predicates, but I need to understand them better
3. I'll use the sparql_builder prompt to formulate a better query
4. Then execute_sparql to get detailed results

LLM (actions):
- Tool: get_graph_stats { graph_iri: "http://example.org/default" }
- Prompt: sparql_builder { task: "Find top predicates by usage" }
- Tool: execute_sparql { endpoint: "...", query: "..." }

Response: "The top predicates are..."
```

## Architecture

The MCP implementation consists of:

- **index.mjs** - Main server creation and transport initialization
- **tools.mjs** - Tool definitions and signatures
- **resources.mjs** - Resource definitions and metadata
- **prompts.mjs** - Prompt templates for LLM guidance
- **__tests__/mcp.test.mjs** - Comprehensive test suite

## Testing

Run tests with:

```bash
pnpm test packages/daemon
```

Tests cover:
- Server creation with correct capabilities
- Tool registration and invocation
- Resource listing and reading
- Prompt availability and structure
- Transport initialization (Stdio and SSE)

## Future Enhancements

- [ ] Graph filtering and access control
- [ ] Custom ontology loaders
- [ ] Performance profiling tools
- [ ] Incremental loading support
- [ ] Semantic validation tools
- [ ] Federation support for distributed graphs
