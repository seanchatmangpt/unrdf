# Generate OpenAPI, Zod, and JSDoc from One Ontology

**Duration:** 25 minutes
**Level:** Intermediate
**Prerequisites:** Familiarity with RDF ontologies, SPARQL basics

## What You'll Learn

Generate synchronized code artifacts from a single RDF ontology source:

- Define API endpoints, request/response types in RDF
- Create templates for OpenAPI specifications
- Generate Zod validation schemas from ontology
- Produce JSDoc type definitions with descriptions
- Configure multiple generation rules in ggen.toml
- Verify consistency across all generated outputs

## Why Synchronized Outputs Matter

Contract-first development ensures all components share the same truth:

```
                    ┌─────────────────────────┐
                    │    RDF Ontology         │
                    │  (Single Source)        │
                    └───────────┬─────────────┘
                                │
            ┌───────────────────┼───────────────────┐
            │                   │                   │
            ▼                   ▼                   ▼
    ┌───────────────┐  ┌───────────────┐  ┌───────────────┐
    │   OpenAPI     │  │     Zod       │  │    JSDoc      │
    │    Spec       │  │   Schemas     │  │    Types      │
    └───────────────┘  └───────────────┘  └───────────────┘
            │                   │                   │
            ▼                   ▼                   ▼
    API Documentation   Runtime Validation   IDE Autocomplete
```

**Benefits:**
- **Single source of truth:** Change the ontology, regenerate everything
- **No drift:** API docs, validation, and types always match
- **Traceability:** Every field traces back to its RDF definition

---

## Step 1: Create the API Ontology (5 min)

Create a directory structure for the project:

```bash
mkdir -p /tmp/sync-demo/ontology
mkdir -p /tmp/sync-demo/templates
mkdir -p /tmp/sync-demo/lib/generated
cd /tmp/sync-demo
```

Create `/tmp/sync-demo/ontology/user-api.ttl`:

```turtle
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix api: <https://example.org/api#> .
@prefix schema: <https://schema.org/> .

# =============================================================================
# ONTOLOGY METADATA
# =============================================================================

<https://example.org/api>
    a owl:Ontology ;
    rdfs:label "User Management API" ;
    rdfs:comment "API ontology for user management with CRUD operations" ;
    owl:versionInfo "1.0.0" .

# =============================================================================
# DOMAIN CLASSES
# =============================================================================

api:User
    a rdfs:Class, owl:Class ;
    rdfs:label "User" ;
    rdfs:comment "A registered user in the system" .

api:CreateUserRequest
    a rdfs:Class ;
    rdfs:label "Create User Request" ;
    rdfs:comment "Request payload for creating a new user" .

api:UpdateUserRequest
    a rdfs:Class ;
    rdfs:label "Update User Request" ;
    rdfs:comment "Request payload for updating an existing user" .

api:UserResponse
    a rdfs:Class ;
    rdfs:label "User Response" ;
    rdfs:comment "Response payload containing user data" .

api:UserListResponse
    a rdfs:Class ;
    rdfs:label "User List Response" ;
    rdfs:comment "Response payload containing a list of users" .

api:ErrorResponse
    a rdfs:Class ;
    rdfs:label "Error Response" ;
    rdfs:comment "Standard error response format" .

# =============================================================================
# USER PROPERTIES
# =============================================================================

api:id
    a owl:DatatypeProperty ;
    rdfs:label "id" ;
    rdfs:comment "Unique identifier for the user (UUID format)" ;
    rdfs:domain api:User, api:UserResponse ;
    rdfs:range xsd:string ;
    api:format "uuid" ;
    api:example "550e8400-e29b-41d4-a716-446655440000" .

api:email
    a owl:DatatypeProperty ;
    rdfs:label "email" ;
    rdfs:comment "User's email address (must be unique)" ;
    rdfs:domain api:User, api:CreateUserRequest, api:UpdateUserRequest, api:UserResponse ;
    rdfs:range xsd:string ;
    api:format "email" ;
    api:example "user@example.com" .

api:name
    a owl:DatatypeProperty ;
    rdfs:label "name" ;
    rdfs:comment "User's display name" ;
    rdfs:domain api:User, api:CreateUserRequest, api:UpdateUserRequest, api:UserResponse ;
    rdfs:range xsd:string ;
    api:minLength 1 ;
    api:maxLength 100 ;
    api:example "John Doe" .

api:password
    a owl:DatatypeProperty ;
    rdfs:label "password" ;
    rdfs:comment "User's password (hashed, never returned in responses)" ;
    rdfs:domain api:CreateUserRequest ;
    rdfs:range xsd:string ;
    api:minLength 8 ;
    api:format "password" .

api:role
    a owl:DatatypeProperty ;
    rdfs:label "role" ;
    rdfs:comment "User's role in the system" ;
    rdfs:domain api:User, api:CreateUserRequest, api:UpdateUserRequest, api:UserResponse ;
    rdfs:range xsd:string ;
    api:enum "admin", "user", "guest" ;
    api:default "user" .

api:createdAt
    a owl:DatatypeProperty ;
    rdfs:label "createdAt" ;
    rdfs:comment "Timestamp when the user was created" ;
    rdfs:domain api:User, api:UserResponse ;
    rdfs:range xsd:dateTime ;
    api:format "date-time" ;
    api:readOnly true .

api:updatedAt
    a owl:DatatypeProperty ;
    rdfs:label "updatedAt" ;
    rdfs:comment "Timestamp when the user was last updated" ;
    rdfs:domain api:User, api:UserResponse ;
    rdfs:range xsd:dateTime ;
    api:format "date-time" ;
    api:readOnly true .

api:active
    a owl:DatatypeProperty ;
    rdfs:label "active" ;
    rdfs:comment "Whether the user account is active" ;
    rdfs:domain api:User, api:UpdateUserRequest, api:UserResponse ;
    rdfs:range xsd:boolean ;
    api:default true .

# =============================================================================
# LIST RESPONSE PROPERTIES
# =============================================================================

api:users
    a owl:ObjectProperty ;
    rdfs:label "users" ;
    rdfs:comment "Array of user objects" ;
    rdfs:domain api:UserListResponse ;
    rdfs:range api:UserResponse .

api:total
    a owl:DatatypeProperty ;
    rdfs:label "total" ;
    rdfs:comment "Total number of users matching the query" ;
    rdfs:domain api:UserListResponse ;
    rdfs:range xsd:integer .

api:page
    a owl:DatatypeProperty ;
    rdfs:label "page" ;
    rdfs:comment "Current page number" ;
    rdfs:domain api:UserListResponse ;
    rdfs:range xsd:integer .

api:pageSize
    a owl:DatatypeProperty ;
    rdfs:label "pageSize" ;
    rdfs:comment "Number of items per page" ;
    rdfs:domain api:UserListResponse ;
    rdfs:range xsd:integer .

# =============================================================================
# ERROR RESPONSE PROPERTIES
# =============================================================================

api:code
    a owl:DatatypeProperty ;
    rdfs:label "code" ;
    rdfs:comment "Error code for programmatic handling" ;
    rdfs:domain api:ErrorResponse ;
    rdfs:range xsd:string .

api:message
    a owl:DatatypeProperty ;
    rdfs:label "message" ;
    rdfs:comment "Human-readable error message" ;
    rdfs:domain api:ErrorResponse ;
    rdfs:range xsd:string .

api:details
    a owl:DatatypeProperty ;
    rdfs:label "details" ;
    rdfs:comment "Additional error details" ;
    rdfs:domain api:ErrorResponse ;
    rdfs:range xsd:string .

# =============================================================================
# API ENDPOINTS
# =============================================================================

api:ListUsersEndpoint
    a api:Endpoint ;
    rdfs:label "List Users" ;
    rdfs:comment "Retrieve a paginated list of all users" ;
    api:path "/users" ;
    api:method "GET" ;
    api:operationId "listUsers" ;
    api:tag "Users" ;
    api:response api:UserListResponse ;
    api:errorResponse api:ErrorResponse .

api:GetUserEndpoint
    a api:Endpoint ;
    rdfs:label "Get User" ;
    rdfs:comment "Retrieve a single user by their ID" ;
    api:path "/users/{id}" ;
    api:method "GET" ;
    api:operationId "getUser" ;
    api:tag "Users" ;
    api:pathParam "id" ;
    api:response api:UserResponse ;
    api:errorResponse api:ErrorResponse .

api:CreateUserEndpoint
    a api:Endpoint ;
    rdfs:label "Create User" ;
    rdfs:comment "Create a new user account" ;
    api:path "/users" ;
    api:method "POST" ;
    api:operationId "createUser" ;
    api:tag "Users" ;
    api:requestBody api:CreateUserRequest ;
    api:response api:UserResponse ;
    api:errorResponse api:ErrorResponse .

api:UpdateUserEndpoint
    a api:Endpoint ;
    rdfs:label "Update User" ;
    rdfs:comment "Update an existing user's information" ;
    api:path "/users/{id}" ;
    api:method "PUT" ;
    api:operationId "updateUser" ;
    api:tag "Users" ;
    api:pathParam "id" ;
    api:requestBody api:UpdateUserRequest ;
    api:response api:UserResponse ;
    api:errorResponse api:ErrorResponse .

api:DeleteUserEndpoint
    a api:Endpoint ;
    rdfs:label "Delete User" ;
    rdfs:comment "Delete a user account permanently" ;
    api:path "/users/{id}" ;
    api:method "DELETE" ;
    api:operationId "deleteUser" ;
    api:tag "Users" ;
    api:pathParam "id" ;
    api:errorResponse api:ErrorResponse .

# =============================================================================
# SHACL VALIDATION SHAPES
# =============================================================================

api:CreateUserRequestShape
    a sh:NodeShape ;
    sh:targetClass api:CreateUserRequest ;
    sh:property [
        sh:path api:email ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:datatype xsd:string ;
        sh:pattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" ;
    ] ;
    sh:property [
        sh:path api:name ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:datatype xsd:string ;
        sh:minLength 1 ;
        sh:maxLength 100 ;
    ] ;
    sh:property [
        sh:path api:password ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:datatype xsd:string ;
        sh:minLength 8 ;
    ] ;
    sh:property [
        sh:path api:role ;
        sh:maxCount 1 ;
        sh:datatype xsd:string ;
        sh:in ( "admin" "user" "guest" ) ;
    ] .

api:UpdateUserRequestShape
    a sh:NodeShape ;
    sh:targetClass api:UpdateUserRequest ;
    sh:property [
        sh:path api:email ;
        sh:maxCount 1 ;
        sh:datatype xsd:string ;
        sh:pattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" ;
    ] ;
    sh:property [
        sh:path api:name ;
        sh:maxCount 1 ;
        sh:datatype xsd:string ;
        sh:minLength 1 ;
        sh:maxLength 100 ;
    ] ;
    sh:property [
        sh:path api:role ;
        sh:maxCount 1 ;
        sh:datatype xsd:string ;
        sh:in ( "admin" "user" "guest" ) ;
    ] ;
    sh:property [
        sh:path api:active ;
        sh:maxCount 1 ;
        sh:datatype xsd:boolean ;
    ] .
```

This ontology defines:
- **Domain classes:** User, request/response types
- **Properties:** With types, formats, constraints, and examples
- **API endpoints:** With HTTP methods, paths, and schemas
- **SHACL shapes:** For validation constraints

---

## Step 2: Create OpenAPI Templates (5 min)

Create `/tmp/sync-demo/templates/openapi-spec.yaml.njk`:

```nunjucks
---
to: openapi.yaml
description: OpenAPI 3.0 specification generated from RDF ontology
---
openapi: "3.0.3"
info:
  title: {{ project.name | default('Generated API') }}
  description: {{ project.description | default('API generated from RDF ontology') }}
  version: {{ project.version | default('1.0.0') }}
  contact:
    name: API Support
    email: support@example.com

servers:
  - url: https://api.example.com/v1
    description: Production server
  - url: https://staging-api.example.com/v1
    description: Staging server

tags:
{% for tag in results | distinctValues('tag') %}
  - name: {{ tag | localName }}
    description: Operations for {{ tag | localName }}
{% endfor %}

paths:
{% for endpoint in results %}
  {{ endpoint.path }}:
    {{ endpoint.method | lower }}:
      operationId: {{ endpoint.operationId }}
      summary: {{ endpoint.label }}
      description: {{ endpoint.comment | default(endpoint.label) }}
      tags:
        - {{ endpoint.tag | localName }}
{% if endpoint.pathParam %}
      parameters:
        - name: {{ endpoint.pathParam }}
          in: path
          required: true
          schema:
            type: string
            format: uuid
          description: Unique identifier
{% endif %}
{% if endpoint.requestBody %}
      requestBody:
        required: true
        content:
          application/json:
            schema:
              $ref: '#/components/schemas/{{ endpoint.requestBody | localName }}'
{% endif %}
      responses:
{% if endpoint.response %}
        '200':
          description: Successful operation
          content:
            application/json:
              schema:
                $ref: '#/components/schemas/{{ endpoint.response | localName }}'
{% else %}
        '204':
          description: No content
{% endif %}
{% if endpoint.errorResponse %}
        '400':
          description: Bad request
          content:
            application/json:
              schema:
                $ref: '#/components/schemas/{{ endpoint.errorResponse | localName }}'
        '404':
          description: Not found
          content:
            application/json:
              schema:
                $ref: '#/components/schemas/{{ endpoint.errorResponse | localName }}'
        '500':
          description: Internal server error
          content:
            application/json:
              schema:
                $ref: '#/components/schemas/{{ endpoint.errorResponse | localName }}'
{% endif %}

{% endfor %}
```

Create `/tmp/sync-demo/templates/openapi-schemas.yaml.njk`:

```nunjucks
---
to: openapi-schemas.yaml
description: OpenAPI schema components generated from RDF classes
---
components:
  schemas:
{% for className, props in results | groupBy('className') %}
    {{ className }}:
      type: object
      description: {{ props[0].classComment | default('') }}
{% set requiredProps = props | selectattr('required') | map(attribute='propName') | list %}
{% if requiredProps | length > 0 %}
      required:
{% for reqProp in requiredProps %}
        - {{ reqProp }}
{% endfor %}
{% endif %}
      properties:
{% for prop in props %}
        {{ prop.propName }}:
          type: {{ prop.xsdType | replace('http://www.w3.org/2001/XMLSchema#', '') | replace('string', 'string') | replace('integer', 'integer') | replace('boolean', 'boolean') | replace('dateTime', 'string') | default('string') }}
{% if prop.xsdType and 'dateTime' in prop.xsdType %}
          format: date-time
{% endif %}
{% if prop.format %}
          format: {{ prop.format }}
{% endif %}
{% if prop.propComment %}
          description: {{ prop.propComment }}
{% endif %}
{% if prop.example %}
          example: {{ prop.example }}
{% endif %}
{% if prop.minLength %}
          minLength: {{ prop.minLength }}
{% endif %}
{% if prop.maxLength %}
          maxLength: {{ prop.maxLength }}
{% endif %}
{% if prop.readOnly %}
          readOnly: true
{% endif %}
{% if prop.default %}
          default: {{ prop.default }}
{% endif %}
{% endfor %}

{% endfor %}
```

---

## Step 3: Create Zod Schema Templates (4 min)

Create `/tmp/sync-demo/templates/zod-schemas.mjs.njk`:

```nunjucks
---
to: schemas.mjs
description: Zod validation schemas generated from RDF ontology
---
/**
 * @file Generated Zod Schemas
 * @description Validation schemas generated from RDF ontology
 * @generated {{ now | date('YYYY-MM-DD HH:mm:ss') }}
 * @source ontology/user-api.ttl
 */
import { z } from 'zod';

{% for className, props in results | groupBy('className') %}
/**
 * {{ props[0].classComment | default(className + ' schema') }}
 * @see {{ props[0].classUri }}
 */
export const {{ className }}Schema = z.object({
{% for prop in props %}
  /**
   * {{ prop.propComment | default(prop.propName) }}
   */
  {{ prop.propName }}: {{ prop.xsdType | zodType }}{% if prop.format == 'email' %}.email(){% endif %}{% if prop.format == 'uuid' %}.uuid(){% endif %}{% if prop.format == 'url' %}.url(){% endif %}{% if prop.minLength %}.min({{ prop.minLength }}){% endif %}{% if prop.maxLength %}.max({{ prop.maxLength }}){% endif %}{% if not prop.required %}.optional(){% endif %}{% if prop.default %}.default({% if prop.xsdType and 'boolean' in prop.xsdType %}{{ prop.default }}{% elif prop.xsdType and 'integer' in prop.xsdType %}{{ prop.default }}{% else %}'{{ prop.default }}'{% endif %}){% endif %},
{% endfor %}
});

/**
 * Inferred TypeScript type for {{ className }}
 */
export const {{ className }}Type = {{ className }}Schema;

{% endfor %}
// =============================================================================
// SCHEMA EXPORTS
// =============================================================================

export const schemas = {
{% for className in results | distinctValues('className') %}
  {{ className }}: {{ className }}Schema,
{% endfor %}
};

export default schemas;
```

---

## Step 4: Create JSDoc Type Templates (4 min)

Create `/tmp/sync-demo/templates/jsdoc-types.mjs.njk`:

```nunjucks
---
to: types.mjs
description: JSDoc type definitions generated from RDF ontology
---
/**
 * @file Generated Type Definitions
 * @description JSDoc types generated from RDF ontology
 * @generated {{ now | date('YYYY-MM-DD HH:mm:ss') }}
 * @source ontology/user-api.ttl
 */

{% for className, props in results | groupBy('className') %}
/**
 * {{ props[0].classComment | default(className) }}
 * @typedef {{ '{' }}Object{{ '}' }} {{ className }}
{% for prop in props %}
 * @property {{ '{' }}{{ prop.xsdType | jsdocType }}{{ '}' }} {% if not prop.required %}[{% endif %}{{ prop.propName }}{% if not prop.required %}]{% endif %} - {{ prop.propComment | default(prop.propName) }}{% if prop.example %} (e.g., {{ prop.example }}){% endif %}

{% endfor %}
 * @see {{ props[0].classUri }}
 */

{% endfor %}
// =============================================================================
// TYPE GUARDS
// =============================================================================

{% for className in results | distinctValues('className') %}
/**
 * Check if value is a valid {{ className }}
 * @param {unknown} value - Value to check
 * @returns {value is {{ className }}} True if value matches {{ className }} shape
 */
export function is{{ className }}(value) {
  if (!value || typeof value !== 'object') return false;
{% for prop in results | selectattr('className', 'equalto', className) | selectattr('required') %}
  if (!('{{ prop.propName }}' in value)) return false;
{% endfor %}
  return true;
}

{% endfor %}
// =============================================================================
// FACTORY FUNCTIONS
// =============================================================================

{% for className, props in results | groupBy('className') %}
/**
 * Create a new {{ className }} with defaults
 * @param {Partial<{{ className }}>} [overrides] - Property overrides
 * @returns {{ '{' }}{{ className }}{{ '}' }} New {{ className }} instance
 */
export function create{{ className }}(overrides = {}) {
  return {
{% for prop in props %}
{% if prop.default %}
    {{ prop.propName }}: {% if prop.xsdType and 'boolean' in prop.xsdType %}{{ prop.default }}{% elif prop.xsdType and 'integer' in prop.xsdType %}{{ prop.default }}{% else %}'{{ prop.default }}'{% endif %},
{% elif prop.xsdType and 'string' in prop.xsdType %}
    {{ prop.propName }}: '',
{% elif prop.xsdType and 'integer' in prop.xsdType %}
    {{ prop.propName }}: 0,
{% elif prop.xsdType and 'boolean' in prop.xsdType %}
    {{ prop.propName }}: false,
{% else %}
    {{ prop.propName }}: null,
{% endif %}
{% endfor %}
    ...overrides,
  };
}

{% endfor %}
export default {
{% for className in results | distinctValues('className') %}
  is{{ className }},
  create{{ className }},
{% endfor %}
};
```

Create `/tmp/sync-demo/templates/api-client.mjs.njk`:

```nunjucks
---
to: api-client.mjs
description: API client generated from RDF endpoint definitions
---
/**
 * @file Generated API Client
 * @description HTTP client generated from RDF API ontology
 * @generated {{ now | date('YYYY-MM-DD HH:mm:ss') }}
 * @source ontology/user-api.ttl
 */

/**
 * API configuration
 * @typedef {Object} ApiConfig
 * @property {string} baseUrl - Base URL for API requests
 * @property {Record<string, string>} [headers] - Default headers
 * @property {number} [timeout] - Request timeout in ms
 */

/**
 * Create API client
 * @param {ApiConfig} config - Client configuration
 * @returns {Object} API client methods
 */
export function createApiClient(config) {
  const { baseUrl, headers = {}, timeout = 30000 } = config;

  /**
   * Make HTTP request
   * @param {string} method - HTTP method
   * @param {string} path - Request path
   * @param {Object} [options] - Request options
   * @returns {Promise<any>} Response data
   */
  async function request(method, path, options = {}) {
    const url = new URL(path, baseUrl);
    const response = await fetch(url, {
      method,
      headers: {
        'Content-Type': 'application/json',
        ...headers,
        ...options.headers,
      },
      body: options.body ? JSON.stringify(options.body) : undefined,
      signal: AbortSignal.timeout(timeout),
    });

    if (!response.ok) {
      const error = await response.json().catch(() => ({}));
      throw new Error(error.message || `HTTP ${response.status}`);
    }

    if (response.status === 204) return null;
    return response.json();
  }

  return {
{% for endpoint in results %}
    /**
     * {{ endpoint.comment | default(endpoint.label) }}
     * @operationId {{ endpoint.operationId }}
{% if endpoint.pathParam %}
     * @param {string} {{ endpoint.pathParam }} - Resource identifier
{% endif %}
{% if endpoint.requestBody %}
     * @param {import('./types.mjs').{{ endpoint.requestBody | localName }}} data - Request payload
{% endif %}
     * @returns {Promise<{% if endpoint.response %}import('./types.mjs').{{ endpoint.response | localName }}{% else %}void{% endif %}>}
     */
    {{ endpoint.operationId }}: ({% if endpoint.pathParam %}{{ endpoint.pathParam }}{% if endpoint.requestBody %}, {% endif %}{% endif %}{% if endpoint.requestBody %}data{% endif %}) =>
      request('{{ endpoint.method }}', '{{ endpoint.path }}'{% if endpoint.pathParam %}.replace('{id}', {{ endpoint.pathParam }}){% endif %}{% if endpoint.requestBody %}, { body: data }{% endif %}),

{% endfor %}
  };
}

export default createApiClient;
```

---

## Step 5: Configure Generation Rules (3 min)

Create `/tmp/sync-demo/ggen.toml`:

```toml
# ggen.toml - Multi-Format Code Generation Configuration
# Generates OpenAPI, Zod schemas, JSDoc types, and API client from RDF ontology

[project]
name = "User Management API"
version = "1.0.0"
description = "Complete API artifacts generated from RDF ontology"

[ontology]
source = "ontology/user-api.ttl"
format = "turtle"
base_iri = "https://example.org/api#"
prefixes = { api = "https://example.org/api#", xsd = "http://www.w3.org/2001/XMLSchema#", rdfs = "http://www.w3.org/2000/01/rdf-schema#", sh = "http://www.w3.org/ns/shacl#" }

[generation]
output_dir = "lib/generated"
templates_dir = "templates"
incremental = true

# =============================================================================
# Rule 1: OpenAPI Specification (Endpoints)
# =============================================================================
[[generation.rules]]
name = "openapi-spec"
description = "Generate OpenAPI 3.0 paths from RDF endpoints"
template = "openapi-spec.yaml.njk"
output_file = "openapi.yaml"
enabled = true
query = """
PREFIX api: <https://example.org/api#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?endpoint ?label ?comment ?path ?method ?operationId ?tag ?pathParam ?requestBody ?response ?errorResponse
WHERE {
  ?endpoint a api:Endpoint ;
            rdfs:label ?label ;
            api:path ?path ;
            api:method ?method ;
            api:operationId ?operationId ;
            api:tag ?tag .
  OPTIONAL { ?endpoint rdfs:comment ?comment }
  OPTIONAL { ?endpoint api:pathParam ?pathParam }
  OPTIONAL { ?endpoint api:requestBody ?requestBody }
  OPTIONAL { ?endpoint api:response ?response }
  OPTIONAL { ?endpoint api:errorResponse ?errorResponse }
}
ORDER BY ?path ?method
"""

# =============================================================================
# Rule 2: OpenAPI Schemas
# =============================================================================
[[generation.rules]]
name = "openapi-schemas"
description = "Generate OpenAPI component schemas from RDF classes"
template = "openapi-schemas.yaml.njk"
output_file = "openapi-schemas.yaml"
enabled = true
query = """
PREFIX api: <https://example.org/api#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX sh: <http://www.w3.org/ns/shacl#>

SELECT ?classUri ?className ?classComment ?propName ?propComment ?xsdType ?format ?example ?minLength ?maxLength ?required ?readOnly ?default
WHERE {
  ?classUri a rdfs:Class ;
            rdfs:label ?className .
  OPTIONAL { ?classUri rdfs:comment ?classComment }

  ?prop rdfs:domain ?classUri ;
        rdfs:label ?propName .
  OPTIONAL { ?prop rdfs:comment ?propComment }
  OPTIONAL { ?prop rdfs:range ?xsdType }
  OPTIONAL { ?prop api:format ?format }
  OPTIONAL { ?prop api:example ?example }
  OPTIONAL { ?prop api:minLength ?minLength }
  OPTIONAL { ?prop api:maxLength ?maxLength }
  OPTIONAL { ?prop api:readOnly ?readOnly }
  OPTIONAL { ?prop api:default ?default }

  OPTIONAL {
    ?shape sh:targetClass ?classUri ;
           sh:property ?propSpec .
    ?propSpec sh:path ?prop ;
              sh:minCount ?minCount .
    FILTER(?minCount >= 1)
    BIND(true AS ?required)
  }
}
ORDER BY ?className ?propName
"""

# =============================================================================
# Rule 3: Zod Validation Schemas
# =============================================================================
[[generation.rules]]
name = "zod-schemas"
description = "Generate Zod validation schemas from RDF classes"
template = "zod-schemas.mjs.njk"
output_file = "schemas.mjs"
enabled = true
query = """
PREFIX api: <https://example.org/api#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX sh: <http://www.w3.org/ns/shacl#>

SELECT ?classUri ?className ?classComment ?propName ?propComment ?xsdType ?format ?minLength ?maxLength ?required ?default
WHERE {
  ?classUri a rdfs:Class ;
            rdfs:label ?className .
  OPTIONAL { ?classUri rdfs:comment ?classComment }

  FILTER(
    ?classUri = api:CreateUserRequest ||
    ?classUri = api:UpdateUserRequest ||
    ?classUri = api:UserResponse ||
    ?classUri = api:UserListResponse ||
    ?classUri = api:ErrorResponse
  )

  ?prop rdfs:domain ?classUri ;
        rdfs:label ?propName .
  OPTIONAL { ?prop rdfs:comment ?propComment }
  OPTIONAL { ?prop rdfs:range ?xsdType }
  OPTIONAL { ?prop api:format ?format }
  OPTIONAL { ?prop api:minLength ?minLength }
  OPTIONAL { ?prop api:maxLength ?maxLength }
  OPTIONAL { ?prop api:default ?default }

  OPTIONAL {
    ?shape sh:targetClass ?classUri ;
           sh:property ?propSpec .
    ?propSpec sh:path ?prop ;
              sh:minCount ?minCount .
    FILTER(?minCount >= 1)
    BIND(true AS ?required)
  }
}
ORDER BY ?className ?propName
"""

# =============================================================================
# Rule 4: JSDoc Type Definitions
# =============================================================================
[[generation.rules]]
name = "jsdoc-types"
description = "Generate JSDoc type definitions from RDF classes"
template = "jsdoc-types.mjs.njk"
output_file = "types.mjs"
enabled = true
query = """
PREFIX api: <https://example.org/api#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX sh: <http://www.w3.org/ns/shacl#>

SELECT ?classUri ?className ?classComment ?propName ?propComment ?xsdType ?example ?required ?default
WHERE {
  ?classUri a rdfs:Class ;
            rdfs:label ?className .
  OPTIONAL { ?classUri rdfs:comment ?classComment }

  FILTER(
    ?classUri = api:User ||
    ?classUri = api:CreateUserRequest ||
    ?classUri = api:UpdateUserRequest ||
    ?classUri = api:UserResponse ||
    ?classUri = api:UserListResponse ||
    ?classUri = api:ErrorResponse
  )

  ?prop rdfs:domain ?classUri ;
        rdfs:label ?propName .
  OPTIONAL { ?prop rdfs:comment ?propComment }
  OPTIONAL { ?prop rdfs:range ?xsdType }
  OPTIONAL { ?prop api:example ?example }
  OPTIONAL { ?prop api:default ?default }

  OPTIONAL {
    ?shape sh:targetClass ?classUri ;
           sh:property ?propSpec .
    ?propSpec sh:path ?prop ;
              sh:minCount ?minCount .
    FILTER(?minCount >= 1)
    BIND(true AS ?required)
  }
}
ORDER BY ?className ?propName
"""

# =============================================================================
# Rule 5: API Client
# =============================================================================
[[generation.rules]]
name = "api-client"
description = "Generate HTTP API client from RDF endpoints"
template = "api-client.mjs.njk"
output_file = "api-client.mjs"
enabled = true
query = """
PREFIX api: <https://example.org/api#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?endpoint ?label ?comment ?path ?method ?operationId ?pathParam ?requestBody ?response
WHERE {
  ?endpoint a api:Endpoint ;
            rdfs:label ?label ;
            api:path ?path ;
            api:method ?method ;
            api:operationId ?operationId .
  OPTIONAL { ?endpoint rdfs:comment ?comment }
  OPTIONAL { ?endpoint api:pathParam ?pathParam }
  OPTIONAL { ?endpoint api:requestBody ?requestBody }
  OPTIONAL { ?endpoint api:response ?response }
}
ORDER BY ?path ?method
"""

# =============================================================================
# Rule 6: Index File
# =============================================================================
[[generation.rules]]
name = "index"
description = "Generate barrel export file"
template = "index.mjs.njk"
output_file = "index.mjs"
enabled = true
query = """
SELECT ?file
WHERE {
  VALUES ?file { "schemas" "types" "api-client" }
}
"""
```

Create `/tmp/sync-demo/templates/index.mjs.njk`:

```nunjucks
---
to: index.mjs
description: Barrel export for all generated modules
---
/**
 * @file Generated API Module
 * @description Re-exports all generated artifacts
 * @generated {{ now | date('YYYY-MM-DD HH:mm:ss') }}
 */

// Validation schemas
export * from './schemas.mjs';

// Type definitions and guards
export * from './types.mjs';

// API client
export { createApiClient } from './api-client.mjs';
```

---

## Step 6: Generate Everything (2 min)

Run the sync command to generate all outputs:

```bash
cd /tmp/sync-demo
npx unrdf sync --config ggen.toml --verbose
```

**Expected Output:**
```
UNRDF Sync

Phase 1: Loading configuration...
   Config: ggen.toml
   Project: User Management API

Phase 2: Loading ontology...
   Loaded: 127 triples

Phase 3: Processing rules...

   Rule: openapi-spec
   Query returned 5 results
   OK lib/generated/openapi.yaml (1842 bytes)

   Rule: openapi-schemas
   Query returned 24 results
   OK lib/generated/openapi-schemas.yaml (2156 bytes)

   Rule: zod-schemas
   Query returned 18 results
   OK lib/generated/schemas.mjs (2834 bytes)

   Rule: jsdoc-types
   Query returned 22 results
   OK lib/generated/types.mjs (3521 bytes)

   Rule: api-client
   Query returned 5 results
   OK lib/generated/api-client.mjs (2108 bytes)

   Rule: index
   Query returned 3 results
   OK lib/generated/index.mjs (312 bytes)

Sync complete!
   Rules processed: 6
   Files generated: 6
   Duration: 847.32ms
```

---

## Step 7: Verify Consistency (2 min)

Check that all outputs reference the same type definitions.

**Verify OpenAPI schemas:**

```bash
cat /tmp/sync-demo/lib/generated/openapi-schemas.yaml | head -50
```

**Expected snippet:**
```yaml
components:
  schemas:
    CreateUserRequest:
      type: object
      description: Request payload for creating a new user
      required:
        - email
        - name
        - password
      properties:
        email:
          type: string
          format: email
          description: User's email address (must be unique)
          example: user@example.com
```

**Verify Zod schema matches:**

```bash
cat /tmp/sync-demo/lib/generated/schemas.mjs | head -30
```

**Expected snippet:**
```javascript
export const CreateUserRequestSchema = z.object({
  /**
   * User's email address (must be unique)
   */
  email: z.string().email(),
  /**
   * User's display name
   */
  name: z.string().min(1).max(100),
  /**
   * User's password (hashed, never returned in responses)
   */
  password: z.string().min(8),
```

**Verify JSDoc types match:**

```bash
cat /tmp/sync-demo/lib/generated/types.mjs | head -25
```

**Expected snippet:**
```javascript
/**
 * Request payload for creating a new user
 * @typedef {Object} CreateUserRequest
 * @property {string} email - User's email address (must be unique) (e.g., user@example.com)
 * @property {string} name - User's display name (e.g., John Doe)
 * @property {string} password - User's password (hashed, never returned in responses)
 * @property {string} [role] - User's role in the system
```

**Key consistency checks:**
- All three outputs list the same properties for `CreateUserRequest`
- Required fields (`email`, `name`, `password`) are consistent
- Descriptions come from the same `rdfs:comment` source
- Format constraints (email, min/max length) are applied everywhere

---

## Summary

You've learned how to:

- Define a comprehensive API ontology with classes, properties, and endpoints
- Create templates for OpenAPI, Zod, and JSDoc outputs
- Configure multiple generation rules in `ggen.toml`
- Generate all artifacts with a single `unrdf sync` command
- Verify consistency across generated outputs

## Generated File Reference

| File | Purpose | Lines |
|------|---------|-------|
| `openapi.yaml` | API endpoint definitions | ~120 |
| `openapi-schemas.yaml` | Request/response schemas | ~150 |
| `schemas.mjs` | Zod runtime validation | ~180 |
| `types.mjs` | JSDoc type definitions | ~220 |
| `api-client.mjs` | Generated HTTP client | ~140 |
| `index.mjs` | Barrel exports | ~15 |

## Next Steps

- **Tutorial:** [Building Ontologies with Templates](./building-ontologies-with-templates.md) - 20 min
- **How-to:** [Custom Template Filters](../how-to/sync-custom-filters.md)
- **Reference:** [ggen.toml Configuration](../reference/sync-config.md)

## Key Takeaways

1. **Single source of truth:** One ontology drives all outputs
2. **Synchronized updates:** Change RDF once, regenerate everything
3. **Type consistency:** All outputs share the same property definitions
4. **Validation alignment:** Zod schemas mirror OpenAPI constraints
5. **Documentation coherence:** Comments propagate to all formats

## Troubleshooting

**No results from SPARQL query:**
- Check prefix definitions in `ggen.toml` match ontology prefixes
- Verify class URIs in FILTER clauses match ontology exactly
- Use `--verbose` flag to see query execution details

**Template rendering errors:**
- Ensure all referenced filters (`groupBy`, `localName`) are available
- Check frontmatter `to:` path is relative to output_dir
- Verify Nunjucks syntax (use `{% %}` for control, `{{ }}` for output)

**Inconsistent outputs:**
- Run `unrdf sync --force` to regenerate all files
- Check that all templates query the same properties
- Verify SPARQL ORDER BY ensures deterministic results

## Reference

- [Sync Configuration Reference](../reference/sync-config.md)
- [Template Design Patterns](../explanation/sync-template-patterns.md)
- [SPARQL Query Building](../how-to/build-sparql-queries.md)
