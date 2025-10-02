<template>
  <div class="api-docs-container">
    <div class="docs-header">
      <h1>Knowledge Hooks API Documentation</h1>
      <p class="subtitle">
        Interactive API explorer for UNRDF Knowledge Hooks
      </p>
    </div>

    <div class="docs-tabs">
      <button
        :class="{ active: activeTab === 'swagger' }"
        @click="activeTab = 'swagger'"
      >
        API Explorer
      </button>
      <button
        :class="{ active: activeTab === 'guide' }"
        @click="activeTab = 'guide'"
      >
        API Guide
      </button>
      <button
        :class="{ active: activeTab === 'examples' }"
        @click="activeTab = 'examples'"
      >
        Code Examples
      </button>
    </div>

    <div class="docs-content">
      <!-- Swagger UI Tab -->
      <div v-if="activeTab === 'swagger'" class="swagger-container">
        <div id="swagger-ui"></div>
      </div>

      <!-- API Guide Tab -->
      <div v-if="activeTab === 'guide'" class="guide-container">
        <div class="guide-section">
          <h2>Getting Started</h2>
          <p>
            The Knowledge Hooks API enables reactive semantic reasoning on RDF graphs.
            All endpoints require JWT Bearer authentication.
          </p>

          <div class="code-block">
            <pre><code>{{ quickStartExample }}</code></pre>
            <button @click="copyCode(quickStartExample)" class="copy-btn">
              Copy
            </button>
          </div>
        </div>

        <div class="guide-section">
          <h2>Authentication</h2>
          <p>Include your JWT token in the Authorization header:</p>
          <div class="code-block">
            <pre><code>Authorization: Bearer YOUR_JWT_TOKEN</code></pre>
            <button @click="copyCode('Authorization: Bearer YOUR_JWT_TOKEN')" class="copy-btn">
              Copy
            </button>
          </div>
        </div>

        <div class="guide-section">
          <h2>Core Endpoints</h2>
          <div class="endpoint-list">
            <div class="endpoint-item">
              <span class="method get">GET</span>
              <span class="path">/api/hooks/list</span>
              <span class="description">List all knowledge hooks</span>
            </div>
            <div class="endpoint-item">
              <span class="method post">POST</span>
              <span class="path">/api/hooks/register</span>
              <span class="description">Register a new hook</span>
            </div>
            <div class="endpoint-item">
              <span class="method get">GET</span>
              <span class="path">/api/hooks/{id}</span>
              <span class="description">Get hook by ID</span>
            </div>
            <div class="endpoint-item">
              <span class="method put">PUT</span>
              <span class="path">/api/hooks/{id}</span>
              <span class="description">Update a hook</span>
            </div>
            <div class="endpoint-item">
              <span class="method delete">DELETE</span>
              <span class="path">/api/hooks/{id}</span>
              <span class="description">Delete a hook</span>
            </div>
            <div class="endpoint-item">
              <span class="method post">POST</span>
              <span class="path">/api/hooks/evaluate</span>
              <span class="description">Evaluate a hook</span>
            </div>
          </div>
        </div>

        <div class="guide-section">
          <h2>Rate Limits</h2>
          <ul>
            <li>100 requests per minute per API key</li>
            <li>1000 hook evaluations per hour</li>
          </ul>
        </div>
      </div>

      <!-- Code Examples Tab -->
      <div v-if="activeTab === 'examples'" class="examples-container">
        <div class="example-section">
          <h2>JavaScript / Node.js</h2>
          <div class="code-block">
            <pre><code>{{ jsExample }}</code></pre>
            <button @click="copyCode(jsExample)" class="copy-btn">
              Copy
            </button>
          </div>
        </div>

        <div class="example-section">
          <h2>Python</h2>
          <div class="code-block">
            <pre><code>{{ pythonExample }}</code></pre>
            <button @click="copyCode(pythonExample)" class="copy-btn">
              Copy
            </button>
          </div>
        </div>

        <div class="example-section">
          <h2>cURL</h2>
          <div class="code-block">
            <pre><code>{{ curlExample }}</code></pre>
            <button @click="copyCode(curlExample)" class="copy-btn">
              Copy
            </button>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script setup>
import { ref, onMounted } from 'vue'
import SwaggerUI from 'swagger-ui'
import 'swagger-ui/dist/swagger-ui.css'

const activeTab = ref('swagger')

const quickStartExample = `// Register a hook
const response = await fetch('http://localhost:3000/api/hooks/register', {
  method: 'POST',
  headers: {
    'Content-Type': 'application/json',
    'Authorization': 'Bearer YOUR_JWT_TOKEN'
  },
  body: JSON.stringify({
    id: 'validate-budget-constraints',
    select: \`
      SELECT ?allocation ?amount ?limit WHERE {
        ?allocation :amount ?amount ; :category ?cat .
        ?cat :limit ?limit .
      }
    \`,
    predicates: [
      {
        kind: 'THRESHOLD',
        variable: 'amount',
        operator: '<=',
        value: 10000
      }
    ],
    combine: 'AND',
    phase: 'pre'
  })
})

const { data } = await response.json()
console.log(\`Hook registered: \${data.hookId}\`)

// Evaluate the hook
const evalResponse = await fetch('http://localhost:3000/api/hooks/evaluate', {
  method: 'POST',
  headers: {
    'Content-Type': 'application/json',
    'Authorization': 'Bearer YOUR_JWT_TOKEN'
  },
  body: JSON.stringify({
    hookId: 'validate-budget-constraints',
    data: \`
      @prefix : <https://example.org/> .
      :allocation1 :amount 8500 ; :category :marketing .
      :marketing :limit 10000 .
    \`
  })
})

const evalResult = await evalResponse.json()
console.log(\`Hook fired: \${evalResult.data.fired}\`)`

const jsExample = `import fetch from 'node-fetch'

const API_BASE = 'http://localhost:3000'
const TOKEN = process.env.API_TOKEN

async function registerHook() {
  const response = await fetch(\`\${API_BASE}/api/hooks/register\`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'Authorization': \`Bearer \${TOKEN}\`
    },
    body: JSON.stringify({
      id: 'validate-budget',
      select: \`
        SELECT ?allocation ?amount WHERE {
          ?allocation :amount ?amount .
        }
      \`,
      predicates: [
        {
          kind: 'THRESHOLD',
          variable: 'amount',
          operator: '<=',
          value: 10000
        }
      ],
      combine: 'AND',
      phase: 'pre'
    })
  })

  return response.json()
}

async function evaluateHook(hookId, rdfData) {
  const response = await fetch(\`\${API_BASE}/api/hooks/evaluate\`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'Authorization': \`Bearer \${TOKEN}\`
    },
    body: JSON.stringify({ hookId, data: rdfData })
  })

  return response.json()
}

// Usage
const result = await registerHook()
console.log(\`Hook registered: \${result.data.hookId}\`)

const evaluation = await evaluateHook('validate-budget', \`
  @prefix : <https://example.org/> .
  :allocation1 :amount 8500 .
\`)
console.log(\`Hook fired: \${evaluation.data.fired}\`)`

const pythonExample = `import requests
import os

API_BASE = 'http://localhost:3000'
TOKEN = os.getenv('API_TOKEN')

headers = {
    'Content-Type': 'application/json',
    'Authorization': f'Bearer {TOKEN}'
}

def register_hook():
    response = requests.post(
        f'{API_BASE}/api/hooks/register',
        headers=headers,
        json={
            'id': 'validate-budget',
            'select': '''
                SELECT ?allocation ?amount WHERE {
                    ?allocation :amount ?amount .
                }
            ''',
            'predicates': [
                {
                    'kind': 'THRESHOLD',
                    'variable': 'amount',
                    'operator': '<=',
                    'value': 10000
                }
            ],
            'combine': 'AND',
            'phase': 'pre'
        }
    )
    return response.json()

def evaluate_hook(hook_id, rdf_data):
    response = requests.post(
        f'{API_BASE}/api/hooks/evaluate',
        headers=headers,
        json={'hookId': hook_id, 'data': rdf_data}
    )
    return response.json()

# Usage
result = register_hook()
print(f"Hook registered: {result['data']['hookId']}")

evaluation = evaluate_hook('validate-budget', '''
    @prefix : <https://example.org/> .
    :allocation1 :amount 8500 .
''')
print(f"Hook fired: {evaluation['data']['fired']}")`

const curlExample = `# Register a hook
curl -X POST http://localhost:3000/api/hooks/register \\
  -H "Content-Type: application/json" \\
  -H "Authorization: Bearer $API_TOKEN" \\
  -d '{
    "id": "validate-budget",
    "select": "SELECT ?allocation ?amount WHERE { ?allocation :amount ?amount . }",
    "predicates": [
      {
        "kind": "THRESHOLD",
        "variable": "amount",
        "operator": "<=",
        "value": 10000
      }
    ],
    "combine": "AND",
    "phase": "pre"
  }'

# Evaluate a hook
curl -X POST http://localhost:3000/api/hooks/evaluate \\
  -H "Content-Type: application/json" \\
  -H "Authorization: Bearer $API_TOKEN" \\
  -d '{
    "hookId": "validate-budget",
    "data": "@prefix : <https://example.org/> .\\n:allocation1 :amount 8500 ."
  }'

# List all hooks
curl -H "Authorization: Bearer $API_TOKEN" \\
  "http://localhost:3000/api/hooks/list?limit=10"

# Get hook by ID
curl -H "Authorization: Bearer $API_TOKEN" \\
  http://localhost:3000/api/hooks/validate-budget

# Delete a hook
curl -X DELETE \\
  -H "Authorization: Bearer $API_TOKEN" \\
  http://localhost:3000/api/hooks/validate-budget`

function copyCode(code) {
  navigator.clipboard.writeText(code)
  // Show a toast notification or feedback
  alert('Code copied to clipboard!')
}

onMounted(() => {
  // Initialize Swagger UI with OpenAPI spec
  SwaggerUI({
    dom_id: '#swagger-ui',
    url: '/docs/api/openapi.yaml', // Path to OpenAPI spec
    deepLinking: true,
    presets: [
      SwaggerUI.presets.apis,
      SwaggerUI.SwaggerUIStandalonePreset
    ],
    plugins: [
      SwaggerUI.plugins.DownloadUrl
    ],
    layout: 'StandaloneLayout',
    tryItOutEnabled: true,
    requestInterceptor: (request) => {
      // Add default authorization header if available
      const token = localStorage.getItem('auth_token')
      if (token) {
        request.headers.Authorization = `Bearer ${token}`
      }
      return request
    }
  })
})
</script>

<style scoped>
.api-docs-container {
  max-width: 1400px;
  margin: 0 auto;
  padding: 2rem;
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, sans-serif;
}

.docs-header {
  text-align: center;
  margin-bottom: 3rem;
}

.docs-header h1 {
  font-size: 2.5rem;
  font-weight: 700;
  color: #1a1a1a;
  margin-bottom: 0.5rem;
}

.subtitle {
  font-size: 1.125rem;
  color: #666;
}

.docs-tabs {
  display: flex;
  gap: 1rem;
  margin-bottom: 2rem;
  border-bottom: 2px solid #e5e7eb;
}

.docs-tabs button {
  padding: 0.75rem 1.5rem;
  font-size: 1rem;
  font-weight: 500;
  background: none;
  border: none;
  border-bottom: 2px solid transparent;
  color: #666;
  cursor: pointer;
  transition: all 0.2s;
  margin-bottom: -2px;
}

.docs-tabs button:hover {
  color: #1a1a1a;
}

.docs-tabs button.active {
  color: #3b82f6;
  border-bottom-color: #3b82f6;
}

.docs-content {
  min-height: 600px;
}

.swagger-container {
  background: white;
  border-radius: 8px;
  box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1);
}

.guide-container,
.examples-container {
  background: white;
  border-radius: 8px;
  padding: 2rem;
  box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1);
}

.guide-section,
.example-section {
  margin-bottom: 3rem;
}

.guide-section h2,
.example-section h2 {
  font-size: 1.75rem;
  font-weight: 600;
  color: #1a1a1a;
  margin-bottom: 1rem;
}

.guide-section p {
  color: #4b5563;
  line-height: 1.6;
  margin-bottom: 1rem;
}

.code-block {
  position: relative;
  background: #1e1e1e;
  border-radius: 8px;
  padding: 1.5rem;
  margin: 1rem 0;
  overflow-x: auto;
}

.code-block pre {
  margin: 0;
  color: #d4d4d4;
  font-family: 'Monaco', 'Menlo', 'Ubuntu Mono', monospace;
  font-size: 0.875rem;
  line-height: 1.5;
}

.code-block code {
  font-family: inherit;
}

.copy-btn {
  position: absolute;
  top: 1rem;
  right: 1rem;
  padding: 0.5rem 1rem;
  background: #3b82f6;
  color: white;
  border: none;
  border-radius: 4px;
  cursor: pointer;
  font-size: 0.875rem;
  transition: background 0.2s;
}

.copy-btn:hover {
  background: #2563eb;
}

.endpoint-list {
  margin-top: 1.5rem;
}

.endpoint-item {
  display: flex;
  align-items: center;
  gap: 1rem;
  padding: 1rem;
  border: 1px solid #e5e7eb;
  border-radius: 6px;
  margin-bottom: 0.5rem;
}

.method {
  padding: 0.25rem 0.75rem;
  border-radius: 4px;
  font-weight: 600;
  font-size: 0.75rem;
  text-transform: uppercase;
  min-width: 60px;
  text-align: center;
}

.method.get {
  background: #dbeafe;
  color: #1e40af;
}

.method.post {
  background: #d1fae5;
  color: #065f46;
}

.method.put {
  background: #fef3c7;
  color: #92400e;
}

.method.delete {
  background: #fee2e2;
  color: #991b1b;
}

.path {
  font-family: 'Monaco', 'Menlo', monospace;
  font-size: 0.875rem;
  color: #1a1a1a;
  font-weight: 500;
  flex: 1;
}

.description {
  color: #666;
  font-size: 0.875rem;
}

.guide-section ul {
  list-style: none;
  padding: 0;
}

.guide-section ul li {
  padding: 0.5rem 0;
  padding-left: 1.5rem;
  position: relative;
  color: #4b5563;
}

.guide-section ul li::before {
  content: '•';
  position: absolute;
  left: 0;
  color: #3b82f6;
  font-weight: bold;
}
</style>
