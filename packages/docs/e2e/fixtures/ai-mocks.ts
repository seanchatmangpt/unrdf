/**
 * Comprehensive AI Mock Fixtures for E2E Testing
 * Focus on AI SDK interactions, streaming, errors, and tool calls
 */

import type { Route } from '@playwright/test'

export const avatars = {
  alex: {
    name: 'Alex the API Developer',
    role: 'Backend Developer',
    email: 'alex@example.com'
  },
  sofia: {
    name: 'Sofia the Technical Writer',
    role: 'Documentation Specialist',
    email: 'sofia@example.com'
  },
  marcus: {
    name: 'Marcus the DevOps Engineer',
    role: 'Platform Engineer',
    email: 'marcus@example.com'
  },
  priya: {
    name: 'Priya the Product Manager',
    role: 'Product Manager',
    email: 'priya@example.com'
  },
  chen: {
    name: 'Chen the Full-Stack Developer',
    role: 'Full-Stack Engineer',
    email: 'chen@example.com'
  },
  jasmine: {
    name: 'Jasmine the QA Engineer',
    role: 'Quality Assurance',
    email: 'jasmine@example.com'
  },
  raj: {
    name: 'Raj the Open Source Contributor',
    role: 'Community Developer',
    email: 'raj@example.com'
  }
}

// AI Chat Mock Responses
export const aiMocks = {
  // Code generation responses
  codeGeneration: {
    apiEndpoint: {
      role: 'assistant',
      content: `Here's how to create a REST API endpoint in Nuxt:

\`\`\`typescript
// server/api/users.get.ts
export default defineEventHandler(async (event) => {
  const users = await useDrizzle()
    .select()
    .from(tables.users)
    .all()

  return users
})
\`\`\`

This creates a GET endpoint at /api/users that returns all users from the database.`
    },

    databaseQuery: {
      role: 'assistant',
      content: `To query the PGlite database with Drizzle ORM:

\`\`\`typescript
import { useDrizzle, tables, eq } from '~/server/utils/drizzle'

const db = useDrizzle()

// Select with filter
const user = await db
  .select()
  .from(tables.users)
  .where(eq(tables.users.id, 'user-123'))
  .get()

// Insert
await db.insert(tables.users).values({
  email: 'test@example.com',
  name: 'Test User'
})
\`\`\`

PGlite uses the same API as PostgreSQL, no changes needed!`
    },

    componentExample: {
      role: 'assistant',
      content: `Here's a ChatPalette component example:

\`\`\`vue
<script setup>
const { messages, input, handleSubmit, isLoading } = useChat({
  api: '/api/chat'
})
</script>

<template>
  <UChatPalette>
    <UChatMessages :messages="messages" />

    <template #prompt>
      <UChatPrompt
        v-model="input"
        :loading="isLoading"
        @submit="handleSubmit"
      />
    </template>
  </UChatPalette>
</template>
\`\`\`

This creates a complete chat interface with streaming support.`
    },

    testExample: {
      role: 'assistant',
      content: `Here's how to test with AI SDK mocks:

\`\`\`typescript
import { MockLanguageModelV2 } from 'ai/test'
import { generateText } from 'ai'

test('AI generates response', async () => {
  const model = new MockLanguageModelV2({
    doGenerate: async () => ({
      rawCall: { rawPrompt: null, rawSettings: {} },
      finishReason: 'stop',
      usage: { promptTokens: 10, completionTokens: 20 },
      text: 'Mocked AI response'
    })
  })

  const result = await generateText({ model, prompt: 'test' })
  expect(result.text).toBe('Mocked AI response')
})
\`\`\``
    }
  },

  // Streaming responses
  streaming: {
    chunks: [
      { type: 'text-delta', textDelta: 'Let' },
      { type: 'text-delta', textDelta: ' me' },
      { type: 'text-delta', textDelta: ' help' },
      { type: 'text-delta', textDelta: ' you' },
      { type: 'text-delta', textDelta: ' with' },
      { type: 'text-delta', textDelta: ' that' },
      { type: 'text-delta', textDelta: '.' },
      {
        type: 'finish',
        finishReason: 'stop',
        usage: { promptTokens: 15, completionTokens: 7 }
      }
    ],

    codeStreaming: [
      { type: 'text-delta', textDelta: '```typescript\n' },
      { type: 'text-delta', textDelta: 'export' },
      { type: 'text-delta', textDelta: ' default' },
      { type: 'text-delta', textDelta: ' defineEventHandler' },
      { type: 'text-delta', textDelta: '(() => {\n' },
      { type: 'text-delta', textDelta: '  return' },
      { type: 'text-delta', textDelta: ' { message:' },
      { type: 'text-delta', textDelta: ' "Hello"' },
      { type: 'text-delta', textDelta: ' }\n' },
      { type: 'text-delta', textDelta: '})\n' },
      { type: 'text-delta', textDelta: '```' },
      {
        type: 'finish',
        finishReason: 'stop',
        usage: { promptTokens: 8, completionTokens: 25 }
      }
    ]
  },

  // Error scenarios
  errors: {
    rateLimited: {
      status: 429,
      error: {
        message: 'Rate limit exceeded',
        type: 'rate_limit_error',
        code: 'rate_limit_exceeded'
      }
    },

    invalidApiKey: {
      status: 401,
      error: {
        message: 'Invalid API key',
        type: 'invalid_request_error',
        code: 'invalid_api_key'
      }
    },

    modelOverloaded: {
      status: 503,
      error: {
        message: 'Model is currently overloaded',
        type: 'service_unavailable',
        code: 'model_overloaded'
      }
    },

    timeout: {
      status: 504,
      error: {
        message: 'Request timeout',
        type: 'timeout_error',
        code: 'request_timeout'
      }
    }
  },

  // Tool call responses
  toolCalls: {
    weatherQuery: {
      role: 'assistant',
      content: '',
      toolCalls: [{
        id: 'call_1',
        type: 'function',
        function: {
          name: 'getWeather',
          arguments: JSON.stringify({ location: 'San Francisco' })
        }
      }]
    },

    chartGeneration: {
      role: 'assistant',
      content: '',
      toolCalls: [{
        id: 'call_2',
        type: 'function',
        function: {
          name: 'generateChart',
          arguments: JSON.stringify({
            type: 'bar',
            data: [10, 20, 30, 40],
            labels: ['Q1', 'Q2', 'Q3', 'Q4']
          })
        }
      }]
    }
  },

  // Multi-turn conversation
  conversation: [
    {
      role: 'user',
      content: 'How do I set up authentication?'
    },
    {
      role: 'assistant',
      content: 'You can use nuxt-auth-utils for authentication. It supports GitHub OAuth out of the box.'
    },
    {
      role: 'user',
      content: 'Can you show me the configuration?'
    },
    {
      role: 'assistant',
      content: `Sure! Add these environment variables:

\`\`\`bash
NUXT_SESSION_PASSWORD=your-32-character-secret
NUXT_OAUTH_GITHUB_CLIENT_ID=your-client-id
NUXT_OAUTH_GITHUB_CLIENT_SECRET=your-client-secret
\`\`\`

Then create a login route at /auth/github`
    }
  ],

  // Database operation responses
  database: {
    createChat: {
      id: 'chat-new-123',
      title: 'New Chat Session',
      userId: 'user-1',
      createdAt: new Date().toISOString()
    },

    listChats: [
      {
        id: 'chat-1',
        title: 'API Development Discussion',
        userId: 'user-1',
        createdAt: '2024-01-01T00:00:00Z'
      },
      {
        id: 'chat-2',
        title: 'Database Query Help',
        userId: 'user-1',
        createdAt: '2024-01-02T00:00:00Z'
      }
    ],

    updateChat: {
      id: 'chat-1',
      title: 'Updated Chat Title',
      userId: 'user-1',
      createdAt: '2024-01-01T00:00:00Z'
    }
  },

  // Complex AI responses
  complex: {
    architectureAdvice: {
      role: 'assistant',
      content: `For a production-ready docs template, I recommend this architecture:

**Frontend Layer:**
- Nuxt UI for components
- Nuxt Content for documentation
- ChatPalette for AI interactions

**Backend Layer:**
- PGlite for embedded database
- Drizzle ORM for type-safe queries
- AI SDK for streaming responses

**Testing Layer:**
- Vitest for unit tests
- Playwright for E2E tests
- Mock AI SDK for deterministic tests

**Deployment:**
- Zero external dependencies (PGlite runs in-process)
- Environment variables for configuration
- GitHub OAuth for authentication

This gives you a fully self-contained system that can run anywhere without infrastructure setup.`
    },

    performanceOptimization: {
      role: 'assistant',
      content: `To optimize AI response performance:

1. **Use streaming** instead of waiting for complete responses
2. **Implement request debouncing** to avoid excessive API calls
3. **Cache frequent queries** with a simple in-memory store
4. **Use PGlite** to avoid network latency
5. **Preload AI responses** for common questions

Example streaming implementation:
\`\`\`typescript
const result = streamText({
  model: gateway('openai:gpt-4o-mini'),
  messages,
  temperature: 0.7
})

return result.toDataStreamResponse()
\`\`\`

This provides instant feedback to users.`
    }
  }
}

// Mock API route handlers
export const createAIMockHandler = (response: unknown) => {
  return async (route: Route) => {
    await route.fulfill({
      status: 200,
      contentType: 'application/json',
      body: JSON.stringify(response)
    })
  }
}

export const createStreamingMockHandler = (chunks: unknown[]) => {
  return async (route: Route) => {
    const stream = chunks.map(chunk => `data: ${JSON.stringify(chunk)}\n\n`).join('')

    await route.fulfill({
      status: 200,
      contentType: 'text/event-stream',
      body: stream
    })
  }
}

export const createErrorMockHandler = (error: { status: number, error: unknown }) => {
  return async (route: Route) => {
    await route.fulfill({
      status: error.status,
      contentType: 'application/json',
      body: JSON.stringify(error.error)
    })
  }
}
