import { test, expect } from '@playwright/test'
import { aiMocks, createAIMockHandler, createStreamingMockHandler, createErrorMockHandler } from '../fixtures/ai-mocks'

/**
 * Avatar: Chen the Full-Stack Developer
 * JTBD: Full-Stack AI Coding, Streaming Chat AI, CRUD AI Examples
 * Focus: AI interactions for full-stack development tasks
 */

test.describe('Chen the Full-Stack Developer - AI-Focused JTBD', () => {
  test.describe('Job 1: Get AI help for full-stack code generation', () => {
    test('should generate complete API endpoint with AI', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-1',
        role: 'assistant',
        content: aiMocks.codeGeneration.apiEndpoint.content
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'Create a REST API endpoint for users' }
          ]
        }
      })

      expect(response.status()).toBe(200)
      const data = await response.json()
      expect(data.content).toContain('server/api')
      expect(data.content).toContain('defineEventHandler')
    })

    test('should get component code via streaming AI', async ({ page }) => {
      await page.route('**/api/chat', createStreamingMockHandler(aiMocks.streaming.codeStreaming))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'Generate a Vue component with chat interface' }
          ]
        }
      })

      expect(response.status()).toBe(200)
      const body = await response.text()
      expect(body).toContain('typescript')
    })

    test('should get database integration code', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-2',
        role: 'assistant',
        content: aiMocks.codeGeneration.databaseQuery.content
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'Show me how to query the database with Drizzle' }
          ]
        }
      })

      const data = await response.json()
      expect(data.content).toContain('Drizzle')
      expect(data.content).toContain('useDrizzle')
    })
  })

  test.describe('Job 2: Implement streaming chat with AI assistance', () => {
    test('should get ChatPalette implementation example', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-3',
        role: 'assistant',
        content: aiMocks.codeGeneration.componentExample.content
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'How do I implement ChatPalette component?' }
          ]
        }
      })

      const data = await response.json()
      expect(data.content).toContain('ChatPalette')
      expect(data.content).toContain('useChat')
      expect(data.content).toContain('streaming')
    })

    test('should debug streaming issues with AI', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-4',
        role: 'assistant',
        content: `# Debugging Streaming Chat Issues

## Common Problems & Solutions

### 1. Stream Not Appearing
**Cause**: Missing \`toDataStreamResponse()\`
\`\`\`typescript
// ❌ Wrong
return result

// ✅ Correct
return result.toDataStreamResponse()
\`\`\`

### 2. Chunks Not Rendering
**Cause**: Missing \`useChat\` composable
\`\`\`typescript
// Component must use:
const { messages, input, handleSubmit } = useChat({
  api: '/api/chat'
})
\`\`\`

### 3. CORS Errors
**Solution**: Ensure headers are set:
\`\`\`typescript
headers: {
  'Content-Type': 'text/event-stream',
  'Cache-Control': 'no-cache',
  'Connection': 'keep-alive'
}
\`\`\`

### 4. Rate Limiting
Implement debouncing on user input`
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'My streaming chat is not working' }
          ]
        }
      })

      const data = await response.json()
      expect(data.content).toContain('Streaming')
      expect(data.content).toMatch(/toDataStreamResponse|useChat/)
    })

    test('should handle AI streaming errors gracefully', async ({ page }) => {
      await page.route('**/api/chat', createErrorMockHandler(aiMocks.errors.modelOverloaded))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'Generate complex streaming implementation' }
          ]
        },
        failOnStatusCode: false
      })

      expect(response.status()).toBe(503)
      const error = await response.json()
      expect(error.type).toBe('service_unavailable')
    })
  })

  test.describe('Job 3: Build CRUD operations with AI code examples', () => {
    test('should get CREATE operation code', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-5',
        role: 'assistant',
        content: `# CREATE Operation with Drizzle

\`\`\`typescript
// server/api/chats.post.ts
export default defineEventHandler(async (event) => {
  const body = await readBody(event)

  const db = useDrizzle()

  const [newChat] = await db
    .insert(tables.chats)
    .values({
      title: body.title,
      userId: event.context.user.id,
      createdAt: new Date()
    })
    .returning()

  return newChat
})
\`\`\`

**Frontend Usage**:
\`\`\`typescript
const { data } = await useFetch('/api/chats', {
  method: 'POST',
  body: { title: 'New Chat' }
})
\`\`\``
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'Show me how to create a new chat record' }
          ]
        }
      })

      const data = await response.json()
      expect(data.content).toContain('CREATE')
      expect(data.content).toContain('insert')
    })

    test('should get READ operation code', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-6',
        role: 'assistant',
        content: `# READ Operations with Drizzle

## Get All Chats
\`\`\`typescript
// server/api/chats.get.ts
export default defineEventHandler(async (event) => {
  const db = useDrizzle()

  const chats = await db
    .select()
    .from(tables.chats)
    .where(eq(tables.chats.userId, event.context.user.id))
    .orderBy(desc(tables.chats.createdAt))
    .all()

  return chats
})
\`\`\`

## Get Single Chat
\`\`\`typescript
// server/api/chats/[id].get.ts
export default defineEventHandler(async (event) => {
  const id = getRouterParam(event, 'id')
  const db = useDrizzle()

  const chat = await db
    .select()
    .from(tables.chats)
    .where(eq(tables.chats.id, id))
    .get()

  if (!chat) {
    throw createError({ statusCode: 404, message: 'Chat not found' })
  }

  return chat
})
\`\`\``
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'How do I read chat records from database?' }
          ]
        }
      })

      const data = await response.json()
      expect(data.content).toContain('READ')
      expect(data.content).toContain('select')
    })

    test('should get UPDATE and DELETE operations', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-7',
        role: 'assistant',
        content: `# UPDATE & DELETE Operations

## UPDATE
\`\`\`typescript
// server/api/chats/[id].patch.ts
export default defineEventHandler(async (event) => {
  const id = getRouterParam(event, 'id')
  const body = await readBody(event)
  const db = useDrizzle()

  const [updated] = await db
    .update(tables.chats)
    .set({ title: body.title })
    .where(eq(tables.chats.id, id))
    .returning()

  return updated
})
\`\`\`

## DELETE
\`\`\`typescript
// server/api/chats/[id].delete.ts
export default defineEventHandler(async (event) => {
  const id = getRouterParam(event, 'id')
  const db = useDrizzle()

  await db
    .delete(tables.chats)
    .where(eq(tables.chats.id, id))

  return { success: true }
})
\`\`\``
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'Show UPDATE and DELETE examples' }
          ]
        }
      })

      const data = await response.json()
      expect(data.content).toContain('UPDATE')
      expect(data.content).toContain('DELETE')
    })

    test('should provide multi-turn CRUD conversation', async ({ page }) => {
      let conversationTurn = 0

      await page.route('**/api/chat', async (route) => {
        conversationTurn++

        if (conversationTurn === 1) {
          await createAIMockHandler({
            id: 'msg-8',
            role: 'assistant',
            content: 'CRUD operations need: CREATE (POST), READ (GET), UPDATE (PATCH/PUT), DELETE (DELETE)'
          })(route)
        } else {
          await createAIMockHandler({
            id: 'msg-9',
            role: 'assistant',
            content: `For validation, use Zod:
\`\`\`typescript
import { z } from 'zod'

const ChatSchema = z.object({
  title: z.string().min(1).max(100)
})

const body = await readValidatedBody(event, ChatSchema.parse)
\`\`\``
          })(route)
        }
      })

      const response1 = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [{ role: 'user', content: 'What CRUD operations do I need?' }]
        }
      })

      const data1 = await response1.json()
      expect(data1.content).toContain('CRUD')

      const response2 = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'What CRUD operations do I need?' },
            { role: 'assistant', content: data1.content },
            { role: 'user', content: 'How do I validate inputs?' }
          ]
        }
      })

      const data2 = await response2.json()
      expect(data2.content).toMatch(/Zod|validation/)
    })
  })

  test.describe('Success Criteria: AI-Powered Full-Stack Development', () => {
    test('can generate full-stack code via AI', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'success-1',
        role: 'assistant',
        content: aiMocks.codeGeneration.componentExample.content
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [{ role: 'user', content: 'Full-stack chat example' }]
        }
      })

      expect(response.status()).toBe(200)
      const data = await response.json()
      expect(data.role).toBe('assistant')
      expect(data.content.length).toBeGreaterThan(0)
    })

    test('can implement streaming chat with AI guidance', async ({ page }) => {
      await page.route('**/api/chat', createStreamingMockHandler(aiMocks.streaming.codeStreaming))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [{ role: 'user', content: 'Streaming implementation' }]
        }
      })

      expect(response.status()).toBe(200)
      const body = await response.text()
      expect(body.length).toBeGreaterThan(0)
    })

    test('can handle CRUD errors with proper AI feedback', async ({ page }) => {
      await page.route('**/api/chat', createErrorMockHandler(aiMocks.errors.rateLimited))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [{ role: 'user', content: 'All CRUD examples' }]
        },
        failOnStatusCode: false
      })

      expect([429, 503, 504]).toContain(response.status())
    })
  })
})
