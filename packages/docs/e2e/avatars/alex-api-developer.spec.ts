import { test, expect } from '@playwright/test'
import { aiMocks, createAIMockHandler, createStreamingMockHandler, createErrorMockHandler } from '../fixtures/ai-mocks'

/**
 * Avatar: Alex the API Developer
 * JTBD: API Documentation, AI Code Assistance, API Testing
 * Focus: AI interactions for code generation and API development
 */

test.describe('Alex the API Developer - AI-Focused JTBD', () => {
  test.describe('Job 1: Get AI assistance for API endpoint creation', () => {
    test('should request API endpoint code via AI', async ({ page }) => {
      // Mock AI response for API endpoint question
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-1',
        role: 'assistant',
        content: aiMocks.codeGeneration.apiEndpoint.content
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'How do I create a REST API endpoint?' }
          ]
        }
      })

      expect(response.status()).toBe(200)
      const data = await response.json()
      expect(data.content).toContain('defineEventHandler')
      expect(data.content).toContain('server/api')
    })

    test('should get streaming AI response for code generation', async ({ page }) => {
      // Mock streaming response
      await page.route('**/api/chat', createStreamingMockHandler(aiMocks.streaming.codeStreaming))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'Generate an API endpoint example' }
          ]
        }
      })

      expect(response.status()).toBe(200)
      const body = await response.text()
      expect(body).toContain('defineEventHandler')
    })

    test('should handle AI code generation with syntax highlighting', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-2',
        role: 'assistant',
        content: aiMocks.codeGeneration.apiEndpoint.content
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'Show me TypeScript API code' }
          ]
        }
      })

      const data = await response.json()
      expect(data.content).toContain('```typescript')
      expect(data.content).toContain('export default')
    })
  })

  test.describe('Job 2: Test API responses with AI-generated mocks', () => {
    test('should generate test data via AI', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-3',
        role: 'assistant',
        content: aiMocks.codeGeneration.testExample.content
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'How do I mock AI responses in tests?' }
          ]
        }
      })

      const data = await response.json()
      expect(data.content).toContain('MockLanguageModelV2')
      expect(data.content).toContain('doGenerate')
    })

    test('should handle AI errors gracefully', async ({ page }) => {
      await page.route('**/api/chat', createErrorMockHandler(aiMocks.errors.rateLimited))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'Generate lots of code' }
          ]
        },
        failOnStatusCode: false
      })

      expect(response.status()).toBe(429)
      const error = await response.json()
      expect(error.type).toBe('rate_limit_error')
    })

    test('should retry on AI service overload', async ({ page }) => {
      let requestCount = 0

      await page.route('**/api/chat', async (route) => {
        requestCount++

        if (requestCount === 1) {
          // First request fails
          await createErrorMockHandler(aiMocks.errors.modelOverloaded)(route)
        } else {
          // Retry succeeds
          await createAIMockHandler({
            id: 'msg-4',
            role: 'assistant',
            content: 'Here is your code after retry'
          })(route)
        }
      })

      // First request
      const firstResponse = await page.request.post('http://localhost:3000/api/chat', {
        data: { messages: [{ role: 'user', content: 'test' }] },
        failOnStatusCode: false
      })

      expect(firstResponse.status()).toBe(503)

      // Retry
      const retryResponse = await page.request.post('http://localhost:3000/api/chat', {
        data: { messages: [{ role: 'user', content: 'test' }] }
      })

      expect(retryResponse.status()).toBe(200)
      expect(requestCount).toBe(2)
    })
  })

  test.describe('Job 3: Document APIs with AI-generated examples', () => {
    test('should generate API documentation via AI', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-5',
        role: 'assistant',
        content: `# API Documentation

## POST /api/chats

Creates a new chat session.

**Request Body:**
\`\`\`json
{
  "title": "string"
}
\`\`\`

**Response:**
\`\`\`json
{
  "id": "string",
  "title": "string",
  "userId": "string",
  "createdAt": "ISO8601"
}
\`\`\``
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'Document the /api/chats endpoint' }
          ]
        }
      })

      const data = await response.json()
      expect(data.content).toContain('API Documentation')
      expect(data.content).toContain('POST /api/chats')
      expect(data.content).toContain('Request Body')
    })

    test('should explain database queries via AI', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-6',
        role: 'assistant',
        content: aiMocks.codeGeneration.databaseQuery.content
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'How do I query PGlite with Drizzle?' }
          ]
        }
      })

      const data = await response.json()
      expect(data.content).toContain('useDrizzle')
      expect(data.content).toContain('PGlite')
    })

    test('should provide multi-turn conversation for API help', async ({ page }) => {
      let conversationTurn = 0

      await page.route('**/api/chat', async (route) => {
        conversationTurn++

        if (conversationTurn === 1) {
          await createAIMockHandler({
            id: 'msg-7',
            role: 'assistant',
            content: 'You can create API endpoints in the server/api directory.'
          })(route)
        } else {
          await createAIMockHandler({
            id: 'msg-8',
            role: 'assistant',
            content: aiMocks.codeGeneration.apiEndpoint.content
          })(route)
        }
      })

      // First question
      const response1 = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'Where do I create API endpoints?' }
          ]
        }
      })

      const data1 = await response1.json()
      expect(data1.content).toContain('server/api')

      // Follow-up question
      const response2 = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'Where do I create API endpoints?' },
            { role: 'assistant', content: data1.content },
            { role: 'user', content: 'Can you show me an example?' }
          ]
        }
      })

      const data2 = await response2.json()
      expect(data2.content).toContain('```typescript')
    })
  })

  test.describe('Success Criteria: AI-Powered API Development', () => {
    test('can generate API code via AI', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'success-1',
        role: 'assistant',
        content: aiMocks.codeGeneration.apiEndpoint.content
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [{ role: 'user', content: 'API code please' }]
        }
      })

      expect(response.status()).toBe(200)
      const data = await response.json()
      expect(data.role).toBe('assistant')
      expect(data.content.length).toBeGreaterThan(0)
    })

    test('can handle AI streaming for real-time feedback', async ({ page }) => {
      await page.route('**/api/chat', createStreamingMockHandler(aiMocks.streaming.chunks))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [{ role: 'user', content: 'Stream response' }]
        }
      })

      expect(response.status()).toBe(200)
      const body = await response.text()
      expect(body.length).toBeGreaterThan(0)
    })

    test('can recover from AI errors', async ({ page }) => {
      await page.route('**/api/chat', createErrorMockHandler(aiMocks.errors.invalidApiKey))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [{ role: 'user', content: 'test' }]
        },
        failOnStatusCode: false
      })

      expect([401, 429, 503, 504]).toContain(response.status())
    })
  })
})
