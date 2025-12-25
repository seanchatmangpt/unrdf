import { test, expect } from '@playwright/test'
import { aiMocks, createAIMockHandler, createStreamingMockHandler, createErrorMockHandler } from '../fixtures/ai-mocks'

/**
 * Avatar: Jasmine the QA Engineer
 * JTBD: Test AI Generation, Mock AI Creation, Coverage AI Analysis
 * Focus: AI interactions for testing and quality assurance
 */

test.describe('Jasmine the QA Engineer - AI-Focused JTBD', () => {
  test.describe('Job 1: Generate test code with AI assistance', () => {
    test('should get unit test generation from AI', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-1',
        role: 'assistant',
        content: `# Unit Test Example

\`\`\`typescript
import { describe, it, expect } from 'vitest'
import { useDrizzle } from '~/server/utils/drizzle'

describe('Chat API', () => {
  it('should create a new chat', async () => {
    const db = useDrizzle()

    const [chat] = await db
      .insert(tables.chats)
      .values({
        title: 'Test Chat',
        userId: 'test-user'
      })
      .returning()

    expect(chat.title).toBe('Test Chat')
    expect(chat.userId).toBe('test-user')
  })

  it('should get all chats for user', async () => {
    const db = useDrizzle()

    const chats = await db
      .select()
      .from(tables.chats)
      .where(eq(tables.chats.userId, 'test-user'))
      .all()

    expect(Array.isArray(chats)).toBe(true)
  })
})
\`\`\``
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'Generate unit tests for chat API' }
          ]
        }
      })

      expect(response.status()).toBe(200)
      const data = await response.json()
      expect(data.content).toMatch(/test|Test/)
      expect(data.content).toContain('expect')
    })

    test('should get E2E test code from AI', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-2',
        role: 'assistant',
        content: `# E2E Test with Playwright

\`\`\`typescript
import { test, expect } from '@playwright/test'

test.describe('Chat Feature', () => {
  test('should create and display chat message', async ({ page }) => {
    // Mock AI response
    await page.route('**/api/chat', async (route) => {
      await route.fulfill({
        status: 200,
        contentType: 'application/json',
        body: JSON.stringify({
          id: 'msg-1',
          role: 'assistant',
          content: 'Hello from AI'
        })
      })
    })

    await page.goto('/')

    // Type message
    const input = page.getByPlaceholder('Ask a question')
    await input.fill('Hello AI')

    // Submit
    const sendButton = page.getByRole('button', { name: 'Send' })
    await sendButton.click()

    // Verify response appears
    await expect(page.getByText('Hello from AI')).toBeVisible()
  })
})
\`\`\``
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'Write an E2E test for chat feature' }
          ]
        }
      })

      const data = await response.json()
      expect(data.content).toMatch(/Playwright|E2E/)
      expect(data.content).toContain('page.route')
    })

    test('should stream test generation code', async ({ page }) => {
      await page.route('**/api/chat', createStreamingMockHandler(aiMocks.streaming.codeStreaming))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'Generate comprehensive test suite' }
          ]
        }
      })

      expect(response.status()).toBe(200)
      const body = await response.text()
      expect(body).toContain('typescript')
    })
  })

  test.describe('Job 2: Create AI mocks with AI help', () => {
    test('should get AI SDK mock examples', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-3',
        role: 'assistant',
        content: aiMocks.codeGeneration.testExample.content
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'How do I mock AI SDK responses?' }
          ]
        }
      })

      const data = await response.json()
      expect(data.content).toContain('Mock')
      expect(data.content).toMatch(/AI|ai/)
    })

    test('should get route mocking patterns', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-4',
        role: 'assistant',
        content: `# Route Mocking Patterns

## Mock AI Chat Response
\`\`\`typescript
await page.route('**/api/chat', async (route) => {
  await route.fulfill({
    status: 200,
    contentType: 'application/json',
    body: JSON.stringify({
      id: 'msg-1',
      role: 'assistant',
      content: 'Mocked response'
    })
  })
})
\`\`\`

## Mock Streaming Response
\`\`\`typescript
await page.route('**/api/chat', async (route) => {
  const chunks = [
    'data: {"type":"text-delta","textDelta":"Hello"}\\n\\n',
    'data: {"type":"finish","finishReason":"stop"}\\n\\n'
  ]

  await route.fulfill({
    status: 200,
    contentType: 'text/event-stream',
    body: chunks.join('')
  })
})
\`\`\`

## Mock Error Response
\`\`\`typescript
await page.route('**/api/chat', async (route) => {
  await route.fulfill({
    status: 429,
    contentType: 'application/json',
    body: JSON.stringify({
      error: 'Rate limit exceeded'
    })
  })
})
\`\`\``
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'Show me route mocking examples' }
          ]
        }
      })

      const data = await response.json()
      expect(data.content).toContain('route')
      expect(data.content).toContain('Mock')
    })

    test('should handle mock generation errors', async ({ page }) => {
      await page.route('**/api/chat', createErrorMockHandler(aiMocks.errors.rateLimited))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'Generate 100 different mock scenarios' }
          ]
        },
        failOnStatusCode: false
      })

      expect(response.status()).toBe(429)
      const error = await response.json()
      expect(error.type).toBe('rate_limit_error')
    })
  })

  test.describe('Job 3: Analyze test coverage with AI', () => {
    test('should get coverage analysis from AI', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-5',
        role: 'assistant',
        content: `# Test Coverage Analysis

## Current Coverage Gaps

### 1. Error Handling (40% coverage)
**Missing tests**:
- Rate limit errors
- Network timeouts
- Invalid API keys
- Model overload scenarios

**Recommendation**: Add error handling tests:
\`\`\`typescript
test('should handle rate limit error', async ({ page }) => {
  await page.route('**/api/chat', createErrorMockHandler({
    status: 429,
    error: { message: 'Rate limited' }
  }))

  // Verify error is displayed to user
})
\`\`\`

### 2. Edge Cases (55% coverage)
**Missing**:
- Empty message handling
- Very long messages (>10k chars)
- Special characters in input
- Rapid consecutive requests

### 3. Multi-turn Conversations (30% coverage)
**Add**: Conversation context persistence tests

## Priority Actions
1. Add error scenario tests (High)
2. Test edge cases (Medium)
3. Multi-turn conversation tests (Low)

**Target**: 80%+ coverage`
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'Analyze test coverage gaps' }
          ]
        }
      })

      const data = await response.json()
      expect(data.content).toMatch(/Coverage|coverage/)
      expect(data.content).toMatch(/test|Test/)
    })

    test('should get test improvement suggestions', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-6',
        role: 'assistant',
        content: `# Test Improvement Suggestions

## 1. Add Parameterized Tests
Instead of multiple similar tests, use test.each:

\`\`\`typescript
test.each([
  { status: 429, error: 'rate_limit' },
  { status: 503, error: 'service_unavailable' },
  { status: 504, error: 'timeout' }
])('should handle $status error', async ({ status, error }) => {
  await page.route('**/api/chat', createErrorMockHandler({
    status,
    error: { type: error }
  }))

  // Test error handling
})
\`\`\`

## 2. Use Page Object Pattern
\`\`\`typescript
class ChatPage {
  constructor(private page: Page) {}

  async sendMessage(text: string) {
    await this.page.getByPlaceholder('Ask').fill(text)
    await this.page.getByRole('button', { name: 'Send' }).click()
  }

  async getLastMessage() {
    return this.page.locator('.message').last()
  }
}
\`\`\`

## 3. Add Visual Regression Tests
Use Playwright's screenshot comparison

## 4. Test Accessibility
Add axe-core integration for a11y testing`
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'How can I improve my tests?' }
          ]
        }
      })

      const data = await response.json()
      expect(data.content).toMatch(/Test|test/)
      expect(data.content).toMatch(/improvement|Improvement/)
    })

    test('should provide multi-turn testing conversation', async ({ page }) => {
      let conversationTurn = 0

      await page.route('**/api/chat', async (route) => {
        conversationTurn++

        if (conversationTurn === 1) {
          await createAIMockHandler({
            id: 'msg-7',
            role: 'assistant',
            content: 'Key testing strategies: unit tests for logic, integration tests for APIs, E2E tests for user flows.'
          })(route)
        } else {
          await createAIMockHandler({
            id: 'msg-8',
            role: 'assistant',
            content: `For AI SDK testing, use MockLanguageModelV2:
\`\`\`typescript
const model = new MockLanguageModelV2({
  doGenerate: async () => ({
    text: 'Mocked response',
    finishReason: 'stop'
  })
})
\`\`\``
          })(route)
        }
      })

      const response1 = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [{ role: 'user', content: 'What testing strategies should I use?' }]
        }
      })

      const data1 = await response1.json()
      expect(data1.content).toContain('test')

      const response2 = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'What testing strategies should I use?' },
            { role: 'assistant', content: data1.content },
            { role: 'user', content: 'How do I mock the AI SDK?' }
          ]
        }
      })

      const data2 = await response2.json()
      expect(data2.content).toContain('Mock')
    })
  })

  test.describe('Success Criteria: AI-Powered Testing', () => {
    test('can generate test code via AI', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'success-1',
        role: 'assistant',
        content: aiMocks.codeGeneration.testExample.content
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [{ role: 'user', content: 'Generate test' }]
        }
      })

      expect(response.status()).toBe(200)
      const data = await response.json()
      expect(data.role).toBe('assistant')
      expect(data.content.length).toBeGreaterThan(0)
    })

    test('can create mocks with AI streaming', async ({ page }) => {
      await page.route('**/api/chat', createStreamingMockHandler(aiMocks.streaming.chunks))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [{ role: 'user', content: 'Create mocks' }]
        }
      })

      expect(response.status()).toBe(200)
      const body = await response.text()
      expect(body.length).toBeGreaterThan(0)
    })

    test('can analyze coverage despite AI errors', async ({ page }) => {
      await page.route('**/api/chat', createErrorMockHandler(aiMocks.errors.timeout))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [{ role: 'user', content: 'Deep coverage analysis' }]
        },
        failOnStatusCode: false
      })

      expect([504, 503, 429]).toContain(response.status())
    })
  })
})
