import { test, expect } from '@playwright/test'
import { aiMocks, createAIMockHandler, createStreamingMockHandler, createErrorMockHandler } from '../fixtures/ai-mocks'

/**
 * Avatar: Sofia the Technical Writer
 * JTBD: Content Generation, Markdown AI Assistance, Documentation AI
 * Focus: AI interactions for documentation and content creation
 */

test.describe('Sofia the Technical Writer - AI-Focused JTBD', () => {
  test.describe('Job 1: Generate documentation content with AI', () => {
    test('should generate Getting Started guide via AI', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-1',
        role: 'assistant',
        content: `# Getting Started

Welcome to our documentation platform! Here's what you need to know:

## Installation

\`\`\`bash
npm install @nuxt/ui
\`\`\`

## Quick Start

Create your first page in minutes with our intuitive interface.`
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'Write a Getting Started guide introduction' }
          ]
        }
      })

      expect(response.status()).toBe(200)
      const data = await response.json()
      expect(data.content).toContain('Getting Started')
      expect(data.content).toContain('Installation')
    })

    test('should get AI help for markdown syntax', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-2',
        role: 'assistant',
        content: `Here are common markdown syntax examples:

## Headers
\`# H1\`, \`## H2\`, \`### H3\`

## Code Blocks
\`\`\`typescript
const example = "code"
\`\`\`

## Lists
- Item 1
- Item 2

## Links
[Link text](url)`
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'Show me markdown syntax examples' }
          ]
        }
      })

      const data = await response.json()
      expect(data.content).toContain('markdown')
      expect(data.content).toContain('```')
    })

    test('should generate code examples with AI streaming', async ({ page }) => {
      await page.route('**/api/chat', createStreamingMockHandler(aiMocks.streaming.codeStreaming))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'Generate a code block example for docs' }
          ]
        }
      })

      expect(response.status()).toBe(200)
      const body = await response.text()
      expect(body).toContain('typescript')
    })
  })

  test.describe('Job 2: Improve existing content with AI suggestions', () => {
    test('should get AI suggestions for content improvement', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-3',
        role: 'assistant',
        content: `Here are suggestions to improve your documentation:

1. **Add more examples**: Code examples help users understand concepts faster
2. **Use clear headings**: Break content into scannable sections
3. **Include troubleshooting**: Add a FAQ section for common issues
4. **Add diagrams**: Visual aids improve comprehension
5. **Keep it concise**: Shorter sentences are easier to read`
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'How can I improve my API documentation?' }
          ]
        }
      })

      const data = await response.json()
      expect(data.content).toContain('suggestions')
      expect(data.content).toContain('examples')
    })

    test('should rewrite technical content for clarity', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-4',
        role: 'assistant',
        content: `**Before:**
The utilization of PGlite facilitates the instantiation of PostgreSQL-compatible database instances.

**After:**
PGlite lets you run PostgreSQL databases directly in your application without external setup.

**Improvements:**
- Removed jargon
- Simplified sentence structure
- Added practical benefit`
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'Rewrite this for non-technical users: The utilization of PGlite facilitates...' }
          ]
        }
      })

      const data = await response.json()
      expect(data.content).toContain('Before')
      expect(data.content).toContain('After')
    })

    test('should handle AI errors when generating content', async ({ page }) => {
      await page.route('**/api/chat', createErrorMockHandler(aiMocks.errors.rateLimited))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'Generate 100 documentation pages' }
          ]
        },
        failOnStatusCode: false
      })

      expect(response.status()).toBe(429)
      const error = await response.json()
      expect(error.type).toBe('rate_limit_error')
    })
  })

  test.describe('Job 3: Generate technical tutorials with AI', () => {
    test('should create step-by-step tutorial via AI', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-5',
        role: 'assistant',
        content: `# Tutorial: Building Your First API

## Step 1: Set Up Your Project
\`\`\`bash
mkdir my-api && cd my-api
npm init -y
\`\`\`

## Step 2: Install Dependencies
\`\`\`bash
npm install @nuxt/ui
\`\`\`

## Step 3: Create Your First Endpoint
\`\`\`typescript
// server/api/hello.get.ts
export default defineEventHandler(() => {
  return { message: 'Hello World' }
})
\`\`\`

## Step 4: Test Your API
Visit http://localhost:3000/api/hello`
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'Create a tutorial for building an API' }
          ]
        }
      })

      const data = await response.json()
      expect(data.content).toContain('Tutorial')
      expect(data.content).toContain('Step 1')
      expect(data.content).toContain('```')
    })

    test('should generate component usage documentation', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-6',
        role: 'assistant',
        content: aiMocks.codeGeneration.componentExample.content
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'Document the ChatPalette component' }
          ]
        }
      })

      const data = await response.json()
      expect(data.content).toContain('ChatPalette')
      expect(data.content).toContain('useChat')
    })

    test('should provide multi-turn conversation for documentation help', async ({ page }) => {
      let conversationTurn = 0

      await page.route('**/api/chat', async (route) => {
        conversationTurn++

        if (conversationTurn === 1) {
          await createAIMockHandler({
            id: 'msg-7',
            role: 'assistant',
            content: 'Documentation should focus on user goals and provide clear examples.'
          })(route)
        } else {
          await createAIMockHandler({
            id: 'msg-8',
            role: 'assistant',
            content: `Here's a template:

## [Feature Name]

**What it does**: Brief description

**When to use**: Use cases

**Example**:
\`\`\`typescript
// Code example
\`\`\``
          })(route)
        }
      })

      const response1 = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [{ role: 'user', content: 'What makes good documentation?' }]
        }
      })

      const data1 = await response1.json()
      expect(data1.content).toContain('examples')

      const response2 = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'What makes good documentation?' },
            { role: 'assistant', content: data1.content },
            { role: 'user', content: 'Can you give me a template?' }
          ]
        }
      })

      const data2 = await response2.json()
      expect(data2.content).toContain('template')
    })
  })

  test.describe('Success Criteria: AI-Powered Documentation', () => {
    test('can generate documentation content via AI', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'success-1',
        role: 'assistant',
        content: '# API Documentation\n\nYour complete guide to using our API.'
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [{ role: 'user', content: 'Generate API docs' }]
        }
      })

      expect(response.status()).toBe(200)
      const data = await response.json()
      expect(data.role).toBe('assistant')
      expect(data.content.length).toBeGreaterThan(0)
    })

    test('can stream markdown content for real-time preview', async ({ page }) => {
      await page.route('**/api/chat', createStreamingMockHandler(aiMocks.streaming.chunks))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [{ role: 'user', content: 'Write content' }]
        }
      })

      expect(response.status()).toBe(200)
      const body = await response.text()
      expect(body.length).toBeGreaterThan(0)
    })

    test('can handle content generation errors', async ({ page }) => {
      await page.route('**/api/chat', createErrorMockHandler(aiMocks.errors.timeout))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [{ role: 'user', content: 'Generate very long content' }]
        },
        failOnStatusCode: false
      })

      expect([504, 503, 429]).toContain(response.status())
    })
  })
})
