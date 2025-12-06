import { test, expect } from '@playwright/test'
import { aiMocks, createAIMockHandler, createStreamingMockHandler, createErrorMockHandler } from '../fixtures/ai-mocks'

/**
 * Avatar: Priya the Product Manager
 * JTBD: Product AI Research, Feature AI Feasibility, Technical AI Explanations
 * Focus: AI interactions for product decisions and understanding
 */

test.describe('Priya the Product Manager - AI-Focused JTBD', () => {
  test.describe('Job 1: Research product features via AI', () => {
    test('should ask AI about template features', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-1',
        role: 'assistant',
        content: `# Nuxt UI Docs Template Features

## Core Features
1. **AI-Powered Chat**: Integrated ChatPalette component with streaming support
2. **PGlite Database**: Embedded PostgreSQL, no external services needed
3. **GitHub OAuth**: Built-in authentication
4. **Nuxt Content**: MDC syntax for rich documentation
5. **Drizzle ORM**: Type-safe database queries
6. **Playwright E2E**: Comprehensive testing suite

## User Benefits
- **Zero Configuration**: Works out of the box
- **Production Ready**: Battle-tested patterns
- **Developer Experience**: Hot reload, TypeScript, modern tooling`
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'What features does this template support?' }
          ]
        }
      })

      expect(response.status()).toBe(200)
      const data = await response.json()
      expect(data.content).toContain('Features')
      expect(data.content).toContain('AI')
    })

    test('should get competitive analysis via AI', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-2',
        role: 'assistant',
        content: `# Comparison: Nuxt UI Docs vs Alternatives

## Nuxt UI Docs Template
**Pros**:
- PGlite = no external database
- AI chat built-in
- Zero config setup

**Cons**:
- Newer ecosystem

## VitePress
**Pros**:
- Mature, well-documented
- Fast SSG

**Cons**:
- No database support
- No built-in AI

## Docusaurus
**Pros**:
- Large ecosystem
- Versioning built-in

**Cons**:
- React-only
- No embedded database

**Recommendation**: Use Nuxt UI Docs for AI-first, database-backed documentation.`
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'How does this compare to VitePress and Docusaurus?' }
          ]
        }
      })

      const data = await response.json()
      expect(data.content).toContain('Comparison')
      expect(data.content).toContain('VitePress')
    })

    test('should stream market research insights', async ({ page }) => {
      await page.route('**/api/chat', createStreamingMockHandler(aiMocks.streaming.chunks))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'What are the trends in documentation tools?' }
          ]
        }
      })

      expect(response.status()).toBe(200)
      const body = await response.text()
      expect(body.length).toBeGreaterThan(0)
    })
  })

  test.describe('Job 2: Assess technical feasibility with AI', () => {
    test('should ask about integration complexity', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-3',
        role: 'assistant',
        content: `# Feature Feasibility: Custom Search

## Complexity: Medium
**Estimated effort**: 2-3 days

## Implementation Options

### Option 1: Nuxt Content Search (Easy)
\`\`\`typescript
const { data } = await queryContent('/').where({
  $or: [
    { title: { $icontains: searchTerm } },
    { description: { $icontains: searchTerm } }
  ]
}).find()
\`\`\`

### Option 2: AI-Powered Semantic Search (Medium)
Use embeddings + PGlite vector extension
Estimated: +1 day for vector setup

### Option 3: Algolia Integration (Easy)
Plugin available, but adds external dependency
Estimated: 1 day setup

**Recommendation**: Start with Option 1, add AI search later if needed.`
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'How hard is it to add custom search?' }
          ]
        }
      })

      const data = await response.json()
      expect(data.content).toContain('Feasibility')
      expect(data.content).toContain('Implementation')
    })

    test('should evaluate performance implications', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-4',
        role: 'assistant',
        content: aiMocks.complex.performanceOptimization.content
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'What are the performance implications of AI streaming?' }
          ]
        }
      })

      const data = await response.json()
      expect(data.content).toContain('performance')
      expect(data.content).toContain('streaming')
    })

    test('should handle complex technical questions with errors', async ({ page }) => {
      await page.route('**/api/chat', createErrorMockHandler(aiMocks.errors.rateLimited))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'Analyze all possible technical architectures' }
          ]
        },
        failOnStatusCode: false
      })

      expect(response.status()).toBe(429)
      const error = await response.json()
      expect(error.type).toBe('rate_limit_error')
    })
  })

  test.describe('Job 3: Understand technical concepts via AI explanations', () => {
    test('should get simple explanation of PGlite', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-5',
        role: 'assistant',
        content: `# PGlite Explained (Non-Technical)

## What is PGlite?
Think of it like SQLite, but for PostgreSQL. It's a database that runs **inside** your application.

## Why is this good?

### For Users
- **Zero setup**: No Docker, no PostgreSQL server needed
- **Fast**: Database is in the same process
- **Portable**: Database file travels with your app

### For Product
- **Lower costs**: No database hosting fees
- **Easier onboarding**: Contributors just run \`npm install\`
- **Better DX**: Works offline, no connection strings

## Real-World Analogy
Regular database = Restaurant kitchen (separate building)
PGlite = Kitchen in your home (same location)

Both cook food, but home kitchen is more convenient for small meals.`
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'Explain PGlite in simple terms' }
          ]
        }
      })

      const data = await response.json()
      expect(data.content).toContain('PGlite')
      expect(data.content).toContain('analogy' || 'Analogy')
    })

    test('should explain streaming chat architecture', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-6',
        role: 'assistant',
        content: `# Streaming Chat: How It Works

## The Problem
Traditional chat: User waits 5-10 seconds for complete response
**Bad UX** = User thinks it's broken

## The Solution: Streaming
AI sends response **word by word** as it generates

## User Experience
1. User types question
2. Sees "AI is typing..."
3. Words appear immediately: "Let..." → "Let me..." → "Let me help..."
4. Complete response in ~10 seconds, but user engaged from second 1

## Technical Flow
\`\`\`
User → Nuxt Server → AI SDK → OpenAI API
                    ↓ (streaming)
User ← Server ← EventStream ← Chunks
\`\`\`

## Business Value
- 3x higher engagement (users don't leave)
- Feels "intelligent" (real-time thinking)
- Better conversion on paid features`
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'Explain how streaming chat works from a product perspective' }
          ]
        }
      })

      const data = await response.json()
      expect(data.content).toContain('Streaming')
      expect(data.content).toContain('User')
    })

    test('should provide multi-turn product discussion', async ({ page }) => {
      let conversationTurn = 0

      await page.route('**/api/chat', async (route) => {
        conversationTurn++

        if (conversationTurn === 1) {
          await createAIMockHandler({
            id: 'msg-7',
            role: 'assistant',
            content: 'The main user segments are: developers looking for docs templates, teams needing AI chat, and solo creators wanting zero-config solutions.'
          })(route)
        } else {
          await createAIMockHandler({
            id: 'msg-8',
            role: 'assistant',
            content: `Solo creators prioritize:
1. **Time to first value**: < 5 minutes
2. **Zero cost**: No hosting fees
3. **Simple**: No DevOps knowledge needed

That's why PGlite + Vercel free tier is perfect.`
          })(route)
        }
      })

      const response1 = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [{ role: 'user', content: 'Who are our target users?' }]
        }
      })

      const data1 = await response1.json()
      expect(data1.content).toContain('user')

      const response2 = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'Who are our target users?' },
            { role: 'assistant', content: data1.content },
            { role: 'user', content: 'What do solo creators care about?' }
          ]
        }
      })

      const data2 = await response2.json()
      expect(data2.content).toContain('creators' || 'Solo')
    })
  })

  test.describe('Success Criteria: AI-Powered Product Research', () => {
    test('can research features via AI', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'success-1',
        role: 'assistant',
        content: 'This template includes AI chat, PGlite database, OAuth, and comprehensive testing.'
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [{ role: 'user', content: 'Feature list?' }]
        }
      })

      expect(response.status()).toBe(200)
      const data = await response.json()
      expect(data.role).toBe('assistant')
      expect(data.content.length).toBeGreaterThan(0)
    })

    test('can assess feasibility with AI streaming', async ({ page }) => {
      await page.route('**/api/chat', createStreamingMockHandler(aiMocks.streaming.chunks))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [{ role: 'user', content: 'Is this feature feasible?' }]
        }
      })

      expect(response.status()).toBe(200)
      const body = await response.text()
      expect(body.length).toBeGreaterThan(0)
    })

    test('can get technical explanations despite errors', async ({ page }) => {
      await page.route('**/api/chat', createErrorMockHandler(aiMocks.errors.modelOverloaded))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [{ role: 'user', content: 'Explain everything' }]
        },
        failOnStatusCode: false
      })

      expect([503, 504, 429]).toContain(response.status())
    })
  })
})
