import { test, expect } from '@playwright/test'
import { aiMocks, createAIMockHandler, createStreamingMockHandler, createErrorMockHandler } from '../fixtures/ai-mocks'

/**
 * Avatar: Marcus the DevOps Engineer
 * JTBD: Infrastructure AI Advice, Database AI Help, Deployment AI Guidance
 * Focus: AI interactions for DevOps and infrastructure tasks
 */

test.describe('Marcus the DevOps Engineer - AI-Focused JTBD', () => {
  test.describe('Job 1: Get AI advice for infrastructure setup', () => {
    test('should get PGlite configuration advice from AI', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-1',
        role: 'assistant',
        content: `To configure PGlite for production:

1. **Set up the database directory**:
\`\`\`typescript
import { PGlite } from '@electric-sql/pglite'

const db = new PGlite('./data/pglite')
\`\`\`

2. **Configure environment variables**:
\`\`\`bash
DATABASE_PATH=./data/pglite
\`\`\`

3. **Run migrations on startup**:
Use Drizzle's migration system to keep schema in sync.

PGlite runs in-process, no Docker or PostgreSQL server needed!`
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'How do I configure PGlite for production?' }
          ]
        }
      })

      expect(response.status()).toBe(200)
      const data = await response.json()
      expect(data.content).toContain('PGlite')
      expect(data.content).toContain('production')
    })

    test('should get deployment guidance via AI', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-2',
        role: 'assistant',
        content: `# Deployment Guide for Nuxt UI Docs

## Vercel Deployment
\`\`\`bash
npm install -g vercel
vercel deploy
\`\`\`

## Environment Variables
- \`NUXT_SESSION_PASSWORD\`: 32-character secret
- \`NUXT_OAUTH_GITHUB_CLIENT_ID\`: GitHub OAuth ID
- \`NUXT_OAUTH_GITHUB_CLIENT_SECRET\`: GitHub OAuth secret

## Database
PGlite runs in-process, persists to \`./data\` directory.
Ensure volume/storage is configured for persistence.

## Performance
- Enable caching with Nitro
- Use edge runtime when possible
- Prerender static content`
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'How do I deploy this to Vercel?' }
          ]
        }
      })

      const data = await response.json()
      expect(data.content).toContain('Deployment')
      expect(data.content).toContain('Vercel')
    })

    test('should handle streaming infrastructure advice', async ({ page }) => {
      await page.route('**/api/chat', createStreamingMockHandler(aiMocks.streaming.chunks))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'Tell me about scaling strategies' }
          ]
        }
      })

      expect(response.status()).toBe(200)
      const body = await response.text()
      expect(body.length).toBeGreaterThan(0)
    })
  })

  test.describe('Job 2: Get database optimization help from AI', () => {
    test('should get database query optimization advice', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-3',
        role: 'assistant',
        content: aiMocks.codeGeneration.databaseQuery.content
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'How do I optimize database queries with Drizzle?' }
          ]
        }
      })

      const data = await response.json()
      expect(data.content).toContain('Drizzle')
      expect(data.content).toContain('select')
    })

    test('should get migration strategies from AI', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-4',
        role: 'assistant',
        content: `# Database Migration Best Practices

## 1. Create Migration Files
\`\`\`bash
npx drizzle-kit generate:pg
\`\`\`

## 2. Version Control Migrations
Always commit migration files to git.

## 3. Run Migrations on Deploy
\`\`\`typescript
import { migrate } from 'drizzle-orm/pglite/migrator'
await migrate(db, { migrationsFolder: './migrations' })
\`\`\`

## 4. Rollback Strategy
Keep separate down migrations for critical changes.

## 5. Test Migrations
Run in staging environment first.`
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'What are database migration best practices?' }
          ]
        }
      })

      const data = await response.json()
      expect(data.content).toContain('Migration')
      expect(data.content).toContain('drizzle')
    })

    test('should handle database error scenarios', async ({ page }) => {
      await page.route('**/api/chat', createErrorMockHandler(aiMocks.errors.modelOverloaded))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'Explain all database patterns' }
          ]
        },
        failOnStatusCode: false
      })

      expect(response.status()).toBe(503)
      const error = await response.json()
      expect(error.type).toBe('service_unavailable')
    })
  })

  test.describe('Job 3: Troubleshoot deployment issues with AI', () => {
    test('should diagnose environment variable issues', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-5',
        role: 'assistant',
        content: `# Troubleshooting Environment Variables

## Common Issues

### 1. Missing SESSION_PASSWORD
**Error**: "Session password required"
**Fix**: Add to \`.env\`:
\`\`\`bash
NUXT_SESSION_PASSWORD=$(openssl rand -base64 32)
\`\`\`

### 2. OAuth Not Working
**Check**:
- CLIENT_ID and CLIENT_SECRET are set
- Callback URL matches GitHub settings
- Variables have NUXT_ prefix

### 3. Database Path Issues
**Fix**: Ensure write permissions:
\`\`\`bash
mkdir -p ./data
chmod 755 ./data
\`\`\``
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'My environment variables are not working' }
          ]
        }
      })

      const data = await response.json()
      expect(data.content).toContain('Environment')
      expect(data.content).toContain('NUXT_')
    })

    test('should provide monitoring and logging advice', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-6',
        role: 'assistant',
        content: `# Production Monitoring Setup

## 1. Logging
\`\`\`typescript
// server/utils/logger.ts
export const logger = {
  info: (msg: string) => console.log(\`[INFO] \${msg}\`),
  error: (msg: string, err?: Error) => console.error(\`[ERROR] \${msg}\`, err)
}
\`\`\`

## 2. Error Tracking
Integrate Sentry:
\`\`\`bash
npm install @sentry/nuxt
\`\`\`

## 3. Performance Monitoring
- Track API response times
- Monitor database query performance
- Set up alerts for errors

## 4. Health Check Endpoint
\`\`\`typescript
// server/api/health.get.ts
export default defineEventHandler(() => ({
  status: 'healthy',
  uptime: process.uptime()
}))
\`\`\``
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'How do I set up monitoring?' }
          ]
        }
      })

      const data = await response.json()
      expect(data.content).toContain('Monitoring')
      expect(data.content).toContain('logger')
    })

    test('should provide multi-turn troubleshooting conversation', async ({ page }) => {
      let conversationTurn = 0

      await page.route('**/api/chat', async (route) => {
        conversationTurn++

        if (conversationTurn === 1) {
          await createAIMockHandler({
            id: 'msg-7',
            role: 'assistant',
            content: 'Database errors can be caused by permissions, missing migrations, or connection issues.'
          })(route)
        } else {
          await createAIMockHandler({
            id: 'msg-8',
            role: 'assistant',
            content: `Check permissions:
\`\`\`bash
ls -la ./data
chmod 755 ./data
\`\`\`

Run migrations:
\`\`\`bash
npx drizzle-kit push:pg
\`\`\``
          })(route)
        }
      })

      const response1 = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [{ role: 'user', content: 'Database is not connecting' }]
        }
      })

      const data1 = await response1.json()
      expect(data1.content).toContain('Database')

      const response2 = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'Database is not connecting' },
            { role: 'assistant', content: data1.content },
            { role: 'user', content: 'How do I fix permissions?' }
          ]
        }
      })

      const data2 = await response2.json()
      expect(data2.content).toContain('chmod')
    })
  })

  test.describe('Success Criteria: AI-Powered DevOps', () => {
    test('can get infrastructure advice via AI', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'success-1',
        role: 'assistant',
        content: 'Use PGlite for embedded database, Drizzle for ORM, and Vercel for deployment.'
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [{ role: 'user', content: 'Infrastructure setup?' }]
        }
      })

      expect(response.status()).toBe(200)
      const data = await response.json()
      expect(data.role).toBe('assistant')
      expect(data.content.length).toBeGreaterThan(0)
    })

    test('can troubleshoot deployment issues with AI streaming', async ({ page }) => {
      await page.route('**/api/chat', createStreamingMockHandler(aiMocks.streaming.chunks))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [{ role: 'user', content: 'Deployment failing' }]
        }
      })

      expect(response.status()).toBe(200)
      const body = await response.text()
      expect(body.length).toBeGreaterThan(0)
    })

    test('can handle infrastructure query errors gracefully', async ({ page }) => {
      await page.route('**/api/chat', createErrorMockHandler(aiMocks.errors.timeout))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [{ role: 'user', content: 'Complex infrastructure question' }]
        },
        failOnStatusCode: false
      })

      expect([504, 503, 429]).toContain(response.status())
    })
  })
})
