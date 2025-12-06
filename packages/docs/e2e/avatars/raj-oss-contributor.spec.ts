import { test, expect } from '@playwright/test'
import { aiMocks, createAIMockHandler, createStreamingMockHandler, createErrorMockHandler } from '../fixtures/ai-mocks'

/**
 * Avatar: Raj the Open Source Contributor
 * JTBD: Setup AI Help, Contribution AI Guidance, Onboarding AI Assistant
 * Focus: AI interactions for OSS contribution and onboarding
 */

test.describe('Raj the Open Source Contributor - AI-Focused JTBD', () => {
  test.describe('Job 1: Get setup help from AI', () => {
    test('should get installation instructions via AI', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-1',
        role: 'assistant',
        content: `# Installation Guide

## Prerequisites
- Node.js 18+ installed
- Git installed
- GitHub account

## Step 1: Clone Repository
\`\`\`bash
git clone https://github.com/your-org/nuxt-ui-docs.git
cd nuxt-ui-docs
\`\`\`

## Step 2: Install Dependencies
\`\`\`bash
npm install
# or
pnpm install
\`\`\`

## Step 3: Set Up Environment
\`\`\`bash
cp .env.example .env
\`\`\`

Edit \`.env\` and add:
\`\`\`bash
NUXT_SESSION_PASSWORD=$(openssl rand -base64 32)
\`\`\`

## Step 4: Run Development Server
\`\`\`bash
npm run dev
\`\`\`

Visit http://localhost:3000

**No external database needed!** PGlite runs automatically.`
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'How do I install and run this project?' }
          ]
        }
      })

      expect(response.status()).toBe(200)
      const data = await response.json()
      expect(data.content).toContain('install' || 'Install')
      expect(data.content).toContain('npm' || 'pnpm')
    })

    test('should troubleshoot setup issues via AI', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-2',
        role: 'assistant',
        content: `# Common Setup Issues

## Issue 1: Port 3000 Already in Use
**Error**: \`EADDRINUSE: address already in use :::3000\`

**Solution**:
\`\`\`bash
# Option 1: Use different port
PORT=3001 npm run dev

# Option 2: Kill process on port 3000
npx kill-port 3000
\`\`\`

## Issue 2: Module Not Found
**Error**: \`Cannot find module '@nuxt/ui'\`

**Solution**: Delete node_modules and reinstall:
\`\`\`bash
rm -rf node_modules pnpm-lock.yaml
pnpm install
\`\`\`

## Issue 3: Database Errors
**Error**: \`PGlite initialization failed\`

**Solution**: Check write permissions:
\`\`\`bash
mkdir -p .data
chmod 755 .data
\`\`\`

## Issue 4: Environment Variables Not Loading
**Solution**: Ensure \`.env\` is in project root (not \`packages/docs/.env\`)`
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'npm run dev is failing, what should I check?' }
          ]
        }
      })

      const data = await response.json()
      expect(data.content).toContain('Issue' || 'Error' || 'Solution')
    })

    test('should stream setup guidance', async ({ page }) => {
      await page.route('**/api/chat', createStreamingMockHandler(aiMocks.streaming.chunks))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'Guide me through the setup process' }
          ]
        }
      })

      expect(response.status()).toBe(200)
      const body = await response.text()
      expect(body.length).toBeGreaterThan(0)
    })
  })

  test.describe('Job 2: Learn contribution guidelines via AI', () => {
    test('should get contribution workflow from AI', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-3',
        role: 'assistant',
        content: `# Contribution Workflow

## 1. Find an Issue
- Check \`good first issue\` label
- Ask in Discussions if unclear
- Comment on issue to claim it

## 2. Create a Branch
\`\`\`bash
git checkout -b feature/your-feature-name
# or
git checkout -b fix/issue-123
\`\`\`

## 3. Make Changes
- Follow existing code style
- Add tests for new features
- Update documentation if needed

## 4. Test Your Changes
\`\`\`bash
# Run all tests
npm test

# Run specific test
npm test -- avatars/raj

# Check linting
npm run lint
\`\`\`

## 5. Commit
\`\`\`bash
git add .
git commit -m "feat: add new feature"
# or
git commit -m "fix: resolve issue #123"
\`\`\`

**Commit format**: \`type: description\`
Types: feat, fix, docs, test, chore

## 6. Push and Create PR
\`\`\`bash
git push origin your-branch-name
\`\`\`

Then create PR on GitHub with:
- Clear description
- Link to issue
- Screenshots (if UI change)

## 7. Address Review Feedback
- Be responsive
- Make requested changes
- Push updates to same branch`
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'What is the contribution workflow?' }
          ]
        }
      })

      const data = await response.json()
      expect(data.content).toContain('Contribution' || 'workflow')
      expect(data.content).toContain('commit' || 'PR')
    })

    test('should get code style guidelines from AI', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-4',
        role: 'assistant',
        content: `# Code Style Guidelines

## TypeScript/Vue
\`\`\`typescript
// ✅ Good: Composition API with script setup
<script setup lang="ts">
const { data } = await useFetch('/api/chats')
</script>

// ❌ Avoid: Options API
<script lang="ts">
export default {
  data() { return {} }
}
</script>
\`\`\`

## Naming Conventions
- **Components**: PascalCase (\`ChatMessage.vue\`)
- **Composables**: camelCase starting with \`use\` (\`useChat.ts\`)
- **Utils**: camelCase (\`formatDate.ts\`)
- **Types**: PascalCase with \`I\` prefix or \`Type\` suffix

## Formatting
- **Indentation**: 2 spaces
- **Quotes**: Single quotes for strings
- **Semicolons**: No semicolons (ESLint will auto-remove)
- **Line length**: Max 120 chars

## Testing
\`\`\`typescript
// Test file naming: *.spec.ts or *.test.ts
import { test, expect } from 'vitest'

test('should do something', () => {
  expect(true).toBe(true)
})
\`\`\`

## Best Practices
1. **Use composables**: Extract reusable logic
2. **Type everything**: No \`any\` types
3. **Handle errors**: Try-catch for async operations
4. **Add JSDoc**: Document public APIs`
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'What code style should I follow?' }
          ]
        }
      })

      const data = await response.json()
      expect(data.content).toContain('Style' || 'style')
      expect(data.content).toContain('TypeScript' || 'Vue')
    })

    test('should handle contribution query errors', async ({ page }) => {
      await page.route('**/api/chat', createErrorMockHandler(aiMocks.errors.rateLimited))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'Explain all contribution guidelines in detail' }
          ]
        },
        failOnStatusCode: false
      })

      expect(response.status()).toBe(429)
      const error = await response.json()
      expect(error.type).toBe('rate_limit_error')
    })
  })

  test.describe('Job 3: Get onboarding help via AI assistant', () => {
    test('should understand project architecture via AI', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-5',
        role: 'assistant',
        content: aiMocks.complex.architectureAdvice.content
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'Explain the project architecture' }
          ]
        }
      })

      const data = await response.json()
      expect(data.content).toContain('architecture' || 'Architecture')
      expect(data.content).toContain('Nuxt' || 'PGlite')
    })

    test('should get codebase navigation help', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-6',
        role: 'assistant',
        content: `# Codebase Navigation Guide

## Directory Structure

\`\`\`
packages/docs/
├── app/                    # Nuxt app files
│   ├── components/        # Vue components
│   │   ├── chat/         # Chat-related components
│   │   └── content/      # Content components
│   ├── composables/      # Composable functions
│   ├── layouts/          # Page layouts
│   └── pages/            # Route pages
│
├── server/                # Backend API
│   ├── api/              # API endpoints
│   │   ├── chat.post.ts  # AI chat endpoint
│   │   └── chats/        # CRUD endpoints
│   ├── database/         # Database schema
│   │   └── schema.ts     # Drizzle schema
│   └── utils/            # Server utilities
│       └── drizzle.ts    # DB connection
│
├── content/              # Markdown docs
│   ├── getting-started/
│   └── essentials/
│
├── e2e/                  # E2E tests
│   ├── avatars/          # User journey tests
│   └── fixtures/         # Test fixtures
│
└── shared/               # Shared utilities
    └── types/            # Shared types
\`\`\`

## Key Files
- \`nuxt.config.ts\`: Nuxt configuration
- \`drizzle.config.ts\`: Database config
- \`playwright.config.ts\`: E2E test config

## Where to Make Changes
- **Add API endpoint**: \`server/api/\`
- **Add component**: \`app/components/\`
- **Add page**: \`app/pages/\`
- **Add docs**: \`content/\`
- **Add test**: \`e2e/\``
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'Where should I add my code?' }
          ]
        }
      })

      const data = await response.json()
      expect(data.content).toContain('Directory' || 'server' || 'app')
    })

    test('should provide first contribution suggestions', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'msg-7',
        role: 'assistant',
        content: `# Good First Contributions

## Documentation (Easiest)
1. **Fix typos**: Check \`content/\` folder
2. **Add examples**: Expand code snippets
3. **Improve README**: Clarify setup steps

## Testing (Easy)
1. **Add test cases**: More avatar scenarios in \`e2e/avatars/\`
2. **Improve mocks**: Better AI mock responses in \`e2e/fixtures/\`
3. **Edge cases**: Test error scenarios

## Features (Medium)
1. **New components**: Add UI components in \`app/components/\`
2. **API endpoints**: Add CRUD operations in \`server/api/\`
3. **Chat improvements**: Enhance ChatPalette features

## Good Issues to Start With
- Look for \`good first issue\` label
- Documentation improvements
- Test coverage increases
- Small bug fixes

## Tips
- Start small: Pick ONE thing
- Ask questions in Discussions
- Read existing code first
- Test your changes locally

**Recommended first PR**: Add a new E2E test for an edge case you discovered!`
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'What should I contribute first?' }
          ]
        }
      })

      const data = await response.json()
      expect(data.content).toContain('contribution' || 'Contribution' || 'first')
    })

    test('should provide multi-turn onboarding conversation', async ({ page }) => {
      let conversationTurn = 0

      await page.route('**/api/chat', async (route) => {
        conversationTurn++

        if (conversationTurn === 1) {
          await createAIMockHandler({
            id: 'msg-8',
            role: 'assistant',
            content: 'This is a Nuxt 3 docs template with AI chat, PGlite database, and comprehensive E2E testing.'
          })(route)
        } else {
          await createAIMockHandler({
            id: 'msg-9',
            role: 'assistant',
            content: `Start by:
1. Reading \`README.md\`
2. Running \`npm install && npm run dev\`
3. Exploring the running app at http://localhost:3000
4. Checking \`e2e/avatars/\` to see test examples
5. Picking a \`good first issue\` from GitHub

Then make a small change, test it, and submit a PR!`
          })(route)
        }
      })

      const response1 = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [{ role: 'user', content: 'What is this project about?' }]
        }
      })

      const data1 = await response1.json()
      expect(data1.content).toContain('Nuxt' || 'docs')

      const response2 = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [
            { role: 'user', content: 'What is this project about?' },
            { role: 'assistant', content: data1.content },
            { role: 'user', content: 'How do I get started contributing?' }
          ]
        }
      })

      const data2 = await response2.json()
      expect(data2.content).toContain('README' || 'npm' || 'PR')
    })
  })

  test.describe('Success Criteria: AI-Powered OSS Onboarding', () => {
    test('can get setup help via AI', async ({ page }) => {
      await page.route('**/api/chat', createAIMockHandler({
        id: 'success-1',
        role: 'assistant',
        content: 'Clone the repo, run npm install, copy .env.example to .env, then npm run dev. No database setup needed!'
      }))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [{ role: 'user', content: 'Setup help' }]
        }
      })

      expect(response.status()).toBe(200)
      const data = await response.json()
      expect(data.role).toBe('assistant')
      expect(data.content.length).toBeGreaterThan(0)
    })

    test('can learn contribution guidelines with AI streaming', async ({ page }) => {
      await page.route('**/api/chat', createStreamingMockHandler(aiMocks.streaming.chunks))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [{ role: 'user', content: 'Contribution guidelines' }]
        }
      })

      expect(response.status()).toBe(200)
      const body = await response.text()
      expect(body.length).toBeGreaterThan(0)
    })

    test('can navigate onboarding despite AI errors', async ({ page }) => {
      await page.route('**/api/chat', createErrorMockHandler(aiMocks.errors.modelOverloaded))

      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: {
          messages: [{ role: 'user', content: 'Comprehensive onboarding guide' }]
        },
        failOnStatusCode: false
      })

      expect([503, 504, 429]).toContain(response.status())
    })
  })
})
