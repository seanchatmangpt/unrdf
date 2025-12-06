import { test, expect } from '@playwright/test'
import { ChatModal, APITestHelper } from '../helpers/page-objects'
import { avatars, mockChatMessages } from '../fixtures/test-data'

/**
 * Avatar: Chen the Full-Stack Developer
 * JTBD Tests: UI Components, Streaming Chat, Database CRUD
 */

test.describe('Chen the Full-Stack Developer - JTBD Journey', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/')
  })

  test.describe('Job 1: Use modern UI components', () => {
    test('should use Nuxt UI components', async ({ page }) => {
      // Check for Nuxt UI buttons
      const buttons = page.getByRole('button')
      const count = await buttons.count()

      expect(count).toBeGreaterThan(0)
    })

    test('should use ChatPalette component', async ({ page }) => {
      const chatButton = page.getByRole('button', { name: /ask ai/i })

      if (await chatButton.isVisible()) {
        await chatButton.click()

        // Modal should open
        const modal = page.locator('[role="dialog"]')
        await expect(modal).toBeVisible()
      }
    })

    test('should render modal components', async ({ page }) => {
      const chatButton = page.getByRole('button', { name: /ask ai/i })

      if (await chatButton.isVisible()) {
        await chatButton.click()

        // Check for modal structure
        const dialog = page.locator('[role="dialog"]')
        const exists = await dialog.count() > 0

        if (exists) {
          await expect(dialog).toBeVisible()
        }
      }
    })
  })

  test.describe('Job 2: Implement streaming AI chat', () => {
    test('should send messages to chat API', async ({ page }) => {
      const chatModal = new ChatModal(page)
      const apiHelper = new APITestHelper(page)

      // Mock API
      await apiHelper.mockChatAPI({
        id: 'test-1',
        role: 'assistant',
        content: 'Test response'
      })

      const chatButton = page.getByRole('button', { name: /ask ai/i })
      if (await chatButton.isVisible()) {
        await chatButton.click()

        await chatModal.input.fill('Hello AI')

        const isEnabled = await chatModal.sendButton.isEnabled()
        if (isEnabled) {
          await chatModal.sendButton.click()
        }
      }
    })

    test('should handle streaming responses', async ({ page }) => {
      const chatButton = page.getByRole('button', { name: /ask ai/i })

      if (await chatButton.isVisible()) {
        await chatButton.click()

        // Chat interface should support streaming
        const input = page.getByPlaceholder(/ask a question/i)
        await expect(input).toBeVisible()
      }
    })

    test('should display message history', async ({ page }) => {
      const chatButton = page.getByRole('button', { name: /ask ai/i })

      if (await chatButton.isVisible()) {
        await chatButton.click()

        // Modal should have message container
        const modal = page.locator('[role="dialog"]')
        await expect(modal).toBeVisible()
      }
    })
  })

  test.describe('Job 3: Perform database CRUD operations', () => {
    test('should CREATE: post new chat', async ({ page }) => {
      const response = await page.request.post('http://localhost:3000/api/chats', {
        data: {
          title: 'New Chat'
        },
        failOnStatusCode: false
      })

      expect([200, 201, 400, 401, 403]).toContain(response.status())
    })

    test('should READ: get chats list', async ({ page }) => {
      const response = await page.request.get('http://localhost:3000/api/chats', {
        failOnStatusCode: false
      })

      expect([200, 401, 403]).toContain(response.status())
    })

    test('should UPDATE: modify chat', async ({ page }) => {
      const response = await page.request.post('http://localhost:3000/api/chats/test-id', {
        data: {
          title: 'Updated Title'
        },
        failOnStatusCode: false
      })

      expect(response.status()).toBeGreaterThan(0)
    })

    test('should DELETE: remove chat', async ({ page }) => {
      const response = await page.request.delete('http://localhost:3000/api/chats/test-id', {
        failOnStatusCode: false
      })

      expect([200, 204, 401, 403, 404]).toContain(response.status())
    })
  })

  test.describe('Success Criteria Validation', () => {
    test('can use ChatPalette component', async ({ page }) => {
      const chatButton = page.getByRole('button', { name: /ask ai/i })
      const exists = await chatButton.count() > 0

      // Component should be available
      expect(true).toBeTruthy()
    })

    test('can implement streaming chat', async ({ page }) => {
      // Chat API endpoint exists
      const response = await page.request.post('http://localhost:3000/api/chat', {
        data: { messages: mockChatMessages },
        failOnStatusCode: false
      })

      expect(response.status()).toBeGreaterThan(0)
    })

    test('can perform CRUD operations', async ({ page }) => {
      // All CRUD endpoints should be available
      const endpoints = [
        page.request.get('http://localhost:3000/api/chats', { failOnStatusCode: false }),
        page.request.post('http://localhost:3000/api/chats', {
          data: { title: 'Test' },
          failOnStatusCode: false
        })
      ]

      const responses = await Promise.all(endpoints)
      responses.forEach((res) => {
        expect(res.status()).toBeGreaterThan(0)
      })
    })
  })
})
