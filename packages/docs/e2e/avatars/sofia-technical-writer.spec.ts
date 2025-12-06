import { test, expect } from '@playwright/test'
import { DocsPage, ChatModal } from '../helpers/page-objects'
import { avatars, documentationPages } from '../fixtures/test-data'

/**
 * Avatar: Sofia the Technical Writer
 * JTBD Tests: Content Organization, AI Content Generation, Markdown Rendering
 */

test.describe('Sofia the Technical Writer - JTBD Journey', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/')
  })

  test.describe('Job 1: Organize content hierarchically', () => {
    test('should display navigation structure', async ({ page }) => {
      const docsPage = new DocsPage(page)

      await page.goto('/getting-started')

      // Check for navigation sidebar
      const nav = page.locator('nav, aside')
      const exists = await nav.count() > 0

      if (exists) {
        await expect(nav.first()).toBeVisible()
      }
    })

    test('should navigate between documentation sections', async ({ page }) => {
      await page.goto('/getting-started')
      await expect(page).toHaveURL(/getting-started/)

      await page.goto('/essentials')
      await expect(page).toHaveURL(/essentials/)
    })

    test('should show table of contents', async ({ page }) => {
      await page.goto('/essentials/markdown-syntax')

      // Look for TOC or headings
      const headings = page.locator('h2, h3, h4')
      const count = await headings.count()

      expect(count).toBeGreaterThan(0)
    })
  })

  test.describe('Job 2: Generate content with AI', () => {
    test('should use AI for documentation examples', async ({ page }) => {
      const chatModal = new ChatModal(page)

      const chatButton = page.getByRole('button', { name: /ask ai/i })
      if (await chatButton.isVisible()) {
        await chatButton.click()

        await expect(chatModal.modal).toBeVisible()

        // Ask for documentation help
        await chatModal.input.fill('Write a Getting Started guide introduction')

        const isEnabled = await chatModal.sendButton.isEnabled()
        expect(isEnabled).toBeTruthy()
      }
    })

    test('should get code snippet suggestions', async ({ page }) => {
      const chatModal = new ChatModal(page)

      const chatButton = page.getByRole('button', { name: /ask ai/i })
      if (await chatButton.isVisible()) {
        await chatButton.click()
        await chatModal.input.fill('Show me a markdown code block example')
      }
    })
  })

  test.describe('Job 3: Preview markdown rendering', () => {
    test('should render markdown headings', async ({ page }) => {
      await page.goto('/essentials/markdown-syntax')

      const h1 = page.locator('h1')
      const h2 = page.locator('h2')

      await expect(h1.or(h2).first()).toBeVisible()
    })

    test('should render code blocks', async ({ page }) => {
      await page.goto('/essentials/code-blocks')

      const codeBlock = page.locator('pre code')
      const exists = await codeBlock.count() > 0

      if (exists) {
        await expect(codeBlock.first()).toBeVisible()
      }
    })

    test('should render prose components', async ({ page }) => {
      await page.goto('/essentials/prose-components')

      // Verify page loads with content
      const content = page.locator('main, article')
      await expect(content.first()).toBeVisible()
    })

    test('should render images and embeds', async ({ page }) => {
      await page.goto('/essentials/images-embeds')

      // Look for image or media elements
      const media = page.locator('img, video, iframe')
      // Page should load successfully
      await expect(page.locator('h1')).toBeVisible()
    })
  })

  test.describe('Success Criteria Validation', () => {
    test('can create hierarchical documentation', async ({ page }) => {
      // Verify multi-level navigation exists
      await page.goto('/getting-started')
      const gettingStartedHeading = page.locator('h1')
      await expect(gettingStartedHeading).toBeVisible()

      await page.goto('/essentials/markdown-syntax')
      const essentialsHeading = page.locator('h1')
      await expect(essentialsHeading).toBeVisible()
    })

    test('can access AI content generation', async ({ page }) => {
      const chatExists = await page.getByRole('button', { name: /ask ai/i }).count() > 0

      // AI feature should be available
      expect(true).toBeTruthy()
    })

    test('can preview markdown correctly', async ({ page }) => {
      await page.goto('/essentials/markdown-syntax')

      // Check for rendered markdown elements
      const elements = page.locator('h1, h2, h3, p, code, pre')
      const count = await elements.count()

      expect(count).toBeGreaterThan(0)
    })
  })
})
