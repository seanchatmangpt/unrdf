import type { Page, Locator } from '@playwright/test'

/**
 * Page Object Models for E2E Tests
 * Reusable page interactions following Page Object pattern
 */

export class HomePage {
  readonly page: Page
  readonly heading: Locator
  readonly navigation: Locator
  readonly chatButton: Locator

  constructor(page: Page) {
    this.page = page
    this.heading = page.locator('h1').first()
    this.navigation = page.locator('nav')
    this.chatButton = page.getByRole('button', { name: /ask ai/i })
  }

  async goto() {
    await this.page.goto('/')
  }

  async openChat() {
    await this.chatButton.click()
  }
}

export class DocsPage {
  readonly page: Page
  readonly sidebar: Locator
  readonly content: Locator
  readonly searchButton: Locator
  readonly toc: Locator

  constructor(page: Page) {
    this.page = page
    this.sidebar = page.locator('aside').first()
    this.content = page.locator('article, main').first()
    this.searchButton = page.getByRole('button', { name: /search/i })
    this.toc = page.locator('[class*="toc"]')
  }

  async navigateToSection(section: string) {
    await this.sidebar.getByText(section, { exact: false }).click()
  }

  async searchDocs(query: string) {
    await this.searchButton.click()
    await this.page.keyboard.type(query)
  }
}

export class ChatModal {
  readonly page: Page
  readonly modal: Locator
  readonly input: Locator
  readonly sendButton: Locator
  readonly messages: Locator
  readonly closeButton: Locator

  constructor(page: Page) {
    this.page = page
    this.modal = page.locator('[role="dialog"]')
    this.input = page.getByPlaceholder(/ask a question/i)
    this.sendButton = page.getByRole('button', { name: /send/i })
    this.messages = page.locator('[class*="message"]')
    this.closeButton = page.getByRole('button', { name: /close/i })
  }

  async sendMessage(message: string) {
    await this.input.fill(message)
    await this.sendButton.click()
  }

  async waitForResponse(timeout = 5000) {
    await this.page.waitForTimeout(timeout)
  }

  async getMessages() {
    return await this.messages.allTextContents()
  }

  async close() {
    await this.closeButton.click()
  }
}

export class APITestHelper {
  readonly page: Page

  constructor(page: Page) {
    this.page = page
  }

  async mockChatAPI(response: unknown) {
    await this.page.route('**/api/chat', async (route) => {
      await route.fulfill({
        status: 200,
        contentType: 'application/json',
        body: JSON.stringify(response)
      })
    })
  }

  async mockChatsAPI(chats: unknown[]) {
    await this.page.route('**/api/chats', async (route) => {
      await route.fulfill({
        status: 200,
        contentType: 'application/json',
        body: JSON.stringify(chats)
      })
    })
  }

  async interceptAPICall(url: string): Promise<unknown> {
    return new Promise((resolve) => {
      this.page.on('response', async (response) => {
        if (response.url().includes(url)) {
          resolve(await response.json())
        }
      })
    })
  }
}
