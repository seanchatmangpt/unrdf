/**
 * @file Nuxt Test Setup
 * @description Global setup for Nuxt component and composable tests
 */

import { vi } from 'vitest'
import { config } from '@vue/test-utils'

// Mock global $fetch for composables
global.$fetch = vi.fn()

// Mock createSharedComposable for Nuxt composables
global.createSharedComposable = (fn) => fn

// Configure Vue Test Utils
config.global.stubs = {
  NuxtLink: true,
  NuxtLayout: true,
  ClientOnly: true
}

// Mock Nuxt auto-imports
global.defineNuxtComponent = (options) => options
global.useRuntimeConfig = vi.fn(() => ({
  public: {}
}))

// Setup complete
console.log('[Nuxt Test Setup] Initialized')
