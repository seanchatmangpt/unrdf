/**
 * Test setup for kgen-templates package
 * Configures template engine test environment
 */

import { beforeAll, afterAll, afterEach } from 'vitest'
import { resolve } from 'path'
import fs from 'fs-extra'

// Template engine test globals
globalThis.KGEN_TEMPLATES_TEST_DIR = resolve(process.cwd(), 'tests/temp')
globalThis.KGEN_TEMPLATES_FIXTURES = resolve(process.cwd(), 'tests/fixtures')
globalThis.KGEN_TEMPLATES_FILES = resolve(process.cwd(), 'tests/templates')

// Set up templates test environment
beforeAll(async () => {
  await fs.ensureDir(globalThis.KGEN_TEMPLATES_TEST_DIR)
  await fs.ensureDir(globalThis.KGEN_TEMPLATES_FIXTURES)
  await fs.ensureDir(globalThis.KGEN_TEMPLATES_FILES)

  // Templates-specific environment variables
  process.env.KGEN_TEMPLATES_TEST_MODE = 'true'
  process.env.KGEN_TEMPLATES_LOG_LEVEL = process.env.KGEN_TEMPLATES_LOG_LEVEL || 'silent'
  process.env.KGEN_TEMPLATES_CACHE_DIR = globalThis.KGEN_TEMPLATES_TEST_DIR
})

// Clean up after each test
afterEach(async () => {
  try {
    if (await fs.pathExists(globalThis.KGEN_TEMPLATES_TEST_DIR)) {
      await fs.emptyDir(globalThis.KGEN_TEMPLATES_TEST_DIR)
    }
  } catch (error) {
    console.warn('Templates test cleanup warning:', error.message)
  }
})

// Final cleanup
afterAll(async () => {
  try {
    if (await fs.pathExists(globalThis.KGEN_TEMPLATES_TEST_DIR)) {
      await fs.remove(globalThis.KGEN_TEMPLATES_TEST_DIR)
    }
  } catch (error) {
    console.warn('Templates test final cleanup warning:', error.message)
  }
})

// Export templates test utilities
export const templatesTestUtils = {
  getTempDir: () => globalThis.KGEN_TEMPLATES_TEST_DIR,
  getFixturesDir: () => globalThis.KGEN_TEMPLATES_FIXTURES,
  getTemplatesDir: () => globalThis.KGEN_TEMPLATES_FILES,

  // Create a test template
  async createTestTemplate(name, templateContent) {
    const templatePath = resolve(globalThis.KGEN_TEMPLATES_TEST_DIR, `${name}.njk`)
    await fs.writeFile(templatePath, templateContent, 'utf8')
    return templatePath
  },

  // Create test template context data
  async createTestContext(name, contextData = {}) {
    const contextPath = resolve(globalThis.KGEN_TEMPLATES_TEST_DIR, `${name}-context.json`)
    await fs.writeFile(contextPath, JSON.stringify(contextData, null, 2), 'utf8')
    return contextPath
  },

  // Create a test meta-template
  async createTestMetaTemplate(name, metaContent) {
    const metaPath = resolve(globalThis.KGEN_TEMPLATES_TEST_DIR, `_${name}.njk`)
    await fs.writeFile(metaPath, metaContent, 'utf8')
    return metaPath
  }
}
