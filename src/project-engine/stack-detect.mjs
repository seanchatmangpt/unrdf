/**
 * @file Stack detection - identify React/Next/Nest/Express/Jest/Vitest
 * @module project-engine/stack-detect
 */

import { DataFactory } from 'n3'
import { z } from 'zod'

const { namedNode } = DataFactory

const StackDetectOptionsSchema = z.object({
  fsStore: z.object({}).passthrough(),
  projectIri: z.string().default('http://example.org/unrdf/project#project'),
})

/**
 * Detect tech stack from filesystem
 *
 * @param {Object} options
 * @param {Store} options.fsStore - FS store from scanFileSystemToStore
 * @param {string} [options.projectIri] - IRI of project entity
 * @returns {Object} Stack information
 */
export function detectStackFromFs(options) {
  const validated = StackDetectOptionsSchema.parse(options)
  const { fsStore } = validated

  const stack = {
    uiFramework: null,
    webFramework: null,
    apiFramework: null,
    testFramework: null,
    packageManager: null,
  }

  const fileQuads = fsStore.getQuads(
    null,
    namedNode('http://example.org/unrdf/filesystem#relativePath'),
    null
  )

  const filePaths = new Set()
  for (const quad of fileQuads) {
    filePaths.add(quad.object.value)
  }

  // Detect UI framework
  if (filePaths.has('src/app') || filePaths.has('app') || filePaths.has('src/pages') || filePaths.has('pages')) {
    stack.uiFramework = 'react'
  } else if (filePaths.has('src/views') || filePaths.has('src/components')) {
    stack.uiFramework = 'react'
  }

  // Detect web framework
  if (filePaths.has('next.config.js') || filePaths.has('next.config.mjs') || filePaths.has('src/app')) {
    stack.webFramework = 'next'
    if (filePaths.has('src/app')) {
      stack.webFramework = 'next-app-router'
    } else if (filePaths.has('src/pages') || filePaths.has('pages')) {
      stack.webFramework = 'next-pages'
    }
  } else if (filePaths.has('nest-cli.json')) {
    stack.webFramework = 'nest'
    stack.apiFramework = 'nest'
  } else if (filePaths.has('src/server.js') || filePaths.has('src/app.js')) {
    stack.webFramework = 'express'
    stack.apiFramework = 'express'
  }

  // Detect test framework
  if (filePaths.has('vitest.config.js') || filePaths.has('vitest.config.mjs')) {
    stack.testFramework = 'vitest'
  } else if (filePaths.has('jest.config.js') || filePaths.has('jest.config.json')) {
    stack.testFramework = 'jest'
  } else if (filePaths.has('.mocharc.json') || filePaths.has('.mocharc.js')) {
    stack.testFramework = 'mocha'
  }

  // Detect package manager
  if (filePaths.has('pnpm-lock.yaml')) {
    stack.packageManager = 'pnpm'
  } else if (filePaths.has('yarn.lock')) {
    stack.packageManager = 'yarn'
  } else if (filePaths.has('package-lock.json')) {
    stack.packageManager = 'npm'
  } else if (filePaths.has('bun.lockb')) {
    stack.packageManager = 'bun'
  }

  return stack
}
