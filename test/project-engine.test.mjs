/**
 * @fileoverview Minimal tests for project engine modules
 * Tests core functionality: config, stack detection, materialization
 * @vitest-environment node
 */

import { describe, it, expect } from 'vitest'
import { Store, DataFactory } from 'n3'
import {
  materializeArtifacts,
  getProjectEngineConfig,
} from '../src/project-engine/index.mjs'

const { namedNode, literal } = DataFactory

describe('project-engine', () => {
  describe('Config', () => {
    it('loads default configuration', () => {
      const config = getProjectEngineConfig()
      expect(config.fs.ignorePatterns).toContain('node_modules')
      expect(config.project.conventions.sourcePaths).toContain('src')
      expect(config.diff.structureLens).toBe('project-structure')
    })

    it('merges configuration overrides', () => {
      const config = getProjectEngineConfig({
        fs: { ignorePatterns: ['custom'] },
      })
      expect(config.fs.ignorePatterns).toContain('custom')
      expect(config.fs.ignorePatterns).toContain('node_modules')
    })

    it('validates configuration schema', () => {
      expect(() => {
        getProjectEngineConfig({
          golden: { profile: 'invalid-profile' },
        })
      }).toThrow()
    })
  })


  describe('Materialization', () => {
    it('creates materialization plan from ontology', () => {
      const store = new Store()
      const projectIri = namedNode('http://example.org/unrdf/project#project')
      store.addQuad(
        projectIri,
        namedNode('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'),
        namedNode('http://example.org/unrdf/project#Project')
      )

      const { plan, receipt } = materializeArtifacts({
        ontologyStore: store,
      })

      expect(plan).toHaveProperty('writes')
      expect(plan).toHaveProperty('deletes')
      expect(plan).toHaveProperty('moves')
      expect(receipt).toHaveProperty('beforeHash')
      expect(receipt).toHaveProperty('planHash')
    })

    it('includes metadata in receipt', () => {
      const store = new Store()
      const { receipt } = materializeArtifacts({
        ontologyStore: store,
      })

      expect(receipt).toHaveProperty('timestamp')
      expect(receipt).toHaveProperty('changes')
      expect(receipt.dryRun).toBe(false)
    })
  })
})
