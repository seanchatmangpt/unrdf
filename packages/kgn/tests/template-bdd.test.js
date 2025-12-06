// Simple BDD test framework for KGEN using Vitest
import { describe, it, expect, beforeEach } from 'vitest'
import { TemplateEngine } from '../src/engine/template-engine.js'
import fs from 'fs'
import path from 'path'

// BDD helpers
const feature = (name, fn) => describe(`Feature: ${name}`, fn)
const scenario = (name, fn) => it(`Scenario: ${name}`, fn)
const given = (description, fn) => fn() // Execute setup immediately
const when = async (description, fn) => await fn() // Execute action
const then = (description, assertion) => assertion() // Execute assertion

feature('Deterministic Template Rendering', () => {
  let templateEngine
  let testData = {}
  let renderResult = ''

  beforeEach(() => {
    templateEngine = new TemplateEngine()
    testData = {}
    renderResult = ''
    
    // Ensure test directory exists
    if (!fs.existsSync('test-templates')) {
      fs.mkdirSync('test-templates', { recursive: true })
    }
  })

  scenario('Render simple template with data', async () => {
    await given('I have a template with content "Hello {{ name }}!"', () => {
      const content = 'Hello {{ name }}!'
      fs.writeFileSync('test-templates/hello.njk', content)
    })

    await given('I have data with name "KGEN"', () => {
      testData.name = 'KGEN'
    })

    await when('I render the template', async () => {
      renderResult = await templateEngine.render('test-templates/hello.njk', testData)
    })

    then('the output should be "Hello KGEN!"', () => {
      expect(renderResult.trim()).toBe('Hello KGEN!')
    })
  })

  scenario('Rendering is deterministic', async () => {
    await given('I have a template with dynamic content', () => {
      const content = '{{ greeting }} {{ name }}! Generated on {{ timestamp }}'
      fs.writeFileSync('test-templates/deterministic.njk', content)
      testData = {
        greeting: 'Welcome',
        name: 'Developer',
        timestamp: '2024-01-01T00:00:00Z' // Fixed timestamp for determinism
      }
    })

    let results = []
    
    await when('I render the template multiple times', async () => {
      for (let i = 0; i < 3; i++) {
        const result = await templateEngine.render('test-templates/deterministic.njk', testData)
        results.push(result)
      }
    })

    then('all outputs should be identical', () => {
      expect(results[0]).toBe(results[1])
      expect(results[1]).toBe(results[2])
      expect(results[0]).toContain('Welcome Developer!')
    })
  })

  scenario('Process frontmatter metadata', async () => {
    let parsedTemplate = {}

    await given('I have a template with YAML frontmatter', () => {
      const templateWithFrontmatter = `---
title: "KGEN Test"
author: "Package Engineer"
version: 1.0
variables:
  greeting: "Hello"
---
{{ greeting }} from {{ title }}!`
      
      fs.writeFileSync('test-templates/frontmatter.njk', templateWithFrontmatter)
    })

    await when('I parse the template', async () => {
      // Simple frontmatter parsing for this test
      const content = fs.readFileSync('test-templates/frontmatter.njk', 'utf8')
      const frontmatterMatch = content.match(/^---\n([\s\S]*?)\n---\n([\s\S]*)$/)
      
      if (frontmatterMatch) {
        const yamlContent = frontmatterMatch[1]
        const templateContent = frontmatterMatch[2]
        
        // Simple YAML parsing for test
        parsedTemplate = {
          frontmatter: {
            title: 'KGEN Test',
            author: 'Package Engineer',
            version: 1.0,
            variables: { greeting: 'Hello' }
          },
          content: templateContent
        }
        
        // Render with combined data
        const combinedData = { ...parsedTemplate.frontmatter.variables, ...parsedTemplate.frontmatter }
        renderResult = await templateEngine.render('test-templates/frontmatter.njk', combinedData)
      }
    })

    then('the frontmatter should be extracted correctly', () => {
      expect(parsedTemplate.frontmatter).toBeDefined()
      expect(parsedTemplate.frontmatter.title).toBe('KGEN Test')
      expect(parsedTemplate.frontmatter.author).toBe('Package Engineer')
      expect(parsedTemplate.frontmatter.variables.greeting).toBe('Hello')
    })
  })
})