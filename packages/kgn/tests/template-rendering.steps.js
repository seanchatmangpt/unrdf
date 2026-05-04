import { Given, When, Then } from '@amiceli/vitest-cucumber'
import { expect } from 'vitest'
import { TemplateEngine } from '../src/engine/template-engine.js'
import fs from 'fs'
import path from 'path'

let templateEngine
let testData = {}
let renderResult = ''
let templateContent = ''
let parsedTemplate = {}

Given('the template engine is initialized', () => {
  templateEngine = new TemplateEngine()
})

Given('I have template files with frontmatter', () => {
  // Templates will be created in subsequent steps
  if (!fs.existsSync('test-templates')) {
    fs.mkdirSync('test-templates', { recursive: true })
  }
})

Given('I have a template {string} with content {string}', (templateName, content) => {
  templateContent = content
  const templatePath = path.join('test-templates', templateName)
  fs.writeFileSync(templatePath, content)
  global.currentTemplate = templatePath
})

Given('I have data with name {string}', (name) => {
  testData.name = name
})

Given('I have a template with YAML frontmatter', () => {
  const templateWithFrontmatter = `---
title: "Test Document"
author: "KGEN"
version: 1.0
variables:
  greeting: "Hello"
---
{{ greeting }} {{ name }}!
This is a {{ title }} by {{ author }}.`
  
  fs.writeFileSync('test-templates/frontmatter.njk', templateWithFrontmatter)
  global.frontmatterTemplate = 'test-templates/frontmatter.njk'
})

Given('I have a template using custom filters', () => {
  const filterTemplate = `{{ name | upper }} - {{ date | dateFormat('YYYY-MM-DD') }}`
  fs.writeFileSync('test-templates/filters.njk', filterTemplate)
  global.filterTemplate = 'test-templates/filters.njk'
  
  // Set up test data for filters
  testData.name = 'kgen'
  testData.date = new Date('2024-01-01')
})

Given('the filters are properly registered', () => {
  // Register custom filters with the template engine
  templateEngine.addFilter('upper', (str) => str.toUpperCase())
  templateEngine.addFilter('dateFormat', (date, format) => {
    // Simple date formatting for test
    return date.toISOString().split('T')[0]
  })
})

Given('I have templates for different file types', () => {
  // LaTeX template
  const latexTemplate = `\\documentclass{article}
\\title{{{ title }}}
\\author{{{ author }}}
\\begin{document}
\\maketitle
{{ content }}
\\end{document}`
  fs.writeFileSync('test-templates/document.tex.njk', latexTemplate)
  
  // Office template
  const officeTemplate = `<?xml version="1.0"?>
<document>
  <title>{{ title }}</title>
  <content>{{ content }}</content>
</document>`
  fs.writeFileSync('test-templates/document.docx.njk', officeTemplate)
  
  // NextJS template
  const nextjsTemplate = `import React from 'react'

export default function {{ componentName }}() {
  return (
    <div>
      <h1>{{ title }}</h1>
      <p>{{ description }}</p>
    </div>
  )
}`
  fs.writeFileSync('test-templates/component.tsx.njk', nextjsTemplate)
  
  // Set up test data
  testData = {
    title: 'Test Document',
    author: 'KGEN System',
    content: 'This is generated content.',
    componentName: 'TestComponent',
    description: 'A generated React component'
  }
})

When('I render the template', async () => {
  renderResult = await templateEngine.render(global.currentTemplate, testData)
})

When('I parse the template', async () => {
  parsedTemplate = await templateEngine.parseTemplate(global.frontmatterTemplate)
})

When('I render LaTeX templates', async () => {
  global.latexResult = await templateEngine.render('test-templates/document.tex.njk', testData)
})

When('I render Office templates', async () => {
  global.officeResult = await templateEngine.render('test-templates/document.docx.njk', testData)
})

When('I render NextJS templates', async () => {
  global.nextjsResult = await templateEngine.render('test-templates/component.tsx.njk', testData)
})

Then('the output should be {string}', (expectedOutput) => {
  expect(renderResult.trim()).toBe(expectedOutput)
})

Then('the rendering should be deterministic', async () => {
  // Render the same template multiple times
  const result1 = await templateEngine.render(global.currentTemplate, testData)
  const result2 = await templateEngine.render(global.currentTemplate, testData)
  const result3 = await templateEngine.render(global.currentTemplate, testData)
  
  expect(result1).toBe(result2)
  expect(result2).toBe(result3)
})

Then('the frontmatter should be extracted', () => {
  expect(parsedTemplate.frontmatter).toBeDefined()
  expect(parsedTemplate.frontmatter.title).toBe('Test Document')
  expect(parsedTemplate.frontmatter.author).toBe('KGEN')
})

Then('template variables should be available', () => {
  expect(parsedTemplate.frontmatter.variables).toBeDefined()
  expect(parsedTemplate.frontmatter.variables.greeting).toBe('Hello')
})

Then('custom filters should be applied', async () => {
  const result = await templateEngine.render(global.filterTemplate, testData)
  expect(result).toContain('KGEN') // upper filter applied
  expect(result).toContain('2024-01-01') // dateFormat filter applied
})

Then('the output should be formatted correctly', async () => {
  const result = await templateEngine.render(global.filterTemplate, testData)
  expect(result).toMatch(/^KGEN - \d{4}-\d{2}-\d{2}$/)
})

Then('PDF-ready LaTeX should be generated', () => {
  expect(global.latexResult).toContain('\\documentclass{article}')
  expect(global.latexResult).toContain('\\title{Test Document}')
  expect(global.latexResult).toContain('\\author{KGEN System}')
})

Then('DOCX documents should be created', () => {
  expect(global.officeResult).toContain('<?xml version="1.0"?>')
  expect(global.officeResult).toContain('<title>Test Document</title>')
})

Then('React components should be generated', () => {
  expect(global.nextjsResult).toContain('import React from \'react\'')
  expect(global.nextjsResult).toContain('export default function TestComponent()')
  expect(global.nextjsResult).toContain('<h1>Test Document</h1>')
})