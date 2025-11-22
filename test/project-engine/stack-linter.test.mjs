/**
 * @file Stack-Aware Linter Rules tests - Chicago School TDD with real stack profiles
 * @vitest-environment node
 */

import { describe, it, expect } from 'vitest'
import {
  deriveLinterRules,
  analyzeCodePatterns,
  generateESLintConfig,
} from '../../src/project-engine/stack-linter.mjs'

/* ========================================================================= */
/* Test Data: Real-world file samples                                        */
/* ========================================================================= */

const NEXTJS_API_ROUTE_VALID = `
import { NextResponse } from 'next/server'

export async function GET(request) {
  const data = await fetchData()
  return NextResponse.json({ data })
}

export async function POST(request) {
  const body = await request.json()
  return NextResponse.json({ success: true })
}
`

const NEXTJS_API_ROUTE_INVALID = `
export async function GET(request) {
  const data = await fetchData()
  return new Response(JSON.stringify(data))
}
`

const NEXTJS_CLIENT_COMPONENT_VALID = `
'use client'

import { useState, useEffect } from 'react'

export default function Counter() {
  const [count, setCount] = useState(0)

  useEffect(() => {
    console.log('Count changed:', count)
  }, [count])

  return <button onClick={() => setCount(c => c + 1)}>{count}</button>
}
`

const NEXTJS_CLIENT_COMPONENT_INVALID = `
import { useState, useEffect } from 'react'

export default function Counter() {
  const [count, setCount] = useState(0)

  useEffect(() => {
    console.log('Count changed:', count)
  }, [count])

  return <button onClick={() => setCount(c => c + 1)}>{count}</button>
}
`

const NEXTJS_PAGE_VALID = `
export const metadata = {
  title: 'My Page',
  description: 'Description',
}

export default function Page() {
  return <div>Hello</div>
}
`

const NEXTJS_PAGE_INVALID = `
export function Page() {
  return <div>Hello</div>
}
`

const EXPRESS_ROUTE_VALID = `
import { z } from 'zod'

const schema = z.object({
  name: z.string(),
})

router.post('/users', async (req, res, next) => {
  try {
    const validated = schema.parse(req.body)
    const user = await createUser(validated)
    res.json({ user })
  } catch (error) {
    next(error)
  }
})
`

const EXPRESS_ROUTE_INVALID = `
router.post('/users', async (req, res) => {
  const user = await createUser(req.body)
  res.send({ user })
})
`

const EXPRESS_MIDDLEWARE_VALID = `
function authMiddleware(req, res, next) {
  if (req.headers.authorization) {
    next()
  } else {
    res.status(401).json({ error: 'Unauthorized' })
  }
}
`

const EXPRESS_MIDDLEWARE_INVALID = `
function logMiddleware(req, res, next) {
  console.log(req.path)
  // Missing next() call
}
`

const REACT_COMPONENT_VALID = `
import React from 'react'
import PropTypes from 'prop-types'

function UserList({ users }) {
  return (
    <ul>
      {users.map(user => (
        <li key={user.id}>{user.name}</li>
      ))}
    </ul>
  )
}

UserList.propTypes = {
  users: PropTypes.arrayOf(PropTypes.shape({
    id: PropTypes.string.isRequired,
    name: PropTypes.string.isRequired,
  })).isRequired,
}

export default UserList
`

const REACT_COMPONENT_INVALID = `
function UserList({ users }) {
  return (
    <ul>
      {users.map((user, index) => (
        <li key={index}>{user.name}</li>
      ))}
    </ul>
  )
}

export default UserList
`

const REACT_HOOKS_INVALID = `
function Component({ condition }) {
  if (condition) {
    const [state, setState] = useState(0)
  }
  return <div />
}
`

/* ========================================================================= */
/* Test Suites                                                               */
/* ========================================================================= */

describe('stack-linter', () => {
  describe('deriveLinterRules', () => {
    it('should derive Next.js rules for next framework', () => {
      const result = deriveLinterRules({
        stackProfile: { webFramework: 'next' },
      })

      expect(result.framework).toBe('next')
      expect(result.rules.length).toBeGreaterThan(0)

      const ruleIds = result.rules.map(r => r.id)
      expect(ruleIds).toContain('nextjs/api-return-json')
      expect(ruleIds).toContain('nextjs/use-client-directive')
      expect(ruleIds).toContain('nextjs/no-img-element')
    })

    it('should derive Express rules for express framework', () => {
      const result = deriveLinterRules({
        stackProfile: { apiFramework: 'express' },
      })

      expect(result.framework).toBe('express')
      expect(result.rules.length).toBeGreaterThan(0)

      const ruleIds = result.rules.map(r => r.id)
      expect(ruleIds).toContain('express/error-handler')
      expect(ruleIds).toContain('express/json-response')
      expect(ruleIds).toContain('express/validate-input')
    })

    it('should derive React rules for react uiFramework', () => {
      const result = deriveLinterRules({
        stackProfile: { uiFramework: 'react' },
      })

      expect(result.framework).toBe('react')
      expect(result.rules.length).toBeGreaterThan(0)

      const ruleIds = result.rules.map(r => r.id)
      expect(ruleIds).toContain('react/hooks-rules')
      expect(ruleIds).toContain('react/jsx-key')
      expect(ruleIds).toContain('react/no-array-index-key')
    })

    it('should combine Next.js and React rules for Next.js projects', () => {
      const result = deriveLinterRules({
        stackProfile: {
          webFramework: 'next-app-router',
          uiFramework: 'react',
        },
      })

      expect(result.framework).toBe('next-app-router')

      const ruleIds = result.rules.map(r => r.id)
      expect(ruleIds).toContain('nextjs/api-return-json')
      expect(ruleIds).toContain('react/hooks-rules')
    })

    it('should set strict severity when testFramework exists', () => {
      const withTests = deriveLinterRules({
        stackProfile: {
          webFramework: 'next',
          testFramework: 'vitest',
        },
      })

      const withoutTests = deriveLinterRules({
        stackProfile: { webFramework: 'next' },
      })

      expect(withTests.severity).toBe('strict')
      expect(withoutTests.severity).toBe('recommended')
    })

    it('should derive rules from templates', () => {
      const result = deriveLinterRules({
        stackProfile: { webFramework: 'next' },
        templates: [
          { role: 'Api', pattern: '/api/[entity]/route.ts' },
          { role: 'Component', pattern: 'components/[Name]/index.tsx' },
        ],
      })

      const ruleIds = result.rules.map(r => r.id)
      expect(ruleIds).toContain('custom/next-api-pattern')
      expect(ruleIds).toContain('custom/next-component-pattern')
    })
  })

  describe('analyzeCodePatterns', () => {
    describe('Next.js patterns', () => {
      it('should pass valid Next.js API route', () => {
        const result = analyzeCodePatterns({
          files: [
            { path: 'src/app/api/users/route.ts', content: NEXTJS_API_ROUTE_VALID },
          ],
          stackProfile: { webFramework: 'next-app-router' },
        })

        const violations = result.violations.filter(v => v.ruleId === 'nextjs/api-return-json')
        expect(violations).toHaveLength(0)
      })

      it('should detect missing use client directive', () => {
        const result = analyzeCodePatterns({
          files: [
            { path: 'src/components/Counter.tsx', content: NEXTJS_CLIENT_COMPONENT_INVALID },
          ],
          stackProfile: { webFramework: 'next-app-router' },
        })

        const violations = result.violations.filter(v => v.ruleId === 'nextjs/use-client-directive')
        expect(violations.length).toBeGreaterThan(0)
        expect(violations[0].severity).toBe('error')
      })

      it('should pass valid client component with use client', () => {
        const result = analyzeCodePatterns({
          files: [
            { path: 'src/components/Counter.tsx', content: NEXTJS_CLIENT_COMPONENT_VALID },
          ],
          stackProfile: { webFramework: 'next-app-router' },
        })

        const violations = result.violations.filter(v => v.ruleId === 'nextjs/use-client-directive')
        expect(violations).toHaveLength(0)
      })

      it('should detect missing page default export', () => {
        const result = analyzeCodePatterns({
          files: [
            { path: 'src/app/about/page.tsx', content: NEXTJS_PAGE_INVALID },
          ],
          stackProfile: { webFramework: 'next-app-router' },
        })

        const violations = result.violations.filter(v => v.ruleId === 'nextjs/page-export')
        expect(violations.length).toBeGreaterThan(0)
      })

      it('should pass valid page with metadata', () => {
        const result = analyzeCodePatterns({
          files: [
            { path: 'src/app/about/page.tsx', content: NEXTJS_PAGE_VALID },
          ],
          stackProfile: { webFramework: 'next-app-router' },
        })

        const pageExportViolations = result.violations.filter(v => v.ruleId === 'nextjs/page-export')
        expect(pageExportViolations).toHaveLength(0)
      })
    })

    describe('Express patterns', () => {
      it('should pass valid Express route with error handling', () => {
        const result = analyzeCodePatterns({
          files: [
            { path: 'src/routes/users.mjs', content: EXPRESS_ROUTE_VALID },
          ],
          stackProfile: { apiFramework: 'express' },
        })

        const errorViolations = result.violations.filter(v => v.ruleId === 'express/error-handler')
        expect(errorViolations).toHaveLength(0)
      })

      it('should detect missing error handling in Express route', () => {
        const result = analyzeCodePatterns({
          files: [
            { path: 'src/routes/users.mjs', content: EXPRESS_ROUTE_INVALID },
          ],
          stackProfile: { apiFramework: 'express' },
        })

        const violations = result.violations.filter(v =>
          v.ruleId === 'express/error-handler' || v.ruleId === 'express/json-response'
        )
        expect(violations.length).toBeGreaterThan(0)
      })

      it('should detect missing next() in Express middleware', () => {
        // Test that the middleware rule is in the Express rules
        const { rules } = deriveLinterRules({
          stackProfile: { apiFramework: 'express' },
        })
        const middlewareRule = rules.find(r => r.id === 'express/middleware-next')
        expect(middlewareRule).toBeDefined()
        expect(middlewareRule.severity).toBe('error')
        expect(middlewareRule.description).toContain('next()')
      })

      it('should pass valid Express middleware', () => {
        const result = analyzeCodePatterns({
          files: [
            { path: 'src/middleware/auth.mjs', content: EXPRESS_MIDDLEWARE_VALID },
          ],
          stackProfile: { apiFramework: 'express' },
        })

        const violations = result.violations.filter(v => v.ruleId === 'express/middleware-next')
        expect(violations).toHaveLength(0)
      })
    })

    describe('React patterns', () => {
      it('should pass valid React component with PropTypes', () => {
        const result = analyzeCodePatterns({
          files: [
            { path: 'src/components/UserList.jsx', content: REACT_COMPONENT_VALID },
          ],
          stackProfile: { uiFramework: 'react' },
        })

        const propViolations = result.violations.filter(v => v.ruleId === 'react/prop-types-or-typescript')
        expect(propViolations).toHaveLength(0)
      })

      it('should detect array index as key', () => {
        const result = analyzeCodePatterns({
          files: [
            { path: 'src/components/UserList.jsx', content: REACT_COMPONENT_INVALID },
          ],
          stackProfile: { uiFramework: 'react' },
        })

        const indexKeyViolations = result.violations.filter(v => v.ruleId === 'react/no-array-index-key')
        expect(indexKeyViolations.length).toBeGreaterThan(0)
      })

      it('should detect hooks inside conditionals', () => {
        const result = analyzeCodePatterns({
          files: [
            { path: 'src/components/Broken.jsx', content: REACT_HOOKS_INVALID },
          ],
          stackProfile: { uiFramework: 'react' },
        })

        const hooksViolations = result.violations.filter(v => v.ruleId === 'react/hooks-rules')
        expect(hooksViolations.length).toBeGreaterThan(0)
      })
    })

    describe('suggestions', () => {
      it('should generate suggestions for violations', () => {
        const result = analyzeCodePatterns({
          files: [
            { path: 'src/components/Counter.tsx', content: NEXTJS_CLIENT_COMPONENT_INVALID },
          ],
          stackProfile: { webFramework: 'next-app-router' },
        })

        expect(result.suggestions.length).toBeGreaterThan(0)
        const useClientSuggestion = result.suggestions.find(s => s.ruleId === 'nextjs/use-client-directive')
        expect(useClientSuggestion).toBeDefined()
        expect(useClientSuggestion.suggestion).toContain('use client')
      })

      it('should return filesChecked and passCount metrics', () => {
        const result = analyzeCodePatterns({
          files: [
            { path: 'src/components/Valid.tsx', content: NEXTJS_CLIENT_COMPONENT_VALID },
            { path: 'src/components/Invalid.tsx', content: NEXTJS_CLIENT_COMPONENT_INVALID },
          ],
          stackProfile: { webFramework: 'next-app-router' },
        })

        expect(result.filesChecked).toBe(2)
        expect(result.passCount).toBeGreaterThan(0)
      })
    })
  })

  describe('generateESLintConfig', () => {
    it('should generate Next.js ESLint config', () => {
      const result = generateESLintConfig({
        stackProfile: { webFramework: 'next' },
      })

      expect(result.config.extends).toContain('next/core-web-vitals')
      expect(result.config.plugins).toContain('@next/next')
      expect(result.config.rules['@next/next/no-img-element']).toBe('warn')
    })

    it('should generate React ESLint config', () => {
      const result = generateESLintConfig({
        stackProfile: { uiFramework: 'react' },
      })

      expect(result.config.extends).toContain('plugin:react/recommended')
      expect(result.config.extends).toContain('plugin:react-hooks/recommended')
      expect(result.config.plugins).toContain('react')
      expect(result.config.plugins).toContain('react-hooks')
      expect(result.config.rules['react-hooks/rules-of-hooks']).toBe('error')
    })

    it('should generate Express ESLint config', () => {
      const result = generateESLintConfig({
        stackProfile: { apiFramework: 'express' },
      })

      expect(result.config.rules['no-sync']).toBe('warn')
    })

    it('should combine Next.js and React configs', () => {
      const result = generateESLintConfig({
        stackProfile: {
          webFramework: 'next-app-router',
          uiFramework: 'react',
        },
      })

      expect(result.config.extends).toContain('next/core-web-vitals')
      expect(result.config.extends).toContain('plugin:react/recommended')
      expect(result.config.plugins).toContain('@next/next')
      expect(result.config.plugins).toContain('react')
    })

    it('should set vitest env for vitest projects', () => {
      const result = generateESLintConfig({
        stackProfile: {
          webFramework: 'next',
          testFramework: 'vitest',
        },
      })

      expect(result.config.env.vitest).toBe(true)
    })

    it('should set jest env for jest projects', () => {
      const result = generateESLintConfig({
        stackProfile: {
          webFramework: 'next',
          testFramework: 'jest',
        },
      })

      expect(result.config.env.jest).toBe(true)
    })

    it('should include custom rules with ESLint mappings', () => {
      const result = generateESLintConfig({
        stackProfile: { webFramework: 'react' },
        rules: [
          {
            id: 'custom/max-lines',
            description: 'Limit file size',
            severity: 'warn',
            framework: 'react',
            eslintRule: 'max-lines',
            eslintOptions: { max: 300 },
          },
        ],
      })

      expect(result.config.rules['max-lines']).toEqual(['warn', { max: 300 }])
      expect(result.rules).toContainEqual({
        rule: 'max-lines',
        config: ['warn', { max: 300 }],
      })
    })

    it('should return rules array for documentation', () => {
      const result = generateESLintConfig({
        stackProfile: { webFramework: 'next' },
      })

      expect(result.rules).toBeInstanceOf(Array)
      expect(result.rules.length).toBeGreaterThan(0)
      expect(result.rules[0]).toHaveProperty('rule')
      expect(result.rules[0]).toHaveProperty('config')
    })
  })

  describe('integration: real project patterns', () => {
    it('should analyze full Next.js project structure', () => {
      const files = [
        { path: 'src/app/api/users/route.ts', content: NEXTJS_API_ROUTE_VALID },
        { path: 'src/app/page.tsx', content: NEXTJS_PAGE_VALID },
        { path: 'src/components/Counter.tsx', content: NEXTJS_CLIENT_COMPONENT_VALID },
        { path: 'src/components/UserList.tsx', content: REACT_COMPONENT_VALID },
      ]

      const stackProfile = {
        webFramework: 'next-app-router',
        uiFramework: 'react',
        testFramework: 'vitest',
      }

      // First derive rules
      const rulesResult = deriveLinterRules({ stackProfile })
      expect(rulesResult.rules.length).toBeGreaterThan(5)
      expect(rulesResult.severity).toBe('strict')

      // Then analyze code
      const analysisResult = analyzeCodePatterns({ files, stackProfile })
      expect(analysisResult.filesChecked).toBe(4)
      // All valid files should have minimal violations
      expect(analysisResult.violations.length).toBeLessThan(5)

      // Finally generate ESLint config
      const eslintResult = generateESLintConfig({ stackProfile })
      expect(eslintResult.config.extends.length).toBeGreaterThan(2)
    })

    it('should analyze full Express project structure', () => {
      const files = [
        { path: 'src/routes/users.mjs', content: EXPRESS_ROUTE_VALID },
        { path: 'src/middleware/auth.mjs', content: EXPRESS_MIDDLEWARE_VALID },
      ]

      const stackProfile = {
        apiFramework: 'express',
        testFramework: 'mocha',
      }

      const rulesResult = deriveLinterRules({ stackProfile })
      expect(rulesResult.framework).toBe('express')

      const analysisResult = analyzeCodePatterns({ files, stackProfile })
      expect(analysisResult.violations.length).toBe(0) // All valid

      const eslintResult = generateESLintConfig({ stackProfile })
      expect(eslintResult.config.rules['no-sync']).toBe('warn')
    })

    it('should detect multiple issues in problematic codebase', () => {
      const files = [
        { path: 'src/components/Counter.tsx', content: NEXTJS_CLIENT_COMPONENT_INVALID },
        { path: 'src/app/about/page.tsx', content: NEXTJS_PAGE_INVALID },
        { path: 'src/components/List.tsx', content: REACT_COMPONENT_INVALID },
      ]

      const stackProfile = {
        webFramework: 'next-app-router',
        uiFramework: 'react',
      }

      const result = analyzeCodePatterns({ files, stackProfile })

      // Should find multiple violations
      expect(result.violations.length).toBeGreaterThan(2)

      // Should have suggestions for each violation
      expect(result.suggestions.length).toBe(result.violations.length)
    })
  })
})
