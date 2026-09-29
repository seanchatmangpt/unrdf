import { describe, it, expect } from 'vitest'
import { execFileSync, spawnSync } from 'node:child_process'
import { mkdtempSync, readFileSync, readdirSync, existsSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { fileURLToPath } from 'node:url'
import {
  TEMPLATE_DIR,
  texEscape,
  headingVariables,
  renderTemplate,
  renderMarkdown
} from '../src/render.mjs'
import { VERSION, dependencyVersions } from '../src/version.mjs'

const CLI = fileURLToPath(new URL('../src/cli/index.mjs', import.meta.url))
const PKG = JSON.parse(
  readFileSync(fileURLToPath(new URL('../package.json', import.meta.url)), 'utf8')
)

describe('texEscape', () => {
  it('escapes LaTeX special characters', () => {
    expect(texEscape('50% of $5 & #1_x {y} ~z ^w \\')).toBe(
      '50\\% of \\$5 \\& \\#1\\_x \\{y\\} \\textasciitilde{}z \\textasciicircum{}w \\textbackslash{}'
    )
  })

  it('maps null and undefined to an empty string', () => {
    expect(texEscape(undefined)).toBe('')
    expect(texEscape(null)).toBe('')
  })
})

describe('headingVariables', () => {
  it('derives singular and plural variable names', () => {
    expect(headingVariables('Methods')).toEqual(['methods', 'method'])
    expect(headingVariables('Design & Development')).toEqual(['design'])
    expect(headingVariables('Results')).toEqual(['results', 'result'])
    expect(headingVariables('')).toEqual([])
  })
})

describe('templates', () => {
  const templates = readdirSync(TEMPLATE_DIR).filter(name => name.endsWith('.tex.njk'))

  it('ships the paper and thesis templates', () => {
    expect(templates.sort()).toEqual([
      'argument.tex.njk',
      'contribution.tex.njk',
      'dsr.tex.njk',
      'imrad.tex.njk',
      'monograph.tex.njk',
      'narrative.tex.njk',
      'paper.tex.njk'
    ])
  })

  for (const name of readdirSync(TEMPLATE_DIR).filter(n => n.endsWith('.tex.njk'))) {
    it(`renders ${name} with escaped title and author`, () => {
      const out = renderTemplate(name, {
        title: 'A & B',
        author: 'C_D',
        abstract: 'abs',
        sections: [{ heading: 'Introduction', content: 'x' }]
      })
      expect(out).toContain('A \\& B')
      expect(out).toContain('C\\_D')
      expect(out).toContain('\\begin{document}')
      expect(out).toContain('\\end{document}')
    })
  }

  it('maps section content onto named template variables', () => {
    const out = renderTemplate('imrad.tex.njk', {
      title: 'T',
      author: 'A',
      sections: [{ heading: 'Methods', content: 'we did things' }]
    })
    expect(out).toContain('we did things')
  })

  it('renders markdown without a template', () => {
    expect(
      renderMarkdown({ title: 'T', abstract: 'abs', sections: [{ heading: 'H', content: 'c' }] }, '*A*')
    ).toBe('# T\n\n*A*\n\n## Abstract\n\nabs\n\n## H\n\nc\n')
  })
})

describe('version', () => {
  it('reads the playground package.json', () => {
    expect(VERSION).toBe(PKG.version)
  })

  it('reports declared dependency versions without range prefixes', () => {
    const versions = dependencyVersions('zod', 'citty', 'not-a-dependency')
    expect(versions.zod).toBe(PKG.dependencies.zod.replace(/^\^/, ''))
    expect(versions.citty).toBe(PKG.dependencies.citty.replace(/^\^/, ''))
    expect(versions['not-a-dependency']).toBe('unknown')
  })
})

describe('CLI generate', () => {
  const dir = mkdtempSync(join(tmpdir(), 'playground-cli-'))

  it('writes the rendered paper to the requested file', () => {
    const out = join(dir, 'paper.tex')
    execFileSync('node', [CLI, 'papers', 'generate', 'imrad', '-t', 'My Paper', '-a', 'Ann', '-o', out, '-q'])
    expect(existsSync(out)).toBe(true)
    expect(readFileSync(out, 'utf8')).toContain('\\title{My Paper}')
    expect(readFileSync(out, 'utf8')).toContain('\\author{Ann}')
  })

  it('writes the rendered thesis to the requested file', () => {
    const out = join(dir, 'thesis.tex')
    execFileSync('node', [CLI, 'thesis', 'generate', 'monograph', '-t', 'My Thesis', '-a', 'Ann', '-o', out, '-q'])
    expect(readFileSync(out, 'utf8')).toContain('My Thesis')
  })

  it('reports Zod validation issues and exits non-zero', () => {
    const result = spawnSync('node', [CLI, 'papers', 'generate', 'imrad', '--title', '', '--author', ''], {
      encoding: 'utf8'
    })
    expect(result.status).toBe(1)
    expect(result.stderr).toContain('title: Title is required')
    expect(result.stderr).toContain('author: Author is required')
  })
})
