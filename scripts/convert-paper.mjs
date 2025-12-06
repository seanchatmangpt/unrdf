#!/usr/bin/env node

/**
 * LaTeX to MDX Paper Converter
 *
 * Converts academic LaTeX papers to MDX format for Nextra documentation site.
 *
 * Usage:
 *   pnpm convert-paper <input.tex> [options]           # Single file
 *   pnpm convert-paper --walk <directory> [options]    # Batch convert directory
 *
 * @see /docs/PAPERS_POLICY.md
 */

import fs from 'node:fs/promises'
import path from 'node:path'
import { fileURLToPath } from 'node:url'
import { defineCommand, runMain } from 'citty'

const __dirname = path.dirname(fileURLToPath(import.meta.url))
const ROOT_DIR = path.resolve(__dirname, '..')

// ============================================================================
// Configuration
// ============================================================================

const DEFAULT_OUTPUT_DIR = path.join(ROOT_DIR, 'packages/nextra/app/papers')
const DEFAULT_STATUS = 'draft'

// ============================================================================
// Citty CLI Definition
// ============================================================================

const main = defineCommand({
  meta: {
    name: 'convert-paper',
    version: '2.0.0',
    description: 'Convert LaTeX papers to MDX for Nextra',
  },
  args: {
    input: {
      type: 'positional',
      description: 'Input LaTeX file or directory (with --walk)',
      required: false,
    },
    walk: {
      type: 'string',
      description: 'Walk directory and convert all .tex files',
    },
    slug: {
      type: 'string',
      description: 'Output directory name (e.g., 2024-my-paper)',
    },
    authors: {
      type: 'string',
      description: 'Comma-separated author names',
      default: 'Sean Chatman',
    },
    status: {
      type: 'string',
      description: 'Paper status (draft|review|published|archived)',
      default: DEFAULT_STATUS,
    },
    output: {
      type: 'string',
      description: 'Output directory',
      default: DEFAULT_OUTPUT_DIR,
    },
    preserveLatex: {
      type: 'boolean',
      description: 'Copy LaTeX source to content/papers/latex-source/',
      default: false,
    },
    noFigures: {
      type: 'boolean',
      description: 'Skip figure extraction',
      default: false,
    },
    'auto-slug': {
      type: 'boolean',
      description: 'Auto-generate slug from filename (YYYY-filename)',
      default: false,
    },
    verbose: {
      type: 'boolean',
      description: 'Verbose output',
      default: false,
      alias: 'v',
    },
  },
  async run({ args }) {
    // Determine mode: single file or walk directory
    if (args.walk) {
      await walkAndConvert(args)
    } else if (args.input) {
      await convertSingleFile(args)
    } else {
      console.error('❌ Error: Either provide an input file or use --walk <directory>')
      process.exit(1)
    }
  },
})

// ============================================================================
// Walk & Batch Convert
// ============================================================================

async function walkAndConvert(args) {
  const dir = args.walk
  console.log(`🚶 Walking directory: ${dir}`)
  console.log('')

  // Find all .tex files
  const texFiles = await findTexFiles(dir)

  if (texFiles.length === 0) {
    console.log('❌ No .tex files found in', dir)
    return
  }

  console.log(`📚 Found ${texFiles.length} LaTeX files:`)
  texFiles.forEach((file, i) => {
    console.log(`   ${i + 1}. ${path.basename(file)}`)
  })
  console.log('')

  // Convert each file
  let successCount = 0
  let failCount = 0

  for (const file of texFiles) {
    try {
      const slug = args.slug || generateSlug(file, args['auto-slug'])
      const fileArgs = {
        ...args,
        input: file,
        slug,
      }

      if (args.verbose) {
        console.log(`🔄 Converting: ${path.basename(file)} → ${slug}`)
      }

      await convertPaper(fileArgs)
      successCount++

      if (!args.verbose) {
        console.log(`✅ ${successCount}/${texFiles.length} ${slug}`)
      }
    } catch (error) {
      failCount++
      console.error(`❌ Failed: ${path.basename(file)} - ${error.message}`)
    }
  }

  console.log('')
  console.log(`✨ Batch conversion complete!`)
  console.log(`   Success: ${successCount}/${texFiles.length}`)
  if (failCount > 0) {
    console.log(`   Failed: ${failCount}/${texFiles.length}`)
  }
  console.log('')
  console.log('Next steps:')
  console.log('1. Update navigation: packages/nextra/app/papers/_meta.ts')
  console.log('2. Preview: pnpm -C packages/nextra dev')
  console.log('3. Build: pnpm -C packages/nextra build')
  console.log('')
}

async function findTexFiles(dir) {
  const files = []
  const entries = await fs.readdir(dir, { withFileTypes: true })

  for (const entry of entries) {
    const fullPath = path.join(dir, entry.name)

    if (entry.isDirectory()) {
      // Recursively search subdirectories
      const subFiles = await findTexFiles(fullPath)
      files.push(...subFiles)
    } else if (entry.isFile() && entry.name.endsWith('.tex')) {
      files.push(fullPath)
    }
  }

  return files.sort()
}

function generateSlug(filePath, autoSlug) {
  const basename = path.basename(filePath, '.tex')

  if (autoSlug) {
    // Auto-generate: 2024-filename
    const year = new Date().getFullYear()
    // Remove chapter numbers like "25-" from "25-mu-calculus-theory.tex"
    const cleanName = basename.replace(/^\d+-/, '')
    return `${year}-${cleanName}`
  }

  // Otherwise, user must provide --slug
  console.error(`❌ Error: --slug required for ${basename} (or use --auto-slug)`)
  process.exit(1)
}

// ============================================================================
// Single File Conversion
// ============================================================================

async function convertSingleFile(args) {
  if (!args.slug && !args['auto-slug']) {
    console.error('❌ Error: --slug required (or use --auto-slug)')
    process.exit(1)
  }

  const slug = args.slug || generateSlug(args.input, args['auto-slug'])
  await convertPaper({ ...args, slug })
}

// ============================================================================
// LaTeX Parsing & Conversion
// ============================================================================

/**
 * Convert LaTeX content to MDX
 */
function convertLatexToMDX(latexContent, config) {
  let mdx = latexContent

  // Remove LaTeX comments (lines starting with %)
  mdx = mdx.replace(/^%.*$/gm, '')

  // Extract title from \title{} or \chapter{}
  let titleMatch = mdx.match(/\\title\{([^}]+)\}/)
  if (!titleMatch) {
    // Try to extract from \chapter{}
    const chapterMatch = mdx.match(/\\chapter\{\\texorpdfstring\{([^}]+)\}\{([^}]+)\}\}/) ||
                        mdx.match(/\\chapter\{([^}]+)\}/)
    if (chapterMatch) {
      // Use the first argument of \texorpdfstring if present, otherwise the whole match
      titleMatch = { 1: chapterMatch[1] }
    }
  }
  const title = titleMatch ? titleMatch[1].replace(/\$\\mu\(O\)\$/g, 'μ(O)') : 'Untitled Paper'

  const authorMatch = mdx.match(/\\author\{([^}]+)\}/)
  const authors = config.authors.length > 0
    ? config.authors
    : (authorMatch ? [authorMatch[1]] : ['Unknown Author'])

  const dateMatch = mdx.match(/\\date\{([^}]+)\}/)
  const date = dateMatch ? dateMatch[1] : new Date().toISOString().split('T')[0]

  // Extract abstract (or use first section introduction as abstract)
  let abstractMatch = mdx.match(/\\begin\{abstract\}([\s\S]*?)\\end\{abstract\}/m)
  if (!abstractMatch) {
    // Try to extract first paragraph after \section{Introduction}
    const introMatch = mdx.match(/\\section\{Introduction[^}]*\}\s*([\s\S]{100,500}?)\n\n/)
    abstractMatch = introMatch ? { 1: introMatch[1].trim() } : null
  }
  const abstract = abstractMatch
    ? abstractMatch[1].trim().replace(/\n/g, '\n  ')
    : 'Abstract not provided.'

  // Remove LaTeX preamble (everything before \begin{document})
  mdx = mdx.replace(/^[\s\S]*?\\begin\{document\}/m, '')
  mdx = mdx.replace(/\\end\{document\}[\s\S]*$/m, '')

  // Remove \maketitle
  mdx = mdx.replace(/\\maketitle\s*/g, '')

  // Remove abstract environment (already extracted above)
  mdx = mdx.replace(/\\begin\{abstract\}[\s\S]*?\\end\{abstract\}/gm, '')

  // Remove \chapter{} commands (title already extracted)
  mdx = mdx.replace(/\\chapter\{\\texorpdfstring\{[^}]+\}\{[^}]+\}\}\s*/g, '')
  mdx = mdx.replace(/\\chapter\{[^}]+\}\s*/g, '')

  // Remove \label{} commands
  mdx = mdx.replace(/\\label\{[^}]+\}\s*/g, '')

  // Convert sections
  mdx = mdx.replace(/\\section\*?\{([^}]+)\}/g, '## $1')
  mdx = mdx.replace(/\\subsection\*?\{([^}]+)\}/g, '### $1')
  mdx = mdx.replace(/\\subsubsection\*?\{([^}]+)\}/g, '#### $1')
  mdx = mdx.replace(/\\paragraph\{([^}]+)\}/g, '##### $1')

  // Convert \begin{quote} ... \end{quote} → blockquote
  mdx = mdx.replace(/\\begin\{quote\}([\s\S]*?)\\end\{quote\}/g, (_, content) => {
    return content.trim().split('\n').map(line => '> ' + line.trim()).join('\n')
  })

  // Convert text formatting
  mdx = mdx.replace(/\\textbf\{([^}]+)\}/g, '**$1**')
  mdx = mdx.replace(/\\textit\{([^}]+)\}/g, '*$1*')
  mdx = mdx.replace(/\\emph\{([^}]+)\}/g, '*$1*')
  mdx = mdx.replace(/\\texttt\{([^}]+)\}/g, '`$1`')

  // Convert math - display mode
  // \[ ... \] → ```math\n...\n```
  mdx = mdx.replace(/\\\[([\s\S]*?)\\\]/g, (_, math) => {
    return '```math\n' + math.trim() + '\n```'
  })

  // $$ ... $$ → ```math\n...\n```
  mdx = mdx.replace(/\$\$([\s\S]*?)\$\$/g, (_, math) => {
    return '```math\n' + math.trim() + '\n```'
  })

  // \begin{equation} ... \end{equation} → ```math\n...\n```
  mdx = mdx.replace(/\\begin\{equation\*?\}([\s\S]*?)\\end\{equation\*?\}/g, (_, math) => {
    return '```math\n' + math.trim() + '\n```'
  })

  // \begin{align} ... \end{align} → ```math\n...\n```
  mdx = mdx.replace(/\\begin\{align\*?\}([\s\S]*?)\\end\{align\*?\}/g, (_, math) => {
    return '```math\n' + math.trim() + '\n```'
  })

  // Math - inline mode ($...$) - leave as is (KaTeX handles it)

  // Convert lists
  // \begin{enumerate} → ordered list (process first before itemize to avoid conflicts)
  mdx = mdx.replace(/\\begin\{enumerate\}([\s\S]*?)\\end\{enumerate\}/g, (match, content) => {
    let counter = 0
    const converted = content.replace(/^\s*\\item\s+/gm, () => {
      counter++
      return `${counter}. `
    })
    return converted
  })

  // \begin{itemize} → unordered list
  mdx = mdx.replace(/\\begin\{itemize\}([\s\S]*?)\\end\{itemize\}/g, (match, content) => {
    return content.replace(/^\s*\\item\s+/gm, '- ')
  })

  // Convert citations (simple version)
  mdx = mdx.replace(/\\cite\{([^}]+)\}/g, '[$1](#references)')
  mdx = mdx.replace(/\\citep\{([^}]+)\}/g, '[$1](#references)')
  mdx = mdx.replace(/\\citet\{([^}]+)\}/g, '[$1](#references)')

  // Convert cross-references
  // Section~\ref{sec:foo} → [Section](#foo)
  // Chapter~\ref{ch:bar} → [Chapter](#bar)
  mdx = mdx.replace(/Section~\\ref\{([^}]+)\}/g, '[Section](#$1)')
  mdx = mdx.replace(/Chapter~\\ref\{([^}]+)\}/g, '[Chapter](#$1)')
  mdx = mdx.replace(/\\ref\{([^}]+)\}/g, '[Reference](#$1)') // Fallback for plain \ref

  // Convert figures
  mdx = mdx.replace(
    /\\begin\{figure\}[\s\S]*?\\includegraphics(?:\[[^\]]*\])?\{([^}]+)\}[\s\S]*?\\caption\{([^}]+)\}[\s\S]*?\\end\{figure\}/g,
    (_, imagePath, caption) => {
      const basename = path.basename(imagePath, path.extname(imagePath))
      return `![${caption}](./figures/${basename}.svg)`
    }
  )

  // Convert tables (basic support)
  // Note: Complex tables require manual fixup
  mdx = mdx.replace(
    /\\begin\{table\}[\s\S]*?\\begin\{tabular\}[\s\S]*?\\end\{tabular\}[\s\S]*?\\caption\{([^}]+)\}[\s\S]*?\\end\{table\}/g,
    '**Table**: $1\n\n(Table conversion requires manual editing)\n'
  )

  // Convert theorems, definitions, etc. to Callouts
  mdx = mdx.replace(
    /\\begin\{theorem\}(?:\[([^\]]+)\])?([\s\S]*?)\\end\{theorem\}/g,
    (_, label, content) => {
      const title = label ? `Theorem (${label})` : 'Theorem'
      return `<Callout type="info" title="${title}">\n${content.trim()}\n</Callout>`
    }
  )

  mdx = mdx.replace(
    /\\begin\{definition\}(?:\[([^\]]+)\])?([\s\S]*?)\\end\{definition\}/g,
    (_, label, content) => {
      const title = label ? `Definition (${label})` : 'Definition'
      return `<Callout type="default" title="${title}">\n${content.trim()}\n</Callout>`
    }
  )

  mdx = mdx.replace(
    /\\begin\{lemma\}(?:\[([^\]]+)\])?([\s\S]*?)\\end\{lemma\}/g,
    (_, label, content) => {
      const title = label ? `Lemma (${label})` : 'Lemma'
      return `<Callout type="info" title="${title}">\n${content.trim()}\n</Callout>`
    }
  )

  mdx = mdx.replace(
    /\\begin\{proof\}([\s\S]*?)\\end\{proof\}/g,
    (_, content) => {
      return `<details>\n<summary>**Proof**</summary>\n\n${content.trim()}\n\n</details>`
    }
  )

  // Remove common LaTeX commands that don't need conversion
  mdx = mdx.replace(/\\noindent\s*/g, '')
  mdx = mdx.replace(/\\newpage\s*/g, '\n\n---\n\n')
  mdx = mdx.replace(/\\pagebreak\s*/g, '\n\n---\n\n')
  mdx = mdx.replace(/\\clearpage\s*/g, '\n\n---\n\n')
  mdx = mdx.replace(/~\\cite/g, ' [') // Non-breaking space before citation
  mdx = mdx.replace(/~\\ref/g, ' [')  // Non-breaking space before reference

  // Clean up excessive whitespace
  mdx = mdx.replace(/\n{3,}/g, '\n\n')
  mdx = mdx.trim()

  // Generate front matter
  const frontMatter = generateFrontMatter({
    title,
    authors,
    date,
    status: config.status,
    abstract,
    slug: config.slug
  })

  return frontMatter + '\n\n' + mdx
}

/**
 * Generate YAML front matter for MDX
 */
function generateFrontMatter({ title, authors, date, status, abstract, slug }) {
  const today = new Date().toISOString().split('T')[0]

  // Generate citation
  const firstAuthor = authors[0].split(' ').pop() // Last name
  const year = date.split('-')[0]
  const citation = `${firstAuthor}, ${authors[0].split(' ')[0][0]}. (${year}). ${title}. UNRDF Technical Report. https://seanchatmangpt.github.io/unrdf/papers/${slug}`

  return `---
title: "${title}"
authors:
${authors.map(a => `  - ${a}`).join('\n')}
date: "${date}"
updated: "${today}"
status: ${status}
abstract: |
  ${abstract.split('\n').join('\n  ')}
keywords:
  - RDF
  - Knowledge Graphs
  - Semantic Web
citation: |
  ${citation}
---

# ${title}

<div className="text-sm text-gray-600 dark:text-gray-400 mb-8">
  **Authors**: ${authors.join(', ')} • **Published**: ${date} • **Status**: ${status}
</div>

## Abstract

${abstract}`
}

// ============================================================================
// File Operations
// ============================================================================

/**
 * Main conversion function
 */
async function convertPaper(config) {
  const verbose = config.verbose

  if (verbose) {
    console.log('🚀 Starting LaTeX → MDX conversion...')
    console.log(`   Input: ${config.input}`)
    console.log(`   Slug: ${config.slug}`)
    console.log(`   Authors: ${config.authors}`)
    console.log(`   Status: ${config.status}`)
    console.log('')
  }

  // Read LaTeX file
  const latexContent = await fs.readFile(config.input, 'utf-8')

  // Convert to MDX
  const authors = typeof config.authors === 'string'
    ? config.authors.split(',').map(a => a.trim())
    : config.authors
  const mdxContent = convertLatexToMDX(latexContent, { ...config, authors })

  // Create output directory
  const outputPath = path.join(config.output, config.slug)
  await fs.mkdir(outputPath, { recursive: true })

  // Create figures directory
  if (!config.noFigures) {
    const figuresPath = path.join(outputPath, 'figures')
    await fs.mkdir(figuresPath, { recursive: true })
  }

  // Write MDX file
  const mdxFilePath = path.join(outputPath, 'page.mdx')
  await fs.writeFile(mdxFilePath, mdxContent, 'utf-8')

  // Preserve LaTeX source if requested
  if (config.preserveLatex) {
    const latexArchiveDir = path.join(ROOT_DIR, 'content/papers/latex-source')
    await fs.mkdir(latexArchiveDir, { recursive: true })
    const latexArchivePath = path.join(latexArchiveDir, `${config.slug}.tex`)
    await fs.copyFile(config.input, latexArchivePath)
  }

  if (verbose) {
    console.log(`✅ Written MDX file: ${mdxFilePath}`)
    console.log('')
    console.log('✨ Conversion complete!')
    console.log('')
  }
}

// ============================================================================
// Main
// ============================================================================

runMain(main)
