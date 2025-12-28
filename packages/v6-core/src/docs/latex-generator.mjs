/**
 * LaTeX Generator - Convert Diataxis docs to LaTeX
 * @module @unrdf/v6-core/docs/latex-generator
 */

import { writeFile, mkdir } from 'node:fs/promises';
import { join } from 'node:path';
import { createHash } from 'node:crypto';
import {
  WasmLatexInputSchema,
  WasmLatexOutputSchema,
  LatexCompilerOptionsSchema
} from './latex-generator.schema.mjs';

/**
 * @typedef {Object} LatexOutput
 * @property {string} mainFile - Path to main .tex file
 * @property {string[]} chapterFiles - Paths to chapter files
 * @property {string} hash - SHA256 of all LaTeX content
 * @property {Object} lockfile - Deterministic lockfile
 */

/**
 * Generate LaTeX from Diataxis documentation
 * @param {import('./pipeline.mjs').DiataxisOutput} diataxisDocs - Diataxis docs
 * @param {string} outputDir - Output directory for LaTeX files
 * @returns {Promise<LatexOutput>}
 */
export async function generateLatex(diataxisDocs, outputDir) {
  await mkdir(outputDir, { recursive: true });
  
  const chapterFiles = [];
  const hashes = [];
  
  // Generate preamble
  const preamble = generatePreamble(diataxisDocs);
  const preamblePath = join(outputDir, 'preamble.tex');
  await writeFile(preamblePath, preamble);
  hashes.push(hashString(preamble));
  
  // Generate chapters for each package
  for (const pkg of diataxisDocs.packages) {
    const chapterContent = generatePackageChapter(pkg);
    const chapterPath = join(outputDir, 'chapter-' + sanitizeFilename(pkg.packageName) + '.tex');
    await writeFile(chapterPath, chapterContent);
    chapterFiles.push(chapterPath);
    hashes.push(hashString(chapterContent));
  }
  
  // Generate main file
  const mainContent = generateMainFile(diataxisDocs, chapterFiles);
  const mainFile = join(outputDir, 'thesis.tex');
  await writeFile(mainFile, mainContent);
  hashes.push(hashString(mainContent));
  
  // Generate lockfile
  const lockfile = generateLockfile(diataxisDocs, mainFile, chapterFiles, hashes);
  const lockfilePath = join(outputDir, 'thesis.lock.json');
  await writeFile(lockfilePath, JSON.stringify(lockfile, null, 2));
  
  return {
    mainFile,
    chapterFiles,
    hash: hashString(hashes.join('')),
    lockfile
  };
}

/**
 * Compile LaTeX source to PDF using WASM engine (STUB)
 * @param {string} source - LaTeX source code
 * @param {Object} [options] - Compilation options
 * @param {string} [options.engine='pdflatex'] - LaTeX engine to use
 * @param {number} [options.passes=2] - Number of compilation passes
 * @param {boolean} [options.shellEscape=false] - Enable shell escape
 * @param {string} [options.interaction='nonstopmode'] - Interaction mode
 * @param {number} [options.timeout=30000] - Compilation timeout in ms
 * @returns {Promise<Object>} WASM compilation result
 * @throws {Error} If validation fails
 * @example
 * const result = await compileLatexWithWasm('\\documentclass{article}...');
 * if (result.success) {
 *   console.log('PDF size:', result.pdfBytes.length);
 * }
 */
export async function compileLatexWithWasm(source, options = {}) {
  const startTime = Date.now();

  // Validate input
  const input = WasmLatexInputSchema.parse({
    source,
    mainFile: 'main.tex',
    options: LatexCompilerOptionsSchema.parse(options)
  });

  // STUB: WASM LaTeX compilation not yet implemented
  console.warn('[LaTeX WASM] Compilation stub called - WASM engine not yet integrated');
  console.log('[LaTeX WASM] Engine:', input.options.engine);
  console.log('[LaTeX WASM] Source size:', source.length, 'bytes');
  console.log('[LaTeX WASM] Timeout:', input.options.timeout, 'ms');

  // TODO: Integrate WASM LaTeX compiler (e.g., texlive.js, swiftlatex)
  // Expected integration:
  // 1. Initialize WASM module
  // 2. Load LaTeX source and additional files
  // 3. Run LaTeX engine in WASM sandbox
  // 4. Extract PDF bytes from WASM memory
  // 5. Parse log output for errors/warnings

  const compilationTime = Date.now() - startTime;

  // Return mock output for now
  const output = {
    success: false,
    pdfBytes: null,
    logOutput: 'STUB: WASM LaTeX engine not yet integrated\nExpected output: PDF compilation',
    warnings: ['WASM compilation not implemented'],
    errors: [],
    compilationTime,
    wasmEngineVersion: 'stub-0.0.0'
  };

  return WasmLatexOutputSchema.parse(output);
}

/**
 * Compile LaTeX file to PDF (STUB for WASM engine)
 * @param {string} texFile - Path to .tex file
 * @param {Object} [options={}] - Compilation options
 * @param {string} [options.engine='pdflatex'] - LaTeX engine to use
 * @param {number} [options.passes=2] - Number of compilation passes
 * @param {number} [options.timeout=30000] - Compilation timeout in ms
 * @returns {Promise<string>} Path to generated PDF
 * @example
 * const pdfPath = await compileToPDF('./thesis.tex', { engine: 'xelatex' });
 */
export async function compileToPDF(texFile, options = {}) {
  const startTime = Date.now();

  // Validate options
  const validatedOptions = LatexCompilerOptionsSchema.parse(options);

  // STUB: Will be implemented with WASM LaTeX compiler
  const pdfPath = texFile.replace(/\.tex$/, '.pdf');
  const logPath = texFile.replace(/\.tex$/, '.log');

  console.warn('[LaTeX] PDF compilation not yet implemented (WASM engine pending)');
  console.log('[LaTeX] Input: ' + texFile);
  console.log('[LaTeX] Expected output: ' + pdfPath);
  console.log('[LaTeX] Engine:', validatedOptions.engine);
  console.log('[LaTeX] Passes:', validatedOptions.passes);

  // TODO: Integrate WASM LaTeX compiler
  // Future implementation will:
  // 1. Read .tex file
  // 2. Call compileLatexWithWasm() with source
  // 3. Write PDF bytes to pdfPath
  // 4. Write log output to logPath
  // 5. Return compilation result with stats

  // Stub result object (for future use)
  // const result = {
  //   pdfPath,
  //   success: false,
  //   logPath,
  //   errors: [],
  //   warnings: ['PDF compilation stub - WASM engine not integrated'],
  //   stats: {
  //     compilationTime: Date.now() - startTime
  //   }
  // };

  return pdfPath;
}

/**
 * Initialize WASM LaTeX engine (STUB)
 * @param {Object} [config] - Engine configuration
 * @param {string} [config.enginePath] - Path to WASM engine binary
 * @param {number} [config.memorySize=268435456] - WASM memory size in bytes (default 256MB)
 * @returns {Promise<Object>} Engine instance
 * @throws {Error} If initialization fails
 * @example
 * const engine = await initWasmLatexEngine({ memorySize: 512 * 1024 * 1024 });
 */
export async function initWasmLatexEngine(config = {}) {
  const { enginePath, memorySize = 268435456 } = config;

  console.warn('[LaTeX WASM] Engine initialization stub called');
  console.log('[LaTeX WASM] Engine path:', enginePath || 'default');
  console.log('[LaTeX WASM] Memory size:', memorySize, 'bytes');

  // TODO: Initialize WASM LaTeX engine
  // Expected implementation:
  // 1. Load WASM binary from enginePath or CDN
  // 2. Allocate WASM memory
  // 3. Initialize LaTeX filesystem in WASM
  // 4. Load LaTeX packages and fonts
  // 5. Return engine instance with compile() method

  return {
    version: 'stub-0.0.0',
    initialized: false,
    memorySize,
    compile: compileLatexWithWasm
  };
}

/**
 * Generate LaTeX preamble
 * @param {import('./pipeline.mjs').DiataxisOutput} docs - Diataxis docs
 * @returns {string} LaTeX preamble
 */
function generatePreamble(docs) {
  return '% UNRDF Thesis - Generated ' + docs.generatedAt + '\n' +
         '% Merkle Root: ' + docs.merkleRoot + '\n\n' +
         '\\documentclass[12pt,a4paper]{report}\n\n' +
         '% Packages\n' +
         '\\usepackage[utf8]{inputenc}\n' +
         '\\usepackage[T1]{fontenc}\n' +
         '\\usepackage{lmodern}\n' +
         '\\usepackage{hyperref}\n' +
         '\\usepackage{listings}\n' +
         '\\usepackage{xcolor}\n' +
         '\\usepackage{geometry}\n' +
         '\\usepackage{fancyhdr}\n' +
         '\\usepackage{titlesec}\n\n' +
         '% Page layout\n' +
         '\\geometry{margin=1in}\n\n' +
         '% Hyperlinks\n' +
         '\\hypersetup{\n' +
         '  colorlinks=true,\n' +
         '  linkcolor=blue,\n' +
         '  filecolor=magenta,\n' +
         '  urlcolor=cyan,\n' +
         '  pdftitle={UNRDF Documentation},\n' +
         '  pdfauthor={UNRDF Contributors}\n' +
         '}\n\n' +
         '% Code listings\n' +
         '\\lstset{\n' +
         '  language=JavaScript,\n' +
         '  basicstyle=\\ttfamily\\small,\n' +
         '  keywordstyle=\\color{blue},\n' +
         '  commentstyle=\\color{gray},\n' +
         '  stringstyle=\\color{red},\n' +
         '  numbers=left,\n' +
         '  numberstyle=\\tiny,\n' +
         '  frame=single,\n' +
         '  breaklines=true\n' +
         '}\n\n' +
         '% Headers\n' +
         '\\pagestyle{fancy}\n' +
         '\\fancyhf{}\n' +
         '\\rhead{\\thepage}\n' +
         '\\lhead{\\leftmark}\n';
}

/**
 * Generate main LaTeX file
 * @param {import('./pipeline.mjs').DiataxisOutput} docs - Diataxis docs
 * @param {string[]} chapterFiles - Chapter file paths
 * @returns {string} Main LaTeX content
 */
function generateMainFile(docs, chapterFiles) {
  const chapters = chapterFiles.map(file => {
    const relative = file.split('/').pop();
    return '\\input{' + relative + '}';
  }).join('\n');
  
  return '\\input{preamble.tex}\n\n' +
         '\\begin{document}\n\n' +
         '% Title page\n' +
         '\\title{UNRDF Documentation\\\\\n' +
         '       \\large Version 6.0.0-alpha.1}\n' +
         '\\author{UNRDF Contributors}\n' +
         '\\date{Generated: ' + new Date().toISOString() + '}\n' +
         '\\maketitle\n\n' +
         '% Table of contents\n' +
         '\\tableofcontents\n' +
         '\\newpage\n\n' +
         '% Statistics\n' +
         '\\chapter*{Overview}\n' +
         '\\addcontentsline{toc}{chapter}{Overview}\n\n' +
         'This documentation covers ' + docs.stats.packageCount + ' packages with:\n\n' +
         '\\begin{itemize}\n' +
         '  \\item ' + docs.stats.tutorialCount + ' tutorials\n' +
         '  \\item ' + docs.stats.howtoCount + ' how-to guides\n' +
         '  \\item ' + docs.stats.referenceCount + ' reference documents\n' +
         '  \\item ' + docs.stats.explanationCount + ' explanations\n' +
         '\\end{itemize}\n\n' +
         '\\textbf{Merkle Root:} \\texttt{' + docs.merkleRoot + '}\n\n' +
         '\\newpage\n\n' +
         '% Chapters (one per package)\n' +
         chapters + '\n\n' +
         '\\end{document}\n';
}

/**
 * Generate chapter for a package
 * @param {import('./pipeline.mjs').PackageDocs} pkg - Package docs
 * @returns {string} LaTeX chapter content
 */
function generatePackageChapter(pkg) {
  const sections = [];
  
  sections.push('\\chapter{' + escapeLatex(pkg.packageName) + '}');
  sections.push('\\label{chap:' + sanitizeLabel(pkg.packageName) + '}');
  sections.push('');
  sections.push('Package version: \\texttt{' + escapeLatex(pkg.version) + '}');
  sections.push('');
  
  // Tutorials
  if (pkg.tutorials.length > 0) {
    sections.push('\\section{Tutorials}');
    for (const tutorial of pkg.tutorials) {
      sections.push('\\subsection{' + escapeLatex(tutorial.frontmatter.title || tutorial.name) + '}');
      sections.push(markdownToLatex(tutorial.content));
      sections.push('');
    }
  }
  
  // How-to guides
  if (pkg.howtos.length > 0) {
    sections.push('\\section{How-To Guides}');
    for (const howto of pkg.howtos) {
      sections.push('\\subsection{' + escapeLatex(howto.frontmatter.title || howto.name) + '}');
      sections.push(markdownToLatex(howto.content));
      sections.push('');
    }
  }
  
  // Reference
  if (pkg.reference.length > 0) {
    sections.push('\\section{Reference}');
    for (const ref of pkg.reference) {
      sections.push('\\subsection{' + escapeLatex(ref.frontmatter.title || ref.name) + '}');
      sections.push(markdownToLatex(ref.content));
      sections.push('');
    }
  }
  
  // Explanation
  if (pkg.explanation.length > 0) {
    sections.push('\\section{Explanation}');
    for (const exp of pkg.explanation) {
      sections.push('\\subsection{' + escapeLatex(exp.frontmatter.title || exp.name) + '}');
      sections.push(markdownToLatex(exp.content));
      sections.push('');
    }
  }
  
  return sections.join('\n');
}

/**
 * Convert markdown to LaTeX (simplified)
 * @param {string} markdown - Markdown content
 * @returns {string} LaTeX content
 */
function markdownToLatex(markdown) {
  let latex = markdown;
  
  // Code blocks
  latex = latex.replace(/```(\w+)?\n([\s\S]*?)```/g, function(match, lang, code) {
    return '\\begin{lstlisting}\n' + code + '\\end{lstlisting}';
  });
  
  // Inline code
  latex = latex.replace(/`([^`]+)`/g, '\\texttt{$1}');
  
  // Bold
  latex = latex.replace(/\*\*([^*]+)\*\*/g, '\\textbf{$1}');
  
  // Italic
  latex = latex.replace(/\*([^*]+)\*/g, '\\textit{$1}');
  
  // Links
  latex = latex.replace(/\[([^\]]+)\]\(([^)]+)\)/g, '\\href{$2}{$1}');
  
  // Escape special characters
  latex = escapeLatex(latex);
  
  return latex;
}

/**
 * Escape LaTeX special characters
 * @param {string} text - Text to escape
 * @returns {string} Escaped text
 */
function escapeLatex(text) {
  if (text.includes('\\begin{') || text.includes('\\texttt{')) {
    return text;
  }
  
  return text
    .replace(/\\/g, '\\textbackslash{}')
    .replace(/_/g, '\\_')
    .replace(/\$/g, '\\$')
    .replace(/#/g, '\\#')
    .replace(/%/g, '\\%')
    .replace(/&/g, '\\&')
    .replace(/~/g, '\\textasciitilde{}')
    .replace(/\^/g, '\\textasciicircum{}');
}

/**
 * Sanitize filename for LaTeX
 * @param {string} name - Package name
 * @returns {string} Sanitized name
 */
function sanitizeFilename(name) {
  return name.replace(/@/g, '').replace(/\//g, '-');
}

/**
 * Sanitize label for LaTeX
 * @param {string} name - Label name
 * @returns {string} Sanitized label
 */
function sanitizeLabel(name) {
  return name.replace(/@/g, '').replace(/\//g, '-').replace(/\./g, '-');
}

/**
 * Generate deterministic lockfile
 * @param {import('./pipeline.mjs').DiataxisOutput} docs - Diataxis docs
 * @param {string} mainFile - Main file path
 * @param {string[]} chapterFiles - Chapter file paths
 * @param {string[]} hashes - Content hashes
 * @returns {Object} Lockfile object
 */
function generateLockfile(docs, mainFile, chapterFiles, hashes) {
  return {
    version: '6.0.0-alpha.1',
    generatedAt: docs.generatedAt,
    merkleRoot: docs.merkleRoot,
    files: {
      main: mainFile,
      chapters: chapterFiles.map(f => f.split('/').pop())
    },
    latex: {
      compiler: 'latexmk',
      version: '4.77',
      hash: hashString(hashes.join(''))
    },
    packages: docs.packages.reduce((acc, pkg) => {
      acc[pkg.packageName] = {
        version: pkg.version,
        tutorialCount: pkg.tutorials.length,
        howtoCount: pkg.howtos.length,
        referenceCount: pkg.reference.length,
        explanationCount: pkg.explanation.length
      };
      return acc;
    }, {})
  };
}

/**
 * Hash a string with SHA256
 * @param {string} str - String to hash
 * @returns {string} Hex hash
 */
function hashString(str) {
  return createHash('sha256').update(str).digest('hex');
}
