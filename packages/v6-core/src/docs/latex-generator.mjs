/**
 * LaTeX Generator - Convert Diataxis docs to LaTeX
 * @module @unrdf/v6-core/docs/latex-generator
 */

import { writeFile, mkdir } from 'node:fs/promises';
import { join, dirname } from 'node:path';
import { createHash } from 'node:crypto';

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
 * Compile LaTeX to PDF (stubbed for WASM engine)
 * @param {string} texFile - Path to .tex file
 * @param {Object} [options={}] - Compilation options
 * @returns {Promise<string>} Path to generated PDF
 */
export async function compileToPDF(texFile, options = {}) {
  // STUB: Will be implemented with WASM LaTeX compiler
  const pdfPath = texFile.replace(/\.tex$/, '.pdf');
  
  console.warn('[LaTeX] PDF compilation not yet implemented (WASM engine pending)');
  console.log('[LaTeX] Expected output: ' + pdfPath);
  console.log('[LaTeX] Input: ' + texFile);
  
  // TODO: Integrate WASM LaTeX compiler
  
  return pdfPath;
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
