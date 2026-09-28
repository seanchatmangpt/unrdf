/**
 * @file Tests for latex-generator.mjs
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { mkdir, writeFile, rm, readFile } from 'node:fs/promises';
import { join } from 'node:path';
import { tmpdir } from 'node:os';
import { generateLatex } from '../../src/docs/latex-generator.mjs';

describe('LaTeX Generation', () => {
  let tempDir;
  let diataxisOutput;

  beforeEach(async () => {
    tempDir = join(tmpdir(), `test-latex-${Date.now()}`);
    await mkdir(tempDir, { recursive: true });

    diataxisOutput = {
      packages: [
        {
          packageName: '@unrdf/test',
          version: '1.0.0',
          tutorials: [
            {
              name: 'getting-started.md',
              frontmatter: { title: 'Getting Started' },
              content: 'This is a **tutorial**.\n\n```javascript\nconst x = 1;\n```'
            }
          ],
          howtos: [
            {
              name: 'how-to-use.md',
              frontmatter: { title: 'How to Use' },
              content: 'This is a _howto_ guide.'
            }
          ],
          reference: [
            {
              name: 'api.md',
              frontmatter: { title: 'API Reference' },
              content: 'API documentation with `inline code`.'
            }
          ],
          explanation: [
            {
              name: 'concepts.md',
              frontmatter: { title: 'Core Concepts' },
              content: 'Explanation of core concepts.'
            }
          ]
        }
      ],
      merkleRoot: 'abc123',
      generatedAt: '2024-01-01T00:00:00.000Z',
      stats: {
        packageCount: 1,
        tutorialCount: 1,
        howtoCount: 1,
        referenceCount: 1,
        explanationCount: 1,
        totalDocs: 4
      }
    };
  });

  afterEach(async () => {
    await rm(tempDir, { recursive: true, force: true });
  });

  it('should generate LaTeX files', async () => {
    const result = await generateLatex(diataxisOutput, tempDir);

    expect(result).toBeDefined();
    expect(result.mainFile).toBeTruthy();
    expect(result.chapterFiles).toBeInstanceOf(Array);
    expect(result.chapterFiles.length).toBe(1);
    expect(result.hash).toBeTruthy();
    expect(result.lockfile).toBeDefined();
  });

  it('should create main thesis file', async () => {
    const result = await generateLatex(diataxisOutput, tempDir);

    const mainContent = await readFile(result.mainFile, 'utf8');

    expect(mainContent).toContain('\\documentclass');
    expect(mainContent).toContain('\\begin{document}');
    expect(mainContent).toContain('\\end{document}');
    expect(mainContent).toContain('\\tableofcontents');
  });

  it('should create chapter files for packages', async () => {
    const result = await generateLatex(diataxisOutput, tempDir);

    expect(result.chapterFiles.length).toBe(1);

    const chapterContent = await readFile(result.chapterFiles[0], 'utf8');

    expect(chapterContent).toContain('\\chapter{@unrdf/test}');
    expect(chapterContent).toContain('\\section{Tutorials}');
    expect(chapterContent).toContain('Getting Started');
  });

  it('should escape LaTeX special characters', async () => {
    diataxisOutput.packages[0].tutorials[0].content = 'Test $100 & 50% more_data';

    const result = await generateLatex(diataxisOutput, tempDir);
    const chapterContent = await readFile(result.chapterFiles[0], 'utf8');

    // Special chars should be escaped
    expect(chapterContent).toContain('\\$');
    expect(chapterContent).toContain('\\&');
    expect(chapterContent).toContain('\\%');
  });

  it('should convert markdown to LaTeX', async () => {
    const result = await generateLatex(diataxisOutput, tempDir);
    const chapterContent = await readFile(result.chapterFiles[0], 'utf8');

    // Bold text
    expect(chapterContent).toContain('\\textbf{');

    // Code blocks
    expect(chapterContent).toContain('\\begin{lstlisting}');

    // Inline code
    expect(chapterContent).toContain('\\texttt{');
  });

  it('should generate lockfile with metadata', async () => {
    const result = await generateLatex(diataxisOutput, tempDir);

    expect(result.lockfile.version).toBeTruthy();
    expect(result.lockfile.merkleRoot).toBe('abc123');
    expect(result.lockfile.files).toBeDefined();
    expect(result.lockfile.files.main).toBeTruthy();
    expect(result.lockfile.packages).toBeDefined();
  });

  it('should generate compilable LaTeX document', async () => {
    const result = await generateLatex(diataxisOutput, tempDir);
    const mainContent = await readFile(result.mainFile, 'utf8');

    // Should have all required LaTeX structure
    expect(mainContent).toMatch(/\\documentclass\[.*\]{report}/);
    expect(mainContent).toContain('\\input{preamble.tex}');
    expect(mainContent).toContain('\\maketitle');
    expect(mainContent).toContain('\\tableofcontents');
    expect(mainContent).toContain('\\newpage');

    // Should include chapter
    expect(mainContent).toMatch(/\\input{chapter-.*\.tex}/);
  });

  it('should create preamble with packages', async () => {
    const result = await generateLatex(diataxisOutput, tempDir);
    const preamblePath = join(tempDir, 'preamble.tex');
    const preambleContent = await readFile(preamblePath, 'utf8');

    // Essential LaTeX packages
    expect(preambleContent).toContain('\\usepackage{hyperref}');
    expect(preambleContent).toContain('\\usepackage{listings}');
    expect(preambleContent).toContain('\\usepackage{xcolor}');
    expect(preambleContent).toContain('\\usepackage{geometry}');
  });

  it('should handle multiple packages', async () => {
    diataxisOutput.packages.push({
      packageName: '@unrdf/other',
      version: '2.0.0',
      tutorials: [],
      howtos: [],
      reference: [{
        name: 'api.md',
        frontmatter: { title: 'API' },
        content: 'Content'
      }],
      explanation: []
    });

    const result = await generateLatex(diataxisOutput, tempDir);

    expect(result.chapterFiles.length).toBe(2);
  });
});
