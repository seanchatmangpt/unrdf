#!/usr/bin/env node
/**
 * @file Generate LaTeX package index table from JSON
 * @description Agent 2: Transform index.json into index.tex with statistics
 */

import { readFileSync, writeFileSync } from 'fs';
import { join } from 'path';

// Read the index.json file
const indexPath = '/home/user/unrdf/thesis/packages/index.json';
const indexData = JSON.parse(readFileSync(indexPath, 'utf-8'));

const packages = indexData.packages;

// Count statistics
const stats = {
  total: packages.length,
  js: packages.filter(p => p.kind === 'js').length,
  rust: packages.filter(p => p.kind === 'rust').length,
  docs: packages.filter(p => p.kind === 'docs').length,
};

// Collect all unique dependencies
const allDeps = new Set();
packages.forEach(pkg => {
  pkg.deps.forEach(dep => allDeps.add(dep));
});
stats.totalDeps = allDeps.size;

// Generate LaTeX table rows
const rows = packages.map(pkg => {
  const depsCount = pkg.deps.length;
  const escapedPath = pkg.path.replace(/_/g, '\\_');
  return `\\texttt{${pkg.id}} & ${pkg.name} & ${pkg.kind} & ${escapedPath} & ${depsCount} & p.~\\pageref{pkg:${pkg.id}} \\\\`;
}).join('\n');

// Generate the LaTeX document
const latexContent = `% Auto-generated package inventory table
% Generated: ${new Date().toISOString()}
% Source: ${indexPath}

\\section*{Package Inventory}

\\begin{longtable}{|l|l|l|l|r|p{2cm}|}
\\hline
\\textbf{ID} & \\textbf{Name} & \\textbf{Kind} & \\textbf{Path} & \\textbf{Deps} & \\textbf{Chapter} \\\\
\\hline
\\endfirsthead

\\multicolumn{6}{c}%
{\\tablename\\ \\thetable\\ -- \\textit{Continued from previous page}} \\\\
\\hline
\\textbf{ID} & \\textbf{Name} & \\textbf{Kind} & \\textbf{Path} & \\textbf{Deps} & \\textbf{Chapter} \\\\
\\hline
\\endhead

\\hline \\multicolumn{6}{r}{\\textit{Continued on next page}} \\\\
\\endfoot

\\hline
\\endlastfoot

${rows}
\\end{longtable}

\\subsection*{Statistics}

\\begin{itemize}
\\item Total packages: ${stats.total}
\\item JavaScript packages: ${stats.js}
\\item Rust packages: ${stats.rust}
\\item Documentation packages: ${stats.docs}
\\item Unique external dependencies: ${stats.totalDeps}
\\end{itemize}

\\subsection*{Package Distribution}

\\begin{itemize}
\\item Core RDF packages: ${packages.filter(p => p.name.includes('core') || p.name.includes('oxigraph') || p.name.includes('streaming')).length}
\\item YAWL workflow packages: ${packages.filter(p => p.name.includes('yawl')).length}
\\item KGC packages: ${packages.filter(p => p.name.includes('kgc')).length}
\\item Integration packages: ${packages.filter(p => p.name.includes('kafka') || p.name.includes('langchain') || p.name.includes('graphql')).length}
\\item Example/Template projects: ${packages.filter(p => p.path.startsWith('templates/') || p.path.startsWith('playground/')).length}
\\end{itemize}
`;

// Write the LaTeX file
const outputPath = '/home/user/unrdf/thesis/packages/index.tex';
writeFileSync(outputPath, latexContent, 'utf-8');

// Print results to stdout
console.log(`Generated index.tex with ${stats.total} rows`);
console.log(`Statistics: ${stats.js} JS, ${stats.rust} Rust, ${stats.docs} Docs, ${stats.totalDeps} unique deps total`);
console.log(`File written to ${outputPath}`);
