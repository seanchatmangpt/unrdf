/**
 * Thesis Builder - Full pipeline from packages to PDF
 * @module @unrdf/v6-core/docs/thesis-builder
 */

import { mkdir, writeFile } from 'node:fs/promises';
import { join } from 'node:path';
import { collectPackageDocs, generateDiataxisOutput, emitReceipt } from './pipeline.mjs';
import { generateLatex, compileToPDF } from './latex-generator.mjs';

/**
 * @typedef {Object} ThesisConfig
 * @property {string[]} packageDirs - Package directories to include
 * @property {string} outputDir - Output directory for thesis
 * @property {boolean} [generatePDF=false] - Whether to compile PDF
 * @property {boolean} [emitReceipts=true] - Whether to emit receipts
 * @property {string} [receiptsDir] - Directory for receipts (default: outputDir/receipts)
 */

/**
 * @typedef {Object} ThesisOutput
 * @property {string} latexDir - LaTeX output directory
 * @property {string} mainTexFile - Main .tex file path
 * @property {string} [pdfFile] - PDF file path (if generated)
 * @property {Object[]} receipts - Array of receipts from each stage
 * @property {Object} stats - Build statistics
 */

/**
 * Build thesis from package documentation
 * @param {ThesisConfig} config - Thesis configuration
 * @returns {Promise<ThesisOutput>}
 */
export async function buildThesis(config) {
  const {
    packageDirs,
    outputDir,
    generatePDF = false,
    emitReceipts = true,
    receiptsDir = join(outputDir, 'receipts')
  } = config;
  
  const startTime = Date.now();
  const receipts = [];
  
  console.log('[Thesis] Starting build...');
  console.log(`[Thesis] Packages: ${packageDirs.length}`);
  console.log(`[Thesis] Output: ${outputDir}`);
  
  // Stage 1: Collect package docs
  console.log('[Thesis] Stage 1: Collecting package docs...');
  const packageDocs = await collectPackageDocs(packageDirs);
  
  if (emitReceipts) {
    const receipt = await emitReceipt(
      'collect',
      { packageDirs },
      { packageDocs },
      join(receiptsDir, '01-collect.json')
    );
    receipts.push(receipt);
  }
  
  // Stage 2: Generate Diataxis structure
  console.log('[Thesis] Stage 2: Generating Diataxis output...');
  const diataxisOutput = await generateDiataxisOutput(packageDocs);
  
  if (emitReceipts) {
    const receipt = await emitReceipt(
      'diataxis',
      { packageDocs },
      diataxisOutput,
      join(receiptsDir, '02-diataxis.json')
    );
    receipts.push(receipt);
  }
  
  console.log(`[Thesis] Found ${diataxisOutput.stats.totalDocs} docs across ${diataxisOutput.stats.packageCount} packages`);
  
  // Stage 3: Generate LaTeX
  console.log('[Thesis] Stage 3: Generating LaTeX...');
  const latexDir = join(outputDir, 'latex');
  const latexOutput = await generateLatex(diataxisOutput, latexDir);
  
  if (emitReceipts) {
    const receipt = await emitReceipt(
      'latex',
      { diataxisOutput },
      latexOutput,
      join(receiptsDir, '03-latex.json')
    );
    receipts.push(receipt);
  }
  
  console.log(`[Thesis] Generated ${latexOutput.chapterFiles.length} chapters`);
  console.log(`[Thesis] Main file: ${latexOutput.mainFile}`);
  
  // Stage 4: Compile to PDF (optional)
  let pdfFile;
  if (generatePDF) {
    console.log('[Thesis] Stage 4: Compiling PDF...');
    pdfFile = await compileToPDF(latexOutput.mainFile);
    
    if (emitReceipts) {
      const receipt = await emitReceipt(
        'pdf',
        { texFile: latexOutput.mainFile },
        { pdfFile },
        join(receiptsDir, '04-pdf.json')
      );
      receipts.push(receipt);
    }
    
    console.log(`[Thesis] PDF: ${pdfFile}`);
  } else {
    console.log('[Thesis] Skipping PDF compilation (set generatePDF=true to enable)');
  }
  
  const endTime = Date.now();
  const duration = endTime - startTime;
  
  console.log(`[Thesis] Build complete in ${duration}ms`);
  
  return {
    latexDir,
    mainTexFile: latexOutput.mainFile,
    pdfFile,
    receipts,
    stats: {
      duration,
      packages: diataxisOutput.stats.packageCount,
      tutorials: diataxisOutput.stats.tutorialCount,
      howtos: diataxisOutput.stats.howtoCount,
      reference: diataxisOutput.stats.referenceCount,
      explanation: diataxisOutput.stats.explanationCount,
      totalDocs: diataxisOutput.stats.totalDocs,
      merkleRoot: diataxisOutput.merkleRoot
    }
  };
}

/**
 * Render documentation from ontology via SPARQL CONSTRUCT
 * @param {string} ontologyPath - Path to .ttl ontology
 * @param {string} outputDir - Output directory for generated docs
 * @returns {Promise<Object>} Render results
 */
export async function renderFromOntology(ontologyPath, outputDir) {
  // STUB: Will be implemented with SPARQL CONSTRUCT integration
  console.warn('[Thesis] CONSTRUCT rendering not yet implemented');
  console.log(`[Thesis] Ontology: ${ontologyPath}`);
  console.log(`[Thesis] Output: ${outputDir}`);
  
  // TODO: Implement SPARQL CONSTRUCT queries to generate docs from ontology
  // const store = await loadOntology(ontologyPath);
  // const docs = await constructDocs(store);
  // await writeDocs(docs, outputDir);
  
  return {
    status: 'not_implemented',
    ontologyPath,
    outputDir
  };
}

/**
 * Export thesis to specified format
 * @param {string} thesisDir - Thesis directory
 * @param {string} format - Export format (pdf|latex|html)
 * @param {string} outputPath - Output file path
 * @returns {Promise<string>} Output file path
 */
export async function exportThesis(thesisDir, format, outputPath) {
  console.log(`[Thesis] Exporting to ${format}...`);
  
  switch (format) {
    case 'latex':
      // Already in LaTeX format, just copy
      console.log(`[Thesis] LaTeX files at: ${thesisDir}/latex`);
      return join(thesisDir, 'latex');
      
    case 'pdf':
      // Compile LaTeX to PDF
      const texFile = join(thesisDir, 'latex', 'thesis.tex');
      return await compileToPDF(texFile);
      
    case 'html':
      // STUB: HTML export not yet implemented
      console.warn('[Thesis] HTML export not yet implemented');
      return outputPath;
      
    default:
      throw new Error(`Unknown format: ${format}`);
  }
}

