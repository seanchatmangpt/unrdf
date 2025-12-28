/**
 * Thesis Builder - Full pipeline from packages to PDF
 * @module @unrdf/v6-core/docs/thesis-builder
 */

import { mkdir, readFile, writeFile } from 'node:fs/promises';
import { join } from 'node:path';
import { collectPackageDocs, generateDiataxisOutput, emitReceipt } from './pipeline.mjs';
import { generateLatex, compileToPDF } from './latex-generator.mjs';
import {
  SparqlConstructQuerySchema,
  OntologyRenderConfigSchema,
  OntologyRenderResultSchema,
  DocTemplateSchema
} from './thesis-builder.schema.mjs';
import { createStore } from '@unrdf/oxigraph';
import * as TemplateRenderer from './template-renderer.mjs';

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
 * Execute SPARQL CONSTRUCT query
 * @param {Object} store - RDF store instance (OxigraphStore)
 * @param {string} query - SPARQL CONSTRUCT query
 * @param {Object} [options] - Query options
 * @param {Object} [options.prefixes={}] - Namespace prefixes
 * @param {string} [options.outputFormat='turtle'] - Output format (turtle, ntriples, rdfxml)
 * @returns {Promise<string>} Constructed RDF triples
 * @throws {Error} If validation fails or query execution fails
 * @example
 * const store = createStore();
 * const triples = await executeSparqlConstruct(store, 'CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }');
 */
export async function executeSparqlConstruct(store, query, options = {}) {
  // Validate input
  const validated = SparqlConstructQuerySchema.parse({
    query,
    prefixes: options.prefixes || {},
    outputFormat: options.outputFormat || 'turtle'
  });

  try {
    // Build full query with prefixes
    const prefixLines = Object.entries(validated.prefixes)
      .map(([prefix, uri]) => `PREFIX ${prefix}: <${uri}>`)
      .join('\n');

    const fullQuery = prefixLines ? `${prefixLines}\n${validated.query}` : validated.query;

    // Execute CONSTRUCT query via Oxigraph
    // Oxigraph returns an array of quads for CONSTRUCT queries
    const results = store.query(fullQuery);

    // Convert quads array to serialized RDF format
    if (!results || (Array.isArray(results) && results.length === 0)) {
      return ''; // Empty result
    }

    // Use Oxigraph's dump method to serialize
    // Create temporary store with results and dump
    const resultStore = createStore();
    if (Array.isArray(results)) {
      for (const quad of results) {
        resultStore.add(quad);
      }
    }

    // Map output format to Oxigraph format string
    const formatMap = {
      'turtle': 'text/turtle',
      'ntriples': 'application/n-triples',
      'rdfxml': 'application/rdf+xml',
      'nquads': 'application/n-quads'
    };

    const mimeType = formatMap[validated.outputFormat] || 'text/turtle';
    const serialized = resultStore.dump({ format: mimeType });

    return serialized;
  } catch (error) {
    throw new Error(`SPARQL CONSTRUCT execution failed: ${error.message}`);
  }
}

/**
 * Generate documentation from ontology classes
 * @param {Object} store - RDF store with ontology
 * @param {string} classUri - Class URI to generate docs for
 * @param {string} docType - Documentation type (tutorial|howto|reference|explanation)
 * @returns {Promise<Object>} Generated documentation
 * @example
 * const doc = await generateDocFromClass(store, 'http://example.org/Class', 'reference');
 */
export async function generateDocFromClass(store, classUri, docType) {
  try {
    // Query for class metadata
    const metadataQuery = `
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      PREFIX owl: <http://www.w3.org/2002/07/owl#>

      SELECT ?label ?comment ?superClass
      WHERE {
        <${classUri}> rdfs:label ?label .
        OPTIONAL { <${classUri}> rdfs:comment ?comment }
        OPTIONAL { <${classUri}> rdfs:subClassOf ?superClass }
      }
    `;

    const metadata = store.query(metadataQuery);

    // Query for class properties
    const propertiesQuery = `
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

      SELECT ?property ?propLabel ?propComment ?range
      WHERE {
        ?property rdfs:domain <${classUri}> .
        OPTIONAL { ?property rdfs:label ?propLabel }
        OPTIONAL { ?property rdfs:comment ?propComment }
        OPTIONAL { ?property rdfs:range ?range }
      }
    `;

    const properties = store.query(propertiesQuery);

    // Extract values from query results
    const label = metadata[0]?.get('label')?.value || classUri.split('/').pop();
    const comment = metadata[0]?.get('comment')?.value || 'No description available';
    const superClasses = metadata
      .filter(row => row.get('superClass'))
      .map(row => row.get('superClass').value);

    // Build markdown content
    let content = `# ${label}\n\n`;
    content += `**URI**: \`${classUri}\`\n\n`;
    content += `## Description\n\n${comment}\n\n`;

    if (superClasses.length > 0) {
      content += `## Superclasses\n\n`;
      superClasses.forEach(sc => {
        content += `- \`${sc}\`\n`;
      });
      content += '\n';
    }

    if (properties && properties.length > 0) {
      content += `## Properties\n\n`;
      properties.forEach(prop => {
        const propUri = prop.get('property')?.value;
        const propLabel = prop.get('propLabel')?.value || propUri?.split('/').pop();
        const propComment = prop.get('propComment')?.value || '';
        const range = prop.get('range')?.value;

        content += `### ${propLabel}\n\n`;
        if (propComment) content += `${propComment}\n\n`;
        content += `- **URI**: \`${propUri}\`\n`;
        if (range) content += `- **Range**: \`${range}\`\n`;
        content += '\n';
      });
    }

    return {
      path: `${docType}/${classUri.split('/').pop()}.md`,
      type: docType,
      content,
      metadata: {
        classUri,
        label,
        comment,
        superClasses,
        propertyCount: properties?.length || 0
      }
    };
  } catch (error) {
    throw new Error(`Failed to generate documentation for class ${classUri}: ${error.message}`);
  }
}

/**
 * Load ontology into RDF store
 * @param {string} ontologyPath - Path to ontology file (.ttl, .rdf, .owl, .nt)
 * @returns {Promise<Object>} RDF store with loaded ontology
 * @throws {Error} If file cannot be loaded or parsed
 * @example
 * const store = await loadOntology('./schema.ttl');
 * console.log('Loaded', store.size, 'triples');
 */
export async function loadOntology(ontologyPath) {
  try {
    // Read ontology file
    const data = await readFile(ontologyPath, 'utf-8');

    // Determine format from file extension
    const ext = ontologyPath.split('.').pop().toLowerCase();
    const formatMap = {
      'ttl': 'text/turtle',
      'turtle': 'text/turtle',
      'rdf': 'application/rdf+xml',
      'owl': 'application/rdf+xml',
      'nt': 'application/n-triples',
      'ntriples': 'application/n-triples',
      'nq': 'application/n-quads',
      'nquads': 'application/n-quads',
      'trig': 'application/trig'
    };

    const format = formatMap[ext];
    if (!format) {
      throw new Error(`Unsupported ontology format: .${ext}. Supported: ${Object.keys(formatMap).join(', ')}`);
    }

    // Create Oxigraph store and load data
    const store = createStore();
    store.load(data, { format });

    // Add query method wrapper for convenience
    return store;
  } catch (error) {
    throw new Error(`Failed to load ontology from ${ontologyPath}: ${error.message}`);
  }
}

/**
 * Render documentation from ontology via SPARQL CONSTRUCT
 * @param {string} ontologyPath - Path to .ttl ontology
 * @param {string} outputDir - Output directory for generated docs
 * @param {Object} [config] - Rendering configuration
 * @param {Array} [config.queries] - Custom SPARQL CONSTRUCT queries
 * @param {string} [config.templateDir] - Documentation templates directory
 * @param {boolean} [config.generateDiataxis=true] - Generate Diataxis structure
 * @returns {Promise<Object>} Render results
 * @throws {Error} If validation fails
 * @example
 * const result = await renderFromOntology('./schema.ttl', './docs', {
 *   queries: [{ query: 'CONSTRUCT { ?s ?p ?o } WHERE { ?s a owl:Class }' }]
 * });
 */
export async function renderFromOntology(ontologyPath, outputDir, config = {}) {
  // Validate configuration
  const validated = OntologyRenderConfigSchema.parse({
    ontologyPath,
    outputDir,
    ...config
  });

  await mkdir(outputDir, { recursive: true });

  const generatedDocs = [];
  const errors = [];
  const warnings = [];

  try {
    // 1. Load ontology
    const store = await loadOntology(validated.ontologyPath);

    // 2. Find all classes in ontology
    const classQuery = `
      PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      PREFIX owl: <http://www.w3.org/2002/07/owl#>

      SELECT DISTINCT ?class
      WHERE {
        { ?class rdf:type owl:Class }
        UNION
        { ?class rdf:type rdfs:Class }
      }
    `;

    const classes = store.query(classQuery);

    // 3. Generate docs for each class
    for (const row of classes) {
      const classUri = row.get('class')?.value;
      if (!classUri) continue;

      try {
        // Determine doc type based on Diataxis structure
        const docType = validated.generateDiataxis ? 'reference' : 'docs';
        const doc = await generateDocFromClass(store, classUri, docType);

        // Write markdown file
        const docPath = join(outputDir, doc.path);
        await mkdir(join(outputDir, docType), { recursive: true });
        await writeFile(docPath, doc.content, 'utf-8');

        generatedDocs.push({
          path: doc.path,
          classUri,
          type: docType,
          size: doc.content.length
        });
      } catch (error) {
        errors.push({
          classUri,
          error: error.message
        });
      }
    }

    // 4. Execute custom CONSTRUCT queries if provided
    if (validated.queries && validated.queries.length > 0) {
      for (const queryConfig of validated.queries) {
        try {
          const result = await executeSparqlConstruct(store, queryConfig.query, {
            outputFormat: queryConfig.outputFormat || 'turtle'
          });

          const filename = queryConfig.name || `construct-${generatedDocs.length}.ttl`;
          const filePath = join(outputDir, filename);
          await writeFile(filePath, result, 'utf-8');

          generatedDocs.push({
            path: filename,
            type: 'construct-result',
            size: result.length
          });
        } catch (error) {
          errors.push({
            query: queryConfig.query.substring(0, 100),
            error: error.message
          });
        }
      }
    }

    const result = {
      status: 'success',
      ontologyPath: validated.ontologyPath,
      outputDir: validated.outputDir,
      generatedDocs,
      errors,
      warnings,
      stats: {
        totalDocs: generatedDocs.length,
        totalErrors: errors.length,
        totalClasses: classes.length
      }
    };

    return OntologyRenderResultSchema.parse(result);
  } catch (error) {
    errors.push({
      stage: 'initialization',
      error: error.message
    });

    const result = {
      status: 'error',
      ontologyPath: validated.ontologyPath,
      outputDir: validated.outputDir,
      generatedDocs,
      errors,
      warnings
    };

    return OntologyRenderResultSchema.parse(result);
  }
}

/**
 * Apply documentation template to RDF data
 * @param {Object} template - Documentation template
 * @param {Object} rdfData - RDF data from CONSTRUCT query or SPARQL results
 * @returns {Promise<string>} Generated markdown content
 * @example
 * const markdown = await applyDocTemplate({
 *   type: 'reference',
 *   name: 'MyClass',
 *   template: '# {{title}}\n{{description}}'
 * }, { title: 'Example', description: 'An example class' });
 */
export async function applyDocTemplate(template, rdfData) {
  const validated = DocTemplateSchema.parse(template);

  try {
    // Prepare template data from RDF bindings
    const templateData = {
      type: validated.type,
      name: validated.name,
      generated: true,
      timestamp: new Date().toISOString(),
      ...rdfData
    };

    // Use template string or load from file
    let templateString = validated.template;

    // If template is a path, load it
    if (validated.template && validated.template.endsWith('.md')) {
      try {
        templateString = await readFile(validated.template, 'utf-8');
      } catch (error) {
        // Fall back to default template if file not found
        templateString = getDefaultTemplate(validated.type);
      }
    } else if (!templateString) {
      // Use default template for type
      templateString = getDefaultTemplate(validated.type);
    }

    // Render template with Mustache
    const renderedContent = TemplateRenderer.render(templateString, templateData);

    // Add frontmatter if not already present
    if (!renderedContent.startsWith('---')) {
      const frontmatter = `---
type: ${validated.type}
name: ${validated.name}
generated: true
timestamp: ${templateData.timestamp}
---

`;
      return frontmatter + renderedContent;
    }

    return renderedContent;
  } catch (error) {
    throw new Error(`Failed to apply template: ${error.message}`);
  }
}

/**
 * Get default template for doc type
 * @param {string} type - Documentation type
 * @returns {string} Default template string
 */
function getDefaultTemplate(type) {
  const templates = {
    reference: `# {{name}}

{{#uri}}**URI**: \`{{uri}}\`{{/uri}}

## Description

{{description}}

{{#properties}}
### Properties

{{#properties}}
- **{{name}}**: {{description}}
{{/properties}}
{{/properties}}`,

    tutorial: `# {{name}}

{{description}}

## Prerequisites

{{#prerequisites}}
- {{.}}
{{/prerequisites}}

## Steps

{{#steps}}
{{step}}. {{description}}
{{/steps}}`,

    howto: `# How to {{name}}

{{description}}

## Steps

{{#steps}}
{{step}}. {{description}}
{{/steps}}`,

    explanation: `# {{name}}

{{description}}

{{#sections}}
## {{title}}

{{content}}
{{/sections}}`
  };

  return templates[type] || `# {{name}}\n\n{{description}}`;
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
      // Generate basic HTML export (placeholder for full HTML rendering)
      console.log('[Thesis] Generating basic HTML export...');
      return outputPath;
      
    default:
      throw new Error(`Unknown format: ${format}`);
  }
}

