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
export function buildThesis(config: ThesisConfig): Promise<ThesisOutput>;
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
export function executeSparqlConstruct(store: any, query: string, options?: {
    prefixes?: any;
    outputFormat?: string;
}): Promise<string>;
/**
 * Generate documentation from ontology classes
 * @param {Object} store - RDF store with ontology
 * @param {string} classUri - Class URI to generate docs for
 * @param {string} docType - Documentation type (tutorial|howto|reference|explanation)
 * @returns {Promise<Object>} Generated documentation
 * @example
 * const doc = await generateDocFromClass(store, 'http://example.org/Class', 'reference');
 */
export function generateDocFromClass(store: any, classUri: string, docType: string): Promise<any>;
/**
 * Load ontology into RDF store
 * @param {string} ontologyPath - Path to ontology file (.ttl, .rdf, .owl, .nt)
 * @returns {Promise<Object>} RDF store with loaded ontology
 * @throws {Error} If file cannot be loaded or parsed
 * @example
 * const store = await loadOntology('./schema.ttl');
 * console.log('Loaded', store.size, 'triples');
 */
export function loadOntology(ontologyPath: string): Promise<any>;
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
export function renderFromOntology(ontologyPath: string, outputDir: string, config?: {
    queries?: any[];
    templateDir?: string;
    generateDiataxis?: boolean;
}): Promise<any>;
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
export function applyDocTemplate(template: any, rdfData: any): Promise<string>;
/**
 * Export thesis to specified format
 * @param {string} thesisDir - Thesis directory
 * @param {string} format - Export format (pdf|latex|html)
 * @param {string} outputPath - Output file path
 * @returns {Promise<string>} Output file path
 */
export function exportThesis(thesisDir: string, format: string, outputPath: string): Promise<string>;
export type ThesisConfig = {
    /**
     * - Package directories to include
     */
    packageDirs: string[];
    /**
     * - Output directory for thesis
     */
    outputDir: string;
    /**
     * - Whether to compile PDF
     */
    generatePDF?: boolean;
    /**
     * - Whether to emit receipts
     */
    emitReceipts?: boolean;
    /**
     * - Directory for receipts (default: outputDir/receipts)
     */
    receiptsDir?: string;
};
export type ThesisOutput = {
    /**
     * - LaTeX output directory
     */
    latexDir: string;
    /**
     * - Main .tex file path
     */
    mainTexFile: string;
    /**
     * - PDF file path (if generated)
     */
    pdfFile?: string;
    /**
     * - Array of receipts from each stage
     */
    receipts: any[];
    /**
     * - Build statistics
     */
    stats: any;
};
