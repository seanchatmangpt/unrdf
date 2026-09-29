/**
 * KGC Markdown Renderer - Usage Example
 *
 * Demonstrates how to use the KGC markdown renderer to create
 * deterministic, verifiable documentation with embedded receipts.
 *
 * @example
 * node packages/fusion/examples/kgc-markdown-renderer-example.mjs
 */

import { createReceipt, chainReceipts } from '../src/receipts-kernel.mjs';
import {
  renderKGCDocument,
  renderSection,
  renderTable,
  renderCodeBlock,
  hashMarkdown,
} from '../src/kgc-markdown-renderer.mjs';

// Enable deterministic mode for reproducible output
process.env.DETERMINISTIC = '1';

console.log('=== KGC Markdown Renderer Example ===\n');

// 1. Create receipts for document provenance
console.log('1. Creating document receipts...');
const receipt1 = await createReceipt('document-created', {
  author: 'Example Author',
  title: 'Getting Started with UNRDF',
});

const receipt2 = await createReceipt('section-added', {
  section: 'Introduction',
  wordCount: 150,
}, { chain: receipt1.hash });

const receipt3 = await createReceipt('review-approved', {
  reviewer: 'Technical Reviewer',
  status: 'approved',
}, { chain: receipt2.hash });

console.log(`   Created ${[receipt1, receipt2, receipt3].length} receipts\n`);

// 2. Define document frontmatter
const frontmatter = {
  title: 'Getting Started with UNRDF',
  date: '2024-01-01',
  author: 'Example Author',
  version: '1.0.0',
  tags: ['tutorial', 'unrdf', 'rdf', 'knowledge-graph'],
};

// 3. Define sections (Diataxis framework)
const sections = [
  {
    type: 'tutorial',
    title: 'Introduction',
    level: 2,
    content: `
This tutorial will guide you through the basics of using UNRDF for RDF knowledge graph management.

UNRDF combines several powerful technologies:
- RDF triple stores with Oxigraph
- Time-travel and versioning with KGC-4D
- Blockchain receipts and verification
- Policy hooks and validation
`.trim(),
  },
  {
    type: 'how-to',
    title: 'Quick Start',
    level: 2,
    content: 'Follow these steps to get started quickly:',
    blocks: [
      {
        type: 'kgc:query',
        content: `
PREFIX ex: <http://example.org/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

SELECT ?person ?name
WHERE {
  ?person rdf:type ex:Person .
  ?person ex:name ?name .
}
        `.trim(),
        result: [
          { person: 'ex:alice', name: 'Alice Smith' },
          { person: 'ex:bob', name: 'Bob Jones' },
        ],
      },
    ],
  },
  {
    type: 'reference',
    title: 'API Reference',
    level: 2,
    content: 'Core API functions:',
    blocks: [
      {
        type: 'kgc:extract',
        content: 'extract-api',
        result: [
          {
            name: 'createStore',
            type: 'function',
            description: 'Creates a new Oxigraph RDF store',
          },
          {
            name: 'dataFactory',
            type: 'object',
            description: 'RDF data factory for creating quads',
          },
          {
            name: 'KGCStore',
            type: 'class',
            description: 'Time-travel enabled KGC store',
          },
        ],
      },
    ],
    links: [
      { text: 'UNRDF GitHub', url: 'https://github.com/seanchatmangpt/unrdf' },
      { text: 'RDF Specification', url: 'https://www.w3.org/RDF/' },
      { text: 'Oxigraph', url: 'https://github.com/oxigraph/oxigraph' },
    ],
  },
  {
    type: 'explanation',
    title: 'Architecture',
    level: 2,
    content: `
The UNRDF architecture is built on three layers:

1. **Storage Layer**: Oxigraph provides fast RDF triple storage
2. **Versioning Layer**: KGC-4D adds time-travel and Git integration
3. **Verification Layer**: Blockchain receipts ensure provenance

This design enables both performance and auditability.
`.trim(),
  },
];

// 4. Render the complete document
console.log('2. Rendering KGC document...');
const document = renderKGCDocument(frontmatter, sections, [receipt1, receipt2, receipt3]);

// 5. Compute document hash for verification
const docHash = hashMarkdown(document);
console.log(`   Document hash: ${docHash.slice(0, 16)}...\n`);

// 6. Verify determinism
console.log('3. Verifying determinism...');
const doc2 = renderKGCDocument(frontmatter, sections, [receipt1, receipt2, receipt3]);
const hash2 = hashMarkdown(doc2);
console.log(`   Hash match: ${docHash === hash2 ? '✓ YES' : '✗ NO'}\n`);

// 7. Display document stats
console.log('4. Document statistics:');
console.log(`   Total length: ${document.length} characters`);
console.log(`   Lines: ${document.split('\n').length}`);
console.log(`   Sections: ${sections.length}`);
console.log(`   Receipts: ${[receipt1, receipt2, receipt3].length}\n`);

// 8. Save document
import { writeFile } from 'node:fs/promises';
const outputPath = '/tmp/kgc-example-document.md';
await writeFile(outputPath, document);
console.log(`5. Document saved to: ${outputPath}\n`);

// 9. Display sample output
console.log('=== Document Preview (first 800 chars) ===\n');
console.log(document.slice(0, 800));
console.log('\n... (truncated) ...\n');

// 10. Demonstrate individual renderers
console.log('=== Individual Renderer Examples ===\n');

// Table example
console.log('Table rendering:');
const tableData = [
  { name: 'Alice', role: 'Developer', years: 5 },
  { name: 'Bob', role: 'Designer', years: 3 },
  { name: 'Charlie', role: 'Manager', years: 8 },
];
const columns = [
  { key: 'name', header: 'Name', align: 'left' },
  { key: 'role', header: 'Role', align: 'left' },
  { key: 'years', header: 'Years', align: 'right' },
];
const table = renderTable(tableData, columns, 'name');
console.log(table);
console.log('');

// Code block example
console.log('Code block rendering:');
const code = renderCodeBlock(
  'import { createStore } from "@unrdf/fusion";\n\nconst store = createStore();',
  'javascript',
  { lineNumbers: true, startLine: 1 }
);
console.log(code);
console.log('');

// Section example
console.log('Section rendering:');
const exampleSection = renderSection({
  type: 'tutorial',
  title: 'Example Section',
  level: 3,
  content: 'This is an example section.',
});
console.log(exampleSection.slice(0, 200));
console.log('');

console.log('=== Example Complete ===');
