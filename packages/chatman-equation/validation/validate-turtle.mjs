/**
 * @file Turtle RDF Validation
 * @module @unrdf/chatman-equation/validation/validate-turtle
 * @description Validates all Turtle files are well-formed and load into RDF store
 */

import { readFile, readdir } from 'fs/promises';
import { join, extname } from 'path';
import { Parser, Store } from 'n3';

/**
 * Find all Turtle files in a directory recursively
 * @param {string} dir - Directory to search
 * @returns {Promise<string[]>} Array of Turtle file paths
 */
async function findTurtleFiles(dir) {
  const files = [];
  const entries = await readdir(dir, { withFileTypes: true });

  for (const entry of entries) {
    const fullPath = join(dir, entry.name);
    if (entry.isDirectory()) {
      files.push(...await findTurtleFiles(fullPath));
    } else if (entry.isFile() && (extname(entry.name) === '.ttl' || extname(entry.name) === '.turtle')) {
      files.push(fullPath);
    }
  }

  return files;
}

/**
 * Validate a single Turtle file
 * @param {string} filePath - Path to Turtle file
 * @returns {Promise<{valid: boolean, errors: string[], triples: number, store: Store}>}
 */
async function validateTurtleFile(filePath) {
  try {
    const content = await readFile(filePath, 'utf-8');
    const parser = new Parser({ format: 'text/turtle' });
    const store = new Store();

    return new Promise((resolve) => {
      const quads = [];
      const errors = [];

      parser.parse(content, (error, quad, prefixes) => {
        if (error) {
          errors.push(error.message);
          return;
        }

        if (quad) {
          quads.push(quad);
          store.add(quad);
        } else {
          // Parsing complete
          if (errors.length > 0) {
            resolve({
              valid: false,
              errors,
              triples: 0,
              store: null,
            });
          } else {
            resolve({
              valid: true,
              errors: [],
              triples: quads.length,
              store,
            });
          }
        }
      });
    });
  } catch (error) {
    return {
      valid: false,
      errors: [`Parse error: ${error.message}`],
      triples: 0,
      store: null,
    };
  }
}

/**
 * Validate RDF semantics and basic integrity
 * @param {Store} store - N3 Store containing parsed triples
 * @returns {{valid: boolean, errors: string[], warnings: string[]}}
 */
function validateRDFSemantics(store) {
  const errors = [];
  const warnings = [];

  // Check for blank node subjects (valid but may be unintended)
  const blankNodeSubjects = store.getSubjects(null, null, null).filter(s => s.termType === 'BlankNode');
  if (blankNodeSubjects.length > 0) {
    warnings.push(`Found ${blankNodeSubjects.length} blank node subject(s) - ensure this is intentional`);
  }

  // Check for literal subjects (invalid in RDF)
  const literalSubjects = store.getSubjects(null, null, null).filter(s => s.termType === 'Literal');
  if (literalSubjects.length > 0) {
    errors.push(`Invalid: Found ${literalSubjects.length} literal subject(s) - literals cannot be subjects in RDF`);
  }

  // Check for undefined predicates (all predicates should be URIs)
  const predicates = store.getPredicates(null, null, null);
  const invalidPredicates = predicates.filter(p => p.termType !== 'NamedNode');
  if (invalidPredicates.length > 0) {
    errors.push(`Invalid: Found ${invalidPredicates.length} non-URI predicate(s) - predicates must be URIs`);
  }

  return {
    valid: errors.length === 0,
    errors,
    warnings,
  };
}

/**
 * Main validation function
 * @returns {Promise<{success: boolean, results: Array}>}
 */
async function validateAllTurtle() {
  // Resolve package root - works whether run from root or package dir
  const packageRoot = process.cwd().endsWith('chatman-equation')
    ? process.cwd()
    : join(process.cwd(), 'packages/chatman-equation');
  const turtleFiles = await findTurtleFiles(packageRoot);

  console.log(`\n🐢 Turtle RDF Validation Report`);
  console.log(`${'='.repeat(60)}\n`);
  console.log(`Found ${turtleFiles.length} Turtle file(s)\n`);

  const results = [];
  let allValid = true;
  let totalTriples = 0;

  for (const file of turtleFiles) {
    const result = await validateTurtleFile(file);
    const relativePath = file.replace(packageRoot + '/', '');

    // Additional semantic validation if parsing succeeded
    let semanticResult = { valid: true, errors: [], warnings: [] };
    if (result.valid && result.store) {
      semanticResult = validateRDFSemantics(result.store);
    }

    const combinedResult = {
      file: relativePath,
      valid: result.valid && semanticResult.valid,
      errors: [...result.errors, ...semanticResult.errors],
      warnings: semanticResult.warnings,
      triples: result.triples,
    };

    results.push(combinedResult);
    totalTriples += result.triples;

    if (combinedResult.valid) {
      console.log(`✅ ${relativePath} (${result.triples} triple(s))`);
      if (combinedResult.warnings.length > 0) {
        combinedResult.warnings.forEach(warn => console.log(`   ⚠️  ${warn}`));
      }
    } else {
      console.log(`❌ ${relativePath}`);
      combinedResult.errors.forEach(err => console.log(`   - ${err}`));
      allValid = false;
    }
  }

  console.log(`\n${'='.repeat(60)}`);
  console.log(`\n📊 Summary:`);
  console.log(`   Total files: ${turtleFiles.length}`);
  console.log(`   Valid: ${results.filter(r => r.valid).length}`);
  console.log(`   Invalid: ${results.filter(r => !r.valid).length}`);
  console.log(`   Total triples: ${totalTriples}`);
  console.log(`   Status: ${allValid ? '✅ ALL PASS' : '❌ FAILURES DETECTED'}\n`);

  return {
    success: allValid,
    results,
    stats: {
      total: turtleFiles.length,
      valid: results.filter(r => r.valid).length,
      invalid: results.filter(r => !r.valid).length,
      totalTriples,
    },
  };
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  const result = await validateAllTurtle();
  process.exit(result.success ? 0 : 1);
}

export { validateAllTurtle, validateTurtleFile, validateRDFSemantics };
