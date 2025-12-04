/**
 * @file SPARQL Query Examples
 * @description Demonstrates SPARQL query execution: SELECT, ASK, CONSTRUCT
 */

import { Store } from 'n3';
import { DataFactory } from 'n3';

const { namedNode, literal, quad } = DataFactory;

/**
 * Creates a store with sample book catalog data
 * @returns {Store} Populated RDF store
 */
export function createBookCatalog() {
  const store = new Store();

  // Book 1: The RDF Book
  store.addQuad(
    namedNode('http://example.org/book1'),
    namedNode('http://purl.org/dc/elements/1.1/title'),
    literal('The RDF Book')
  );
  store.addQuad(
    namedNode('http://example.org/book1'),
    namedNode('http://purl.org/dc/elements/1.1/creator'),
    literal('Alice Smith')
  );
  store.addQuad(
    namedNode('http://example.org/book1'),
    namedNode('http://example.org/vocab/price'),
    literal('29.99', namedNode('http://www.w3.org/2001/XMLSchema#decimal'))
  );
  store.addQuad(
    namedNode('http://example.org/book1'),
    namedNode('http://example.org/vocab/inStock'),
    literal('true', namedNode('http://www.w3.org/2001/XMLSchema#boolean'))
  );

  // Book 2: SPARQL Queries
  store.addQuad(
    namedNode('http://example.org/book2'),
    namedNode('http://purl.org/dc/elements/1.1/title'),
    literal('SPARQL Queries')
  );
  store.addQuad(
    namedNode('http://example.org/book2'),
    namedNode('http://purl.org/dc/elements/1.1/creator'),
    literal('Bob Jones')
  );
  store.addQuad(
    namedNode('http://example.org/book2'),
    namedNode('http://example.org/vocab/price'),
    literal('39.99', namedNode('http://www.w3.org/2001/XMLSchema#decimal'))
  );
  store.addQuad(
    namedNode('http://example.org/book2'),
    namedNode('http://example.org/vocab/inStock'),
    literal('false', namedNode('http://www.w3.org/2001/XMLSchema#boolean'))
  );

  // Book 3: Semantic Web Primer
  store.addQuad(
    namedNode('http://example.org/book3'),
    namedNode('http://purl.org/dc/elements/1.1/title'),
    literal('Semantic Web Primer')
  );
  store.addQuad(
    namedNode('http://example.org/book3'),
    namedNode('http://purl.org/dc/elements/1.1/creator'),
    literal('Alice Smith')
  );
  store.addQuad(
    namedNode('http://example.org/book3'),
    namedNode('http://example.org/vocab/price'),
    literal('49.99', namedNode('http://www.w3.org/2001/XMLSchema#decimal'))
  );
  store.addQuad(
    namedNode('http://example.org/book3'),
    namedNode('http://example.org/vocab/inStock'),
    literal('true', namedNode('http://www.w3.org/2001/XMLSchema#boolean'))
  );

  return store;
}

/**
 * Executes a SELECT query pattern on the store
 * @param {Store} store - RDF store to query
 * @param {object} options - Query options
 * @returns {Array} Query results
 */
export function selectQuery(store, { subject, predicate, object }) {
  const quads = store.getQuads(
    subject || null,
    predicate || null,
    object || null,
    null
  );

  return quads.map(q => ({
    subject: q.subject.value,
    predicate: q.predicate.value,
    object: q.object.value,
    objectType: q.object.termType
  }));
}

/**
 * Gets all books by a specific author
 * @param {Store} store - RDF store
 * @param {string} authorName - Author name to search for
 * @returns {Array} Books by the author
 */
export function getBooksByAuthor(store, authorName) {
  const authorQuads = store.getQuads(
    null,
    namedNode('http://purl.org/dc/elements/1.1/creator'),
    literal(authorName),
    null
  );

  return authorQuads.map(q => {
    const bookUri = q.subject;
    const titleQuads = store.getQuads(
      bookUri,
      namedNode('http://purl.org/dc/elements/1.1/title'),
      null,
      null
    );

    return {
      uri: bookUri.value,
      title: titleQuads[0]?.object.value || 'Unknown'
    };
  });
}

/**
 * Checks if any books are in stock (ASK query simulation)
 * @param {Store} store - RDF store
 * @returns {boolean} True if any books are in stock
 */
export function hasBooksInStock(store) {
  const inStockQuads = store.getQuads(
    null,
    namedNode('http://example.org/vocab/inStock'),
    literal('true', namedNode('http://www.w3.org/2001/XMLSchema#boolean')),
    null
  );

  return inStockQuads.length > 0;
}

/**
 * Constructs a new graph with book summaries (CONSTRUCT query simulation)
 * @param {Store} store - RDF store
 * @returns {Store} New store with constructed triples
 */
export function constructBookSummaries(store) {
  const summaryStore = new Store();

  const allBooks = store.getQuads(
    null,
    namedNode('http://purl.org/dc/elements/1.1/title'),
    null,
    null
  );

  allBooks.forEach(titleQuad => {
    const bookUri = titleQuad.subject;
    const title = titleQuad.object;

    // Get author
    const authorQuads = store.getQuads(
      bookUri,
      namedNode('http://purl.org/dc/elements/1.1/creator'),
      null,
      null
    );

    if (authorQuads.length > 0) {
      // Add summary triple: book -> summary -> "title by author"
      const author = authorQuads[0].object.value;
      const summary = literal(`${title.value} by ${author}`);

      summaryStore.addQuad(
        bookUri,
        namedNode('http://example.org/vocab/summary'),
        summary
      );
    }
  });

  return summaryStore;
}

/**
 * Gets books within a price range
 * @param {Store} store - RDF store
 * @param {number} minPrice - Minimum price
 * @param {number} maxPrice - Maximum price
 * @returns {Array} Books in price range
 */
export function getBooksByPriceRange(store, minPrice, maxPrice) {
  const allBooks = store.getQuads(
    null,
    namedNode('http://purl.org/dc/elements/1.1/title'),
    null,
    null
  );

  return allBooks
    .map(titleQuad => {
      const bookUri = titleQuad.subject;
      const title = titleQuad.object.value;

      const priceQuads = store.getQuads(
        bookUri,
        namedNode('http://example.org/vocab/price'),
        null,
        null
      );

      if (priceQuads.length > 0) {
        const price = parseFloat(priceQuads[0].object.value);
        return { uri: bookUri.value, title, price };
      }
      return null;
    })
    .filter(book => book && book.price >= minPrice && book.price <= maxPrice);
}

/**
 * Formats query results as a table
 * @param {Array} results - Query results
 * @returns {string} Formatted table
 */
export function formatResults(results) {
  if (results.length === 0) {
    return 'No results found';
  }

  const keys = Object.keys(results[0]);
  const maxWidths = keys.map(key =>
    Math.max(key.length, ...results.map(r => String(r[key]).length))
  );

  let table = keys.map((key, i) => key.padEnd(maxWidths[i])).join(' | ') + '\n';
  table += keys.map((_, i) => '-'.repeat(maxWidths[i])).join('-|-') + '\n';

  results.forEach(result => {
    table += keys.map((key, i) => String(result[key]).padEnd(maxWidths[i])).join(' | ') + '\n';
  });

  return table;
}

// Main execution
function main() {
  console.log('=== SPARQL Query Examples ===\n');

  // Create catalog
  const store = createBookCatalog();
  console.log(`Created catalog with ${store.size} triples\n`);

  // 1. SELECT: Get all books by Alice Smith
  console.log('1. SELECT: Books by Alice Smith');
  const aliceBooks = getBooksByAuthor(store, 'Alice Smith');
  console.log(formatResults(aliceBooks));

  // 2. SELECT: Get books in price range
  console.log('2. SELECT: Books priced $30-$45');
  const affordableBooks = getBooksByPriceRange(store, 30, 45);
  console.log(formatResults(affordableBooks));

  // 3. ASK: Are any books in stock?
  console.log('3. ASK: Are any books in stock?');
  const inStock = hasBooksInStock(store);
  console.log(`   Result: ${inStock ? 'YES' : 'NO'}\n`);

  // 4. CONSTRUCT: Build book summaries
  console.log('4. CONSTRUCT: Book summaries');
  const summaries = constructBookSummaries(store);
  const summaryQuads = summaries.getQuads(null, null, null, null);
  summaryQuads.forEach(q => {
    console.log(`   ${q.object.value}`);
  });
  console.log();

  // 5. Complex query: In-stock books by price
  console.log('5. Complex: In-stock books sorted by price');
  const inStockBooks = allBooks
    .filter(book => {
      const stockQuads = store.getQuads(
        namedNode(book.uri),
        namedNode('http://example.org/vocab/inStock'),
        null,
        null
      );
      return stockQuads[0]?.object.value === 'true';
    })
    .sort((a, b) => a.price - b.price);

  console.log(formatResults(inStockBooks));
}

// Helper for complex query
function getAllBooks(store) {
  const titleQuads = store.getQuads(
    null,
    namedNode('http://purl.org/dc/elements/1.1/title'),
    null,
    null
  );

  return titleQuads.map(q => {
    const bookUri = q.subject;
    const priceQuads = store.getQuads(
      bookUri,
      namedNode('http://example.org/vocab/price'),
      null,
      null
    );

    return {
      uri: bookUri.value,
      title: q.object.value,
      price: parseFloat(priceQuads[0]?.object.value || '0')
    };
  });
}

const allBooks = getAllBooks;

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main();
}
