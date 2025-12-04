/**
 * @vitest-environment node
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  createBookCatalog,
  selectQuery,
  getBooksByAuthor,
  hasBooksInStock,
  constructBookSummaries,
  getBooksByPriceRange,
  formatResults
} from '../src/index.mjs';
import { createStore, dataFactory } from '@unrdf/oxigraph';

const { namedNode, literal } = dataFactory;

describe('SPARQL Query Examples', () => {
  it('creates book catalog with sample data', () => {
    const store = createBookCatalog();
    expect(store.size).toBe(12); // 3 books × 4 properties each
  });

  it('executes SELECT query pattern', () => {
    const store = createBookCatalog();
    const results = selectQuery(store, {
      predicate: namedNode('http://purl.org/dc/elements/1.1/title')
    });
    expect(results).toHaveLength(3);
  });

  it('gets books by specific author', () => {
    const store = createBookCatalog();
    const aliceBooks = getBooksByAuthor(store, 'Alice Smith');
    expect(aliceBooks).toHaveLength(2);
    expect(aliceBooks[0].title).toBeTruthy();
  });

  it('checks if books are in stock (ASK)', () => {
    const store = createBookCatalog();
    const hasStock = hasBooksInStock(store);
    expect(hasStock).toBe(true);
  });

  it('constructs book summaries (CONSTRUCT)', () => {
    const store = createBookCatalog();
    const summaries = constructBookSummaries(store);
    expect(summaries.size).toBe(3);

    const summaryQuads = summaries.getQuads(null, null, null, null);
    expect(summaryQuads[0].object.value).toContain(' by ');
  });

  it('filters books by price range', () => {
    const store = createBookCatalog();
    const books = getBooksByPriceRange(store, 30, 45);
    expect(books.length).toBeGreaterThan(0);
    books.forEach(book => {
      expect(book.price).toBeGreaterThanOrEqual(30);
      expect(book.price).toBeLessThanOrEqual(45);
    });
  });

  it('formats results as table', () => {
    const results = [
      { title: 'Book 1', author: 'Alice' },
      { title: 'Book 2', author: 'Bob' }
    ];
    const table = formatResults(results);
    expect(table).toContain('title');
    expect(table).toContain('author');
    expect(table).toContain('Book 1');
  });

  it('handles empty results gracefully', () => {
    const table = formatResults([]);
    expect(table).toBe('No results found');
  });

  it('gets books by non-existent author', () => {
    const store = createBookCatalog();
    const books = getBooksByAuthor(store, 'Unknown Author');
    expect(books).toHaveLength(0);
  });

  it('filters books with no matching price range', () => {
    const store = createBookCatalog();
    const books = getBooksByPriceRange(store, 1000, 2000);
    expect(books).toHaveLength(0);
  });

  it('selects by subject pattern', () => {
    const store = createBookCatalog();
    const results = selectQuery(store, {
      subject: namedNode('http://example.org/book1')
    });
    expect(results).toHaveLength(4);
  });

  it('constructs summaries for all books', () => {
    const store = createBookCatalog();
    const summaries = constructBookSummaries(store);
    const summaryQuads = summaries.getQuads(null, null, null, null);
    expect(summaryQuads).toHaveLength(3);
    summaryQuads.forEach(quad => {
      expect(quad.predicate.value).toBe('http://example.org/vocab/summary');
    });
  });

  it('checks stock status when no books in stock', () => {
    const store = createStore();
    store.addQuad(
      namedNode('http://example.org/book99'),
      namedNode('http://example.org/vocab/inStock'),
      literal('false', namedNode('http://www.w3.org/2001/XMLSchema#boolean'))
    );
    const hasStock = hasBooksInStock(store);
    expect(hasStock).toBe(false);
  });

  it('formats multi-column results correctly', () => {
    const results = [
      { title: 'Book A', author: 'Alice', price: 29.99 },
      { title: 'Book B', author: 'Bob', price: 39.99 }
    ];
    const table = formatResults(results);
    expect(table).toContain('price');
    expect(table).toContain('29.99');
    expect(table).toContain('39.99');
  });
});

describe('Complex Query Patterns', () => {
  it('filters books at exact price boundary', () => {
    const store = createBookCatalog();
    const books = getBooksByPriceRange(store, 29.99, 29.99);
    expect(books).toHaveLength(1);
    expect(books[0].price).toBe(29.99);
  });

  it('returns all books with wide price range', () => {
    const store = createBookCatalog();
    const books = getBooksByPriceRange(store, 0, 1000);
    expect(books).toHaveLength(3);
  });

  it('handles books without price information', () => {
    const store = createStore();
    store.addQuad(
      namedNode('http://example.org/book99'),
      namedNode('http://purl.org/dc/elements/1.1/title'),
      literal('No Price Book')
    );
    const books = getBooksByPriceRange(store, 0, 100);
    expect(books).toHaveLength(0);
  });

  it('selects by object pattern', () => {
    const store = createBookCatalog();
    const results = selectQuery(store, {
      object: literal('Alice Smith')
    });
    expect(results).toHaveLength(2);
  });

  it('formats single-row results', () => {
    const results = [{ title: 'Solo Book' }];
    const table = formatResults(results);
    expect(table).toContain('Solo Book');
  });
});
