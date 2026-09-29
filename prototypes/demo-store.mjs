/**
 * @file Shared demo store adapter for the prototypes
 * @description The prototypes were written against an N3-style store surface
 * (`add(s, p, o)`, `delete(s, p, o)`, `addQuad(quad)`, `removeQuad(quad)`,
 * `size` as a property, `query(sparql)` returning bindings). The workspace
 * store (`UnrdfStore` from @unrdf/core, backed by Oxigraph) takes whole quads
 * and exposes `size()` as a method, so this adapter bridges the two surfaces.
 */

import { createUnrdfStore, quad } from '@unrdf/core';

/**
 * Create a store exposing both the triple-argument and quad-argument APIs.
 *
 * @returns {{addQuad: Function, removeQuad: Function, add: Function,
 *   delete: Function, query: Function, size: number}} Adapter store
 */
export function createDemoStore() {
  const inner = createUnrdfStore();

  const store = {
    addQuad(q) {
      inner.add(q);
      return store;
    },
    removeQuad(q) {
      inner.delete(q);
      return store;
    },
    add(subject, predicate, object) {
      return store.addQuad(quad(subject, predicate, object));
    },
    delete(subject, predicate, object) {
      return store.removeQuad(quad(subject, predicate, object));
    },
    query(sparql) {
      return inner.query(sparql);
    },
    get size() {
      return inner.size();
    },
  };

  return store;
}
