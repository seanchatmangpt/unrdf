/**
 * @fileoverview useLists composable - RDF list operations with context
 * 
 * This composable provides RDF list (rdf:List) operations for reading
 * and writing linked lists in RDF graphs. Now uses unctx for store access.
 * 
 * @version 1.0.0
 * @author GitVan Team
 * @license MIT
 */

import { Store, DataFactory } from "n3";
import { useStoreContext } from "../context/index.mjs";

const { namedNode, blankNode, quad } = DataFactory;

/**
 * Create a lists composable for RDF list operations
 * 
 * @param {Object} [options] - List options
 * @param {string} [options.baseIRI] - Base IRI for new lists
 * @returns {Object} List operations interface
 * 
 * @example
 * // Initialize store context first
 * const runApp = initStore();
 * 
 * runApp(() => {
 *   const lists = useLists();
 *   
 *   // Read a list
 *   const items = lists.read(namedNode('ex:myList'));
 *   
 *   // Write a list
 *   const listHead = lists.write(['item1', 'item2', 'item3']);
 *   
 *   // Check if a node is a list
 *   const isList = lists.isList(namedNode('ex:myList'));
 * });
 * 
 * @throws {Error} If store context is not initialized
 */
export function useLists(options = {}) {
  // Get the store from context
  const storeContext = useStoreContext();
  const store = storeContext.store;

  const { baseIRI = "http://example.org/" } = options;

  return {
    /**
     * Read an RDF list starting from the given head node
     * @param {Term} head - The head node of the list
     * @returns {Array} Array of list items
     */
    read(head) {
      if (!head) {
        throw new Error("[useLists] Head node is required");
      }

      const items = [];
      let current = head;

      while (current) {
        // Check if current node is rdf:nil (end of list)
        if (current.equals(namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"))) {
          break;
        }

        // Get the first item (rdf:first)
        const firstQuads = store.getQuads(current, namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#first"), null, null);
        if (firstQuads.length > 0) {
          items.push(firstQuads[0].object);
        }

        // Get the rest of the list (rdf:rest)
        const restQuads = store.getQuads(current, namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"), null, null);
        if (restQuads.length > 0) {
          current = restQuads[0].object;
        } else {
          break;
        }
      }

      return items;
    },

    /**
     * Write an array of items as an RDF list
     * @param {Array} items - Array of items to write
     * @param {string} [listId] - Optional ID for the list head
     * @returns {Term} The head node of the created list
     */
    write(items, listId) {
      if (!Array.isArray(items)) {
        throw new TypeError("[useLists] Items must be an array");
      }

      if (items.length === 0) {
        // Empty list is just rdf:nil
        return namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil");
      }

      // Create head node
      const head = listId ? namedNode(baseIRI + listId) : blankNode();

      // Create list nodes
      const nodes = [head];
      for (let i = 1; i < items.length; i++) {
        nodes.push(blankNode());
      }

      // Add rdf:first and rdf:rest triples
      for (let i = 0; i < items.length; i++) {
        const currentNode = nodes[i];
        const item = items[i];

        // Add rdf:first
        store.add(quad(
          currentNode,
          namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#first"),
          item
        ));

        // Add rdf:rest
        if (i < items.length - 1) {
          store.add(quad(
            currentNode,
            namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"),
            nodes[i + 1]
          ));
        } else {
          // Last node points to rdf:nil
          store.add(quad(
            currentNode,
            namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"),
            namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil")
          ));
        }
      }

      return head;
    },

    /**
     * Check if a node is the head of an RDF list
     * @param {Term} node - The node to check
     * @returns {boolean} True if the node is a list head
     */
    isList(node) {
      if (!node) {
        return false;
      }

      // Check if it has rdf:first or rdf:rest properties
      const firstQuads = store.getQuads(node, namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#first"), null, null);
      const restQuads = store.getQuads(node, namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"), null, null);

      return firstQuads.length > 0 || restQuads.length > 0;
    },

    /**
     * Get the length of an RDF list
     * @param {Term} head - The head node of the list
     * @returns {number} The length of the list
     */
    length(head) {
      if (!head) {
        return 0;
      }

      let count = 0;
      let current = head;

      while (current) {
        // Check if current node is rdf:nil
        if (current.equals(namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"))) {
          break;
        }

        count++;

        // Get the rest of the list
        const restQuads = store.getQuads(current, namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"), null, null);
        if (restQuads.length > 0) {
          current = restQuads[0].object;
        } else {
          break;
        }
      }

      return count;
    },

    /**
     * Get the item at a specific index in the list
     * @param {Term} head - The head node of the list
     * @param {number} index - The index to get
     * @returns {Term|null} The item at the index or null
     */
    get(head, index) {
      if (!head || index < 0) {
        return null;
      }

      let current = head;
      let currentIndex = 0;

      while (current) {
        // Check if current node is rdf:nil
        if (current.equals(namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"))) {
          break;
        }

        if (currentIndex === index) {
          // Get the first item
          const firstQuads = store.getQuads(current, namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#first"), null, null);
          return firstQuads.length > 0 ? firstQuads[0].object : null;
        }

        currentIndex++;

        // Get the rest of the list
        const restQuads = store.getQuads(current, namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"), null, null);
        if (restQuads.length > 0) {
          current = restQuads[0].object;
        } else {
          break;
        }
      }

      return null;
    },

    /**
     * Append an item to the end of a list
     * @param {Term} head - The head node of the list
     * @param {Term} item - The item to append
     * @returns {Term} The head node of the updated list
     */
    append(head, item) {
      if (!head) {
        // Create a new list with just this item
        return this.write([item]);
      }

      // Find the last node
      let current = head;
      let lastNode = null;

      while (current) {
        // Check if current node is rdf:nil
        if (current.equals(namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"))) {
          break;
        }

        lastNode = current;

        // Get the rest of the list
        const restQuads = store.getQuads(current, namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"), null, null);
        if (restQuads.length > 0) {
          current = restQuads[0].object;
        } else {
          break;
        }
      }

      if (lastNode) {
        // Create a new node for the item
        const newNode = blankNode();

        // Update the last node's rdf:rest to point to the new node
        store.add(quad(
          lastNode,
          namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"),
          newNode
        ));

        // Add the new item
        store.add(quad(
          newNode,
          namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#first"),
          item
        ));

        // Point the new node to rdf:nil
        store.add(quad(
          newNode,
          namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"),
          namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil")
        ));
      }

      return head;
    },

    /**
     * Remove a list from the store
     * @param {Term} head - The head node of the list to remove
     * @returns {Object} This composable instance
     */
    remove(head) {
      if (!head) {
        return this;
      }

      let current = head;

      while (current) {
        // Check if current node is rdf:nil
        if (current.equals(namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"))) {
          break;
        }

        // Get the rest of the list before removing
        const restQuads = store.getQuads(current, namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"), null, null);
        const nextNode = restQuads.length > 0 ? restQuads[0].object : null;

        // Remove all quads for this node
        const nodeQuads = store.getQuads(current, null, null, null);
        for (const quad of nodeQuads) {
          store.delete(quad);
        }

        current = nextNode;
      }

      return this;
    }
  };
}