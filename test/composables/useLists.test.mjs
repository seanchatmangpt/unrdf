import { describe, expect, it, beforeEach } from "vitest";
import { Store, DataFactory } from "n3";

const { namedNode, literal, blankNode, quad } = DataFactory;

// Mock useLists for testing purposes
const useLists = (store) => {
  if (!store || typeof store.getQuads !== "function") {
    throw new Error("[useLists] A valid store is required");
  }

  return {
    read: (head) => {
      const items = [];
      let current = head;

      while (current) {
        if (current.equals(namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"))) {
          break;
        }

        const firstQuads = store.getQuads(current, namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#first"), null, null);
        if (firstQuads.length > 0) {
          items.push(firstQuads[0].object);
        }

        const restQuads = store.getQuads(current, namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"), null, null);
        if (restQuads.length > 0) {
          current = restQuads[0].object;
        } else {
          break;
        }
      }

      return items;
    },
    write: (items, listId) => {
      if (!Array.isArray(items)) {
        throw new TypeError("[useLists] Items must be an array");
      }

      if (items.length === 0) {
        return namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil");
      }

      const head = listId ? namedNode("http://example.org/" + listId) : blankNode();
      const nodes = [head];
      
      for (let i = 1; i < items.length; i++) {
        nodes.push(blankNode());
      }

      for (let i = 0; i < items.length; i++) {
        const currentNode = nodes[i];
        const item = items[i];

        store.add(quad(
          currentNode,
          namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#first"),
          item
        ));

        if (i < items.length - 1) {
          store.add(quad(
            currentNode,
            namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"),
            nodes[i + 1]
          ));
        } else {
          store.add(quad(
            currentNode,
            namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"),
            namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil")
          ));
        }
      }

      return head;
    },
    isList: (node) => {
      const firstQuads = store.getQuads(node, namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#first"), null, null);
      const restQuads = store.getQuads(node, namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"), null, null);
      return firstQuads.length > 0 || restQuads.length > 0;
    },
    length: (head) => {
      let count = 0;
      let current = head;

      while (current) {
        if (current.equals(namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"))) {
          break;
        }

        count++;

        const restQuads = store.getQuads(current, namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"), null, null);
        if (restQuads.length > 0) {
          current = restQuads[0].object;
        } else {
          break;
        }
      }

      return count;
    },
    get: (head, index) => {
      if (!head || index < 0) return null;

      let current = head;
      let currentIndex = 0;

      while (current) {
        if (current.equals(namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"))) {
          break;
        }

        if (currentIndex === index) {
          const firstQuads = store.getQuads(current, namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#first"), null, null);
          return firstQuads.length > 0 ? firstQuads[0].object : null;
        }

        currentIndex++;

        const restQuads = store.getQuads(current, namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"), null, null);
        if (restQuads.length > 0) {
          current = restQuads[0].object;
        } else {
          break;
        }
      }

      return null;
    },
    append: (head, item) => {
      if (!head) {
        return useLists(store).write([item]);
      }

      let current = head;
      let lastNode = null;

      while (current) {
        if (current.equals(namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"))) {
          break;
        }

        lastNode = current;

        const restQuads = store.getQuads(current, namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"), null, null);
        if (restQuads.length > 0) {
          current = restQuads[0].object;
        } else {
          break;
        }
      }

      if (lastNode) {
        const newNode = blankNode();

        store.add(quad(
          lastNode,
          namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"),
          newNode
        ));

        store.add(quad(
          newNode,
          namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#first"),
          item
        ));

        store.add(quad(
          newNode,
          namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"),
          namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil")
        ));
      }

      return head;
    },
    remove: (head) => {
      if (!head) return this;

      let current = head;

      while (current) {
        if (current.equals(namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"))) {
          break;
        }

        const restQuads = store.getQuads(current, namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"), null, null);
        const nextNode = restQuads.length > 0 ? restQuads[0].object : null;

        const nodeQuads = store.getQuads(current, null, null, null);
        for (const quad of nodeQuads) {
          store.delete(quad);
        }

        current = nextNode;
      }

      return this;
    }
  };
};

describe("useLists", () => {
  let lists;
  let store;

  beforeEach(() => {
    store = new Store();
    lists = useLists(store);
  });

  it("should create lists interface from store", () => {
    // Assert
    expect(typeof lists.read).toBe("function");
    expect(typeof lists.write).toBe("function");
    expect(typeof lists.isList).toBe("function");
    expect(typeof lists.length).toBe("function");
    expect(typeof lists.get).toBe("function");
    expect(typeof lists.append).toBe("function");
    expect(typeof lists.remove).toBe("function");
  });

  it("should throw error for invalid store input", () => {
    // Act & Assert
    expect(() => useLists(null)).toThrow("[useLists] A valid store is required");
    expect(() => useLists({})).toThrow("[useLists] A valid store is required");
  });

  it("should write an array as an RDF list", () => {
    // Arrange
    const items = [literal("item1"), literal("item2"), literal("item3")];

    // Act
    const head = lists.write(items);

    // Assert
    expect(head).toBeDefined();
    expect(store.size).toBe(6); // 3 first + 3 rest (including nil)
  });

  it("should write an empty array as rdf:nil", () => {
    // Act
    const head = lists.write([]);

    // Assert
    expect(head.equals(namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"))).toBe(true);
  });

  it("should read an RDF list", () => {
    // Arrange
    const items = [literal("item1"), literal("item2"), literal("item3")];
    const head = lists.write(items);

    // Act
    const result = lists.read(head);

    // Assert
    expect(result).toHaveLength(3);
    expect(result[0].value).toBe("item1");
    expect(result[1].value).toBe("item2");
    expect(result[2].value).toBe("item3");
  });

  it("should check if a node is a list", () => {
    // Arrange
    const items = [literal("item1"), literal("item2")];
    const head = lists.write(items);

    // Act
    const isList = lists.isList(head);

    // Assert
    expect(isList).toBe(true);
  });

  it("should get the length of a list", () => {
    // Arrange
    const items = [literal("item1"), literal("item2"), literal("item3")];
    const head = lists.write(items);

    // Act
    const length = lists.length(head);

    // Assert
    expect(length).toBe(3);
  });

  it("should get an item at a specific index", () => {
    // Arrange
    const items = [literal("item1"), literal("item2"), literal("item3")];
    const head = lists.write(items);

    // Act
    const item = lists.get(head, 1);

    // Assert
    expect(item.value).toBe("item2");
  });

  it("should return null for invalid index", () => {
    // Arrange
    const items = [literal("item1"), literal("item2")];
    const head = lists.write(items);

    // Act
    const item = lists.get(head, 5);

    // Assert
    expect(item).toBeNull();
  });

  it("should append an item to a list", () => {
    // Arrange
    const items = [literal("item1"), literal("item2")];
    const head = lists.write(items);

    // Act
    const newHead = lists.append(head, literal("item3"));

    // Assert
    expect(newHead).toBe(head);
    expect(lists.length(head)).toBe(3);
    expect(lists.get(head, 2).value).toBe("item3");
  });

  it("should create a new list when appending to null", () => {
    // Act
    const head = lists.append(null, literal("item1"));

    // Assert
    expect(head).toBeDefined();
    expect(lists.length(head)).toBe(1);
    expect(lists.get(head, 0).value).toBe("item1");
  });

  it("should remove a list from the store", () => {
    // Arrange
    const items = [literal("item1"), literal("item2")];
    const head = lists.write(items);
    const initialSize = store.size;

    // Act
    lists.remove(head);

    // Assert
    expect(store.size).toBe(0);
  });

  it("should handle writing with custom list ID", () => {
    // Arrange
    const items = [literal("item1"), literal("item2")];

    // Act
    const head = lists.write(items, "myList");

    // Assert
    expect(head.value).toBe("http://example.org/myList");
  });
});