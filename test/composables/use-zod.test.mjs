import { describe, it, expect, beforeEach } from "vitest";
import { useZod } from "../../src/composables/use-zod.mjs";
import { initStore, useStoreContext } from "../../src/context/index.mjs";
import { Store, DataFactory } from "n3";

const { namedNode, literal, quad } = DataFactory;

describe("useZod dynamic schema generation", () => {
  let runApp;

  beforeEach(() => {
    runApp = initStore();
  });

  it("provides schema generation interface", async () => {
    await runApp(async () => {
      const zod = useZod();
      
      expect(zod).toBeDefined();
      expect(typeof zod.generateFromResults).toBe("function");
      expect(typeof zod.generateFromPattern).toBe("function");
      expect(typeof zod.generateFromSHACL).toBe("function");
      expect(typeof zod.validate).toBe("function");
      expect(zod.engine).toBeDefined();
      expect(zod.store).toBeDefined();
    });
  });

  it("generates schema from SPARQL results", async () => {
    await runApp(async () => {
      const zod = useZod();
      
      const results = [
        { id: "http://example.org/alice", name: "Alice", age: "30" },
        { id: "http://example.org/bob", name: "Bob", age: "25" }
      ];
      
      const schema = zod.generateFromResults(results);
      
      expect(schema).toBeDefined();
      
      // Test validation
      const validation = zod.validate(results[0], schema);
      expect(validation.success).toBe(true);
      expect(validation.data).toEqual(results[0]);
    });
  });

  it("infers types from SPARQL results", async () => {
    await runApp(async () => {
      const zod = useZod();
      
      const results = [
        { id: "http://example.org/alice", name: "Alice", age: 30, active: true },
        { id: "http://example.org/bob", name: "Bob", age: 25, active: false }
      ];
      
      const schema = zod.generateFromResults(results);
      
      // Test validation with correct types
      const validation = zod.validate(results[0], schema);
      expect(validation.success).toBe(true);
      
      // Test validation with wrong types
      const wrongData = { id: "not-a-url", name: "Alice", age: "not-a-number", active: "not-a-boolean" };
      const wrongValidation = zod.validate(wrongData, schema);
      expect(wrongValidation.success).toBe(false);
    });
  });

  it("handles empty results gracefully", async () => {
    await runApp(async () => {
      const zod = useZod();
      
      const schema = zod.generateFromResults([]);
      expect(schema).toBeDefined();
      
      const validation = zod.validate({}, schema);
      expect(validation.success).toBe(true);
    });
  });

  it("generates schema from RDF store patterns", async () => {
    await runApp(async () => {
      const zod = useZod();
      const storeContext = useStoreContext();
      
      // Add some test data
      storeContext.add(quad(
        namedNode("http://example.org/alice"),
        namedNode("http://xmlns.com/foaf/0.1/name"),
        literal("Alice")
      ));
      storeContext.add(quad(
        namedNode("http://example.org/alice"),
        namedNode("http://xmlns.com/foaf/0.1/age"),
        literal("30")
      ));
      
      const schema = zod.generateFromPattern("http://example.org/alice");
      
      expect(schema).toBeDefined();
      
      // Test validation
      const testData = { name: "Alice", age: "30" };
      const validation = zod.validate(testData, schema);
      expect(validation.success).toBe(true);
    });
  });

  it("handles patterns with no matches", async () => {
    await runApp(async () => {
      const zod = useZod();
      
      const schema = zod.generateFromPattern("http://example.org/nonexistent");
      expect(schema).toBeDefined();
      
      const validation = zod.validate({}, schema);
      expect(validation.success).toBe(true);
    });
  });

  it("generates schema from SHACL shapes", async () => {
    await runApp(async () => {
      const zod = useZod();
      const storeContext = useStoreContext();
      
      // Add SHACL shape data
      storeContext.add(quad(
        namedNode("http://example.org/PersonShape"),
        namedNode("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"),
        namedNode("http://www.w3.org/ns/shacl#NodeShape")
      ));
      storeContext.add(quad(
        namedNode("http://example.org/PersonShape"),
        namedNode("http://www.w3.org/ns/shacl#path"),
        namedNode("http://xmlns.com/foaf/0.1/name")
      ));
      storeContext.add(quad(
        namedNode("http://example.org/PersonShape"),
        namedNode("http://www.w3.org/ns/shacl#datatype"),
        namedNode("http://www.w3.org/2001/XMLSchema#string")
      ));
      
      const schema = zod.generateFromSHACL("http://example.org/PersonShape");
      
      expect(schema).toBeDefined();
      
      // Test validation
      const testData = { name: "Alice" };
      const validation = zod.validate(testData, schema);
      expect(validation.success).toBe(true);
    });
  });

  it("handles SHACL shapes with no matches", async () => {
    await runApp(async () => {
      const zod = useZod();
      
      const schema = zod.generateFromSHACL("http://example.org/NonexistentShape");
      expect(schema).toBeDefined();
      
      const validation = zod.validate({}, schema);
      expect(validation.success).toBe(true);
    });
  });

  it("supports strict and non-strict modes", async () => {
    await runApp(async () => {
      const strictZod = useZod({ strict: true });
      const nonStrictZod = useZod({ strict: false });
      
      const results = [{ name: "Alice", age: "30" }];
      
      const strictSchema = strictZod.generateFromResults(results);
      const nonStrictSchema = nonStrictZod.generateFromResults(results);
      
      // Test with missing fields
      const partialData = { name: "Alice" };
      
      const strictValidation = strictZod.validate(partialData, strictSchema);
      const nonStrictValidation = nonStrictZod.validate(partialData, nonStrictSchema);
      
      expect(strictValidation.success).toBe(false);
      expect(nonStrictValidation.success).toBe(true);
    });
  });

  it("infers URL types for ID fields", async () => {
    await runApp(async () => {
      const zod = useZod();
      
      const results = [
        { id: "http://example.org/alice", name: "Alice" },
        { userId: "http://example.org/bob", name: "Bob" }
      ];
      
      const schema = zod.generateFromResults(results);
      
      // Test valid URLs
      const validData = { id: "http://example.org/alice", name: "Alice" };
      const validValidation = zod.validate(validData, schema);
      expect(validValidation.success).toBe(true);
      
      // Test invalid URLs
      const invalidData = { id: "not-a-url", name: "Alice" };
      const invalidValidation = zod.validate(invalidData, schema);
      expect(invalidValidation.success).toBe(false);
    });
  });

  it("infers email types for email fields", async () => {
    await runApp(async () => {
      const zod = useZod();
      
      const results = [
        { email: "alice@example.org", name: "Alice" },
        { userEmail: "bob@example.org", name: "Bob" }
      ];
      
      const schema = zod.generateFromResults(results);
      
      // Test valid emails
      const validData = { email: "alice@example.org", name: "Alice" };
      const validValidation = zod.validate(validData, schema);
      expect(validValidation.success).toBe(true);
      
      // Test invalid emails
      const invalidData = { email: "not-an-email", name: "Alice" };
      const invalidValidation = zod.validate(invalidData, schema);
      expect(invalidValidation.success).toBe(false);
    });
  });

  it("handles mixed data types gracefully", async () => {
    await runApp(async () => {
      const zod = useZod();
      
      const results = [
        { name: "Alice", age: "30" },
        { name: "Bob", age: 25 },
        { name: "Charlie", age: "unknown" }
      ];
      
      const schema = zod.generateFromResults(results);
      
      // Should create union type for mixed data
      const testData = { name: "Alice", age: "30" };
      const validation = zod.validate(testData, schema);
      expect(validation.success).toBe(true);
    });
  });

  it("converts predicate IRIs to camelCase keys", async () => {
    await runApp(async () => {
      const zod = useZod();
      const storeContext = useStoreContext();
      
      // Add data with long predicate names
      storeContext.add(quad(
        namedNode("http://example.org/alice"),
        namedNode("http://xmlns.com/foaf/0.1/firstName"),
        literal("Alice")
      ));
      storeContext.add(quad(
        namedNode("http://example.org/alice"),
        namedNode("http://xmlns.com/foaf/0.1/lastName"),
        literal("Smith")
      ));
      
      const schema = zod.generateFromPattern("http://example.org/alice");
      
      // Test that keys are converted to camelCase
      const testData = { firstName: "Alice", lastName: "Smith" };
      const validation = zod.validate(testData, schema);
      expect(validation.success).toBe(true);
    });
  });

  it("provides access to engine and store", async () => {
    await runApp(async () => {
      const zod = useZod();
      
      expect(zod.engine).toBeDefined();
      expect(zod.store).toBeDefined();
      expect(typeof zod.engine.parseTurtle).toBe("function");
      expect(typeof zod.store.add).toBe("function");
    });
  });
});
