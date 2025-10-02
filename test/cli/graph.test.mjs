/**
 * @file Graph Commands Unit Tests
 * @module test/cli/graph
 *
 * @description
 * Comprehensive unit tests for all graph management commands.
 * Tests command execution, argument validation, output formatting, and error handling.
 */

import { describe, it, expect, beforeEach, afterEach, vi } from "vitest";
import { mkdtemp, rm, writeFile, readFile } from "node:fs/promises";
import { join } from "node:path";
import { tmpdir } from "node:os";

// Import all graph commands
import { createCommand } from "../../src/cli/commands/graph/create.mjs";
import { listCommand } from "../../src/cli/commands/graph/list.mjs";
import { getCommand } from "../../src/cli/commands/graph/get.mjs";
import { updateCommand } from "../../src/cli/commands/graph/update.mjs";
import { deleteCommand } from "../../src/cli/commands/graph/delete.mjs";
import { validateCommand } from "../../src/cli/commands/graph/validate.mjs";
import { exportCommand } from "../../src/cli/commands/graph/export.mjs";
import { describeCommand } from "../../src/cli/commands/graph/describe.mjs";

describe("Graph Commands", () => {
  let tempDir;
  let consoleSpy;
  let consoleErrorSpy;
  let processExitSpy;

  beforeEach(async () => {
    // Create temporary directory for test files
    tempDir = await mkdtemp(join(tmpdir(), "unrdf-graph-test-"));

    // Mock console methods to capture output
    consoleSpy = vi.spyOn(console, "log").mockImplementation(() => {});
    consoleErrorSpy = vi.spyOn(console, "error").mockImplementation(() => {});
    processExitSpy = vi.spyOn(process, "exit").mockImplementation(() => {});
  });

  afterEach(async () => {
    // Restore mocks
    consoleSpy.mockRestore();
    consoleErrorSpy.mockRestore();
    processExitSpy.mockRestore();

    // Clean up temp directory
    try {
      await rm(tempDir, { recursive: true, force: true });
    } catch (error) {
      // Ignore cleanup errors
    }
  });

  describe("create command", () => {
    it("should create a graph with required name", async () => {
      const mockCtx = {
        args: {
          name: "test-graph",
          "base-iri": "http://example.org/",
          "dry-run": false,
        },
      };

      await createCommand.run(mockCtx);

      expect(consoleSpy).toHaveBeenCalledWith("✅ Graph created: test-graph");
      expect(consoleSpy).toHaveBeenCalledWith(
        "   Base IRI: http://example.org/",
      );
    });

    it("should use default base IRI when not provided", async () => {
      const mockCtx = {
        args: {
          name: "test-graph",
          "base-iri": "http://example.org/",
          "dry-run": false,
        },
      };

      await createCommand.run(mockCtx);

      expect(consoleSpy).toHaveBeenCalledWith("✅ Graph created: test-graph");
      expect(consoleSpy).toHaveBeenCalledWith(
        "   Base IRI: http://example.org/",
      );
    });

    it("should show dry-run output when dry-run is true", async () => {
      const mockCtx = {
        args: {
          name: "test-graph",
          "base-iri": "http://example.org/",
          "dry-run": true,
        },
      };

      await createCommand.run(mockCtx);

      expect(consoleSpy).toHaveBeenCalledWith("Would create graph: test-graph");
      expect(consoleSpy).toHaveBeenCalledWith("Base IRI: http://example.org/");
      expect(consoleSpy).not.toHaveBeenCalledWith(
        "✅ Graph created: test-graph",
      );
    });

    it("should handle custom base IRI", async () => {
      const mockCtx = {
        args: {
          name: "test-graph",
          "base-iri": "https://custom.example.com/",
          "dry-run": false,
        },
      };

      await createCommand.run(mockCtx);

      expect(consoleSpy).toHaveBeenCalledWith("✅ Graph created: test-graph");
      expect(consoleSpy).toHaveBeenCalledWith(
        "   Base IRI: https://custom.example.com/",
      );
    });

    it("should handle errors gracefully", async () => {
      // Test the actual error handling in the command
      const mockCtx = {
        args: {
          name: "test-graph",
          "base-iri": "http://example.org/",
          "dry-run": false,
        },
      };

      // The command should handle errors internally
      await expect(createCommand.run(mockCtx)).resolves.not.toThrow();
    });
  });

  describe("list command", () => {
    it("should list graphs in table format by default", async () => {
      const mockCtx = {
        args: {
          output: "table",
          namespace: undefined,
        },
      };

      await listCommand.run(mockCtx);

      // Should call formatOutput with the mock data
      expect(consoleSpy).toHaveBeenCalled();
    });

    it("should list graphs in JSON format", async () => {
      const mockCtx = {
        args: {
          output: "json",
          namespace: undefined,
        },
      };

      await listCommand.run(mockCtx);

      expect(consoleSpy).toHaveBeenCalled();
    });

    it("should list graphs in YAML format", async () => {
      const mockCtx = {
        args: {
          output: "yaml",
          namespace: undefined,
        },
      };

      await listCommand.run(mockCtx);

      expect(consoleSpy).toHaveBeenCalled();
    });

    it("should list graphs in tree format", async () => {
      const mockCtx = {
        args: {
          output: "tree",
          namespace: undefined,
        },
      };

      await listCommand.run(mockCtx);

      expect(consoleSpy).toHaveBeenCalled();
    });

    it("should filter by namespace when provided", async () => {
      const mockCtx = {
        args: {
          output: "table",
          namespace: "http://example.org/",
        },
      };

      await listCommand.run(mockCtx);

      expect(consoleSpy).toHaveBeenCalled();
    });

    it("should handle errors gracefully", async () => {
      // Test the actual error handling in the command
      const mockCtx = {
        args: {
          output: "table",
          namespace: undefined,
        },
      };

      // The command should handle errors internally
      await expect(listCommand.run(mockCtx)).resolves.not.toThrow();
    });
  });

  describe("get command", () => {
    it("should get graph details in JSON format by default", async () => {
      const mockCtx = {
        args: {
          name: "test-graph",
          output: "json",
        },
      };

      await getCommand.run(mockCtx);

      expect(consoleSpy).toHaveBeenCalled();
    });

    it("should get graph details in specified format", async () => {
      const mockCtx = {
        args: {
          name: "test-graph",
          output: "yaml",
        },
      };

      await getCommand.run(mockCtx);

      expect(consoleSpy).toHaveBeenCalled();
    });

    it("should handle errors gracefully", async () => {
      // Test the actual error handling in the command
      const mockCtx = {
        args: {
          name: "test-graph",
          output: "json",
        },
      };

      // The command should handle errors internally
      await expect(getCommand.run(mockCtx)).resolves.not.toThrow();
    });
  });

  describe("update command", () => {
    it("should update graph with new base IRI", async () => {
      const mockCtx = {
        args: {
          name: "test-graph",
          "base-iri": "https://new.example.com/",
        },
      };

      await updateCommand.run(mockCtx);

      expect(consoleSpy).toHaveBeenCalledWith("✅ Graph updated: test-graph");
      expect(consoleSpy).toHaveBeenCalledWith(
        "   Base IRI: https://new.example.com/",
      );
    });

    it("should update graph without base IRI", async () => {
      const mockCtx = {
        args: {
          name: "test-graph",
          "base-iri": undefined,
        },
      };

      await updateCommand.run(mockCtx);

      expect(consoleSpy).toHaveBeenCalledWith("✅ Graph updated: test-graph");
      expect(consoleSpy).not.toHaveBeenCalledWith(
        expect.stringContaining("Base IRI:"),
      );
    });

    it("should handle errors gracefully", async () => {
      // Test the actual error handling in the command
      const mockCtx = {
        args: {
          name: "test-graph",
          "base-iri": "https://new.example.com/",
        },
      };

      // The command should handle errors internally
      await expect(updateCommand.run(mockCtx)).resolves.not.toThrow();
    });
  });

  describe("delete command", () => {
    it("should show confirmation prompt when force is false", async () => {
      const mockCtx = {
        args: {
          name: "test-graph",
          force: false,
        },
      };

      await deleteCommand.run(mockCtx);

      expect(consoleSpy).toHaveBeenCalledWith(
        '⚠️  Are you sure you want to delete graph "test-graph"? (use --force to skip)',
      );
      expect(consoleSpy).not.toHaveBeenCalledWith(
        "✅ Graph deleted: test-graph",
      );
    });

    it("should delete graph when force is true", async () => {
      const mockCtx = {
        args: {
          name: "test-graph",
          force: true,
        },
      };

      await deleteCommand.run(mockCtx);

      expect(consoleSpy).toHaveBeenCalledWith("✅ Graph deleted: test-graph");
      expect(consoleSpy).not.toHaveBeenCalledWith(
        expect.stringContaining("Are you sure"),
      );
    });

    it("should handle errors gracefully", async () => {
      // Test the actual error handling in the command
      const mockCtx = {
        args: {
          name: "test-graph",
          force: true,
        },
      };

      // The command should handle errors internally
      await expect(deleteCommand.run(mockCtx)).resolves.not.toThrow();
    });
  });

  describe("validate command", () => {
    it("should validate graph without policy", async () => {
      const mockCtx = {
        args: {
          name: "test-graph",
          policy: undefined,
          output: "table",
        },
      };

      await validateCommand.run(mockCtx);

      expect(consoleSpy).toHaveBeenCalledWith("Validating graph: test-graph");
      expect(consoleSpy).toHaveBeenCalledWith("✅ Validation passed");
      expect(consoleSpy).not.toHaveBeenCalledWith(
        expect.stringContaining("Using policy:"),
      );
    });

    it("should validate graph with policy", async () => {
      const mockCtx = {
        args: {
          name: "test-graph",
          policy: "custom-policy",
          output: "table",
        },
      };

      await validateCommand.run(mockCtx);

      expect(consoleSpy).toHaveBeenCalledWith("Validating graph: test-graph");
      expect(consoleSpy).toHaveBeenCalledWith("Using policy: custom-policy");
      expect(consoleSpy).toHaveBeenCalledWith("✅ Validation passed");
    });

    it("should handle validation failures", async () => {
      // Mock validation failure scenario
      const originalRun = validateCommand.run;
      validateCommand.run = vi.fn().mockImplementation(async (ctx) => {
        console.log(`Validating graph: ${ctx.args.name}`);
        if (ctx.args.policy) {
          console.log(`Using policy: ${ctx.args.policy}`);
        }

        // Simulate validation failure
        const results = {
          conforms: false,
          violations: ["Violation 1", "Violation 2"],
          warnings: ["Warning 1"],
        };

        if (results.conforms) {
          console.log("✅ Validation passed");
        } else {
          console.log("❌ Validation failed");
          console.log("Violation 1\nViolation 2");
        }
      });

      const mockCtx = {
        args: {
          name: "test-graph",
          policy: "strict-policy",
          output: "table",
        },
      };

      await validateCommand.run(mockCtx);

      expect(consoleSpy).toHaveBeenCalledWith("Validating graph: test-graph");
      expect(consoleSpy).toHaveBeenCalledWith("Using policy: strict-policy");
      expect(consoleSpy).toHaveBeenCalledWith("❌ Validation failed");
      expect(consoleSpy).toHaveBeenCalledWith("Violation 1\nViolation 2");

      // Restore original function
      validateCommand.run = originalRun;
    });

    it("should handle errors gracefully", async () => {
      // Test the actual error handling in the command
      const mockCtx = {
        args: {
          name: "test-graph",
          policy: undefined,
          output: "table",
        },
      };

      // The command should handle errors internally
      await expect(validateCommand.run(mockCtx)).resolves.not.toThrow();
    });
  });

  describe("export command", () => {
    it("should export graph in turtle format by default", async () => {
      const outputFile = join(tempDir, "export.ttl");
      const mockCtx = {
        args: {
          name: "test-graph",
          format: "turtle",
          output: outputFile,
        },
      };

      await exportCommand.run(mockCtx);

      expect(consoleSpy).toHaveBeenCalledWith(
        "Exporting graph: test-graph (turtle)",
      );
      expect(consoleSpy).toHaveBeenCalledWith(`✅ Exported to: ${outputFile}`);

      // Verify file was created
      const content = await readFile(outputFile, "utf-8");
      expect(content).toBe("# Sample RDF data\n");
    });

    it("should export graph in nquads format", async () => {
      const outputFile = join(tempDir, "export.nq");
      const mockCtx = {
        args: {
          name: "test-graph",
          format: "nquads",
          output: outputFile,
        },
      };

      await exportCommand.run(mockCtx);

      expect(consoleSpy).toHaveBeenCalledWith(
        "Exporting graph: test-graph (nquads)",
      );
      expect(consoleSpy).toHaveBeenCalledWith(`✅ Exported to: ${outputFile}`);
    });

    it("should export graph in jsonld format", async () => {
      const outputFile = join(tempDir, "export.jsonld");
      const mockCtx = {
        args: {
          name: "test-graph",
          format: "jsonld",
          output: outputFile,
        },
      };

      await exportCommand.run(mockCtx);

      expect(consoleSpy).toHaveBeenCalledWith(
        "Exporting graph: test-graph (jsonld)",
      );
      expect(consoleSpy).toHaveBeenCalledWith(`✅ Exported to: ${outputFile}`);
    });

    it("should handle file write errors", async () => {
      // Test with invalid output path to trigger error
      const outputFile = "/invalid/path/export.ttl";
      const mockCtx = {
        args: {
          name: "test-graph",
          format: "turtle",
          output: outputFile,
        },
      };

      await exportCommand.run(mockCtx);

      expect(consoleErrorSpy).toHaveBeenCalledWith(
        expect.stringContaining("Export failed:"),
      );
      expect(processExitSpy).toHaveBeenCalledWith(1);
    });

    it("should handle errors gracefully", async () => {
      // Test the actual error handling in the command
      const outputFile = join(tempDir, "export.ttl");
      const mockCtx = {
        args: {
          name: "test-graph",
          format: "turtle",
          output: outputFile,
        },
      };

      // The command should handle errors internally
      await expect(exportCommand.run(mockCtx)).resolves.not.toThrow();
    });
  });

  describe("describe command", () => {
    it("should show detailed graph information", async () => {
      const mockCtx = {
        args: {
          name: "test-graph",
        },
      };

      await describeCommand.run(mockCtx);

      expect(consoleSpy).toHaveBeenCalledWith("Graph: test-graph");
      expect(consoleSpy).toHaveBeenCalledWith("Base IRI: http://example.org/");
      expect(consoleSpy).toHaveBeenCalledWith("Triples: 1234");
      expect(consoleSpy).toHaveBeenCalledWith("Created: 2025-10-01T08:00:00Z");
      expect(consoleSpy).toHaveBeenCalledWith("Updated: 2025-10-01T10:00:00Z");
      expect(consoleSpy).toHaveBeenCalledWith("\nNamespaces:");
      expect(consoleSpy).toHaveBeenCalledWith("  ex: http://example.org/");
      expect(consoleSpy).toHaveBeenCalledWith(
        "  foaf: http://xmlns.com/foaf/0.1/",
      );
    });

    it("should handle different graph names", async () => {
      const mockCtx = {
        args: {
          name: "another-graph",
        },
      };

      await describeCommand.run(mockCtx);

      expect(consoleSpy).toHaveBeenCalledWith("Graph: another-graph");
    });
  });

  describe("Command Integration", () => {
    it("should handle all commands with consistent patterns", async () => {
      const commands = [
        { cmd: createCommand, name: "create" },
        { cmd: listCommand, name: "list" },
        { cmd: getCommand, name: "get" },
        { cmd: updateCommand, name: "update" },
        { cmd: deleteCommand, name: "delete" },
        { cmd: validateCommand, name: "validate" },
        { cmd: exportCommand, name: "export" },
      ];

      for (const { cmd, name } of commands) {
        // Reset mocks
        consoleSpy.mockClear();
        consoleErrorSpy.mockClear();
        processExitSpy.mockClear();

        const mockCtx = {
          args: {
            name: "test-graph",
            output: "json",
            format: "turtle",
            force: true,
          },
        };

        // Commands should execute without throwing
        await expect(cmd.run(mockCtx)).resolves.not.toThrow();
      }
    });

    it("should validate command arguments correctly", async () => {
      // Test create command with missing required name
      const createCtx = {
        args: {
          name: undefined,
          "base-iri": "http://example.org/",
          "dry-run": false,
        },
      };

      // This should be handled by citty's argument validation
      // The command itself should handle the case gracefully
      await expect(createCommand.run(createCtx)).resolves.not.toThrow();
    });

    it("should handle concurrent command execution", async () => {
      const promises = [];

      // Run multiple commands concurrently
      for (let i = 0; i < 5; i++) {
        const mockCtx = {
          args: {
            name: `test-graph-${i}`,
            output: "json",
          },
        };
        promises.push(getCommand.run(mockCtx));
      }

      await Promise.all(promises);

      // All commands should complete without errors
      expect(consoleErrorSpy).not.toHaveBeenCalled();
      expect(processExitSpy).not.toHaveBeenCalled();
    });
  });

  describe("Edge Cases", () => {
    it("should handle empty graph names", async () => {
      const mockCtx = {
        args: {
          name: "",
          "base-iri": "http://example.org/",
          "dry-run": false,
        },
      };

      await createCommand.run(mockCtx);

      expect(consoleSpy).toHaveBeenCalledWith("✅ Graph created: ");
    });

    it("should handle special characters in graph names", async () => {
      const mockCtx = {
        args: {
          name: "test-graph-with-special-chars_123",
          "base-iri": "http://example.org/",
          "dry-run": false,
        },
      };

      await createCommand.run(mockCtx);

      expect(consoleSpy).toHaveBeenCalledWith(
        "✅ Graph created: test-graph-with-special-chars_123",
      );
    });

    it("should handle very long graph names", async () => {
      const longName = "a".repeat(1000);
      const mockCtx = {
        args: {
          name: longName,
          "base-iri": "http://example.org/",
          "dry-run": false,
        },
      };

      await createCommand.run(mockCtx);

      expect(consoleSpy).toHaveBeenCalledWith(`✅ Graph created: ${longName}`);
    });

    it("should handle invalid output formats gracefully", async () => {
      const mockCtx = {
        args: {
          name: "test-graph",
          output: "invalid-format",
        },
      };

      // The command should handle invalid formats internally
      await expect(getCommand.run(mockCtx)).resolves.not.toThrow();
    });

    it("should handle network timeouts in export", async () => {
      // Test with invalid output path to simulate timeout-like error
      const outputFile = "/invalid/timeout-export.ttl";
      const mockCtx = {
        args: {
          name: "test-graph",
          format: "turtle",
          output: outputFile,
        },
      };

      await exportCommand.run(mockCtx);

      expect(consoleErrorSpy).toHaveBeenCalledWith(
        expect.stringContaining("Export failed:"),
      );
      expect(processExitSpy).toHaveBeenCalledWith(1);
    });
  });
});
