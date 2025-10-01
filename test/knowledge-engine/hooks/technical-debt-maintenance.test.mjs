/**
 * @file Technical Debt and Maintenance Tests
 *
 * Tests for code quality issues, performance optimization needs,
 * security vulnerability patches, dependency updates, and legacy system integration.
 */

import { describe, it, expect, beforeEach, afterEach, vi } from "vitest";
import { KnowledgeHookManager } from "../../../src/knowledge-engine/knowledge-hook-manager.mjs";
import { defineHook } from "../../../src/knowledge-engine/define-hook.mjs";
import { Store, DataFactory } from "n3";
import { promises as fs } from "fs";
import { join } from "path";

// No mocks - use real implementation

describe("Technical Debt and Maintenance", () => {
  let manager;
  let tempDir;
  let codeQualityIssues;
  let performanceMetrics;
  let securityVulnerabilities;
  let dependencyIssues;

  beforeEach(async () => {
    // Create temporary directory for test files
    tempDir = join(process.cwd(), "test-temp-technical-debt");
    await fs.mkdir(tempDir, { recursive: true });

    // Create the SPARQL file in the temp directory
    await fs.mkdir(join(tempDir, "test-conditions"), { recursive: true });
    await fs.writeFile(
      join(tempDir, "test-conditions", "always-true.ask.rq"),
      "ASK { ?s ?p ?o }",
    );

    // Initialize tracking arrays
    codeQualityIssues = [];
    performanceMetrics = [];
    securityVulnerabilities = [];
    dependencyIssues = [];

    manager = new KnowledgeHookManager({
      basePath: tempDir,
      enableKnowledgeHooks: true,
      strictMode: false,
    });
  });

  afterEach(async () => {
    // Clean up temporary directory
    await fs.rm(tempDir, { recursive: true, force: true });
  });

  describe("Code Quality Issues", () => {
    it("should detect TODO comments and incomplete implementations", async () => {
      const codeQualityHook = defineHook({
        meta: {
          name: "maintenance:code-quality",
          description: "Detect code quality issues and technical debt",
        },
        when: {
          kind: "sparql-ask",
          ref: {
            uri: "file://test-conditions/always-true.ask.rq",
            sha256:
              "720ca56979ad91cd84a42aa3e84c21128090126acea855e8c928d42dcc7aea01",
          },
        },
        run: async (event) => {
          // Simulate code analysis for TODO comments
          const codeContent = event.payload.codeContent || "";
          const todoMatches = codeContent.match(/TODO|FIXME|XXX|HACK/gi) || [];

          if (todoMatches.length > 0) {
            codeQualityIssues.push({
              type: "TODO_COMMENTS",
              count: todoMatches.length,
              severity: "MEDIUM",
              description:
                "Code contains TODO comments indicating incomplete implementation",
            });
          }

          // Check for console.log statements (should use proper logging)
          const consoleLogMatches = codeContent.match(/console\.log/g) || [];
          if (consoleLogMatches.length > 0) {
            codeQualityIssues.push({
              type: "CONSOLE_LOGS",
              count: consoleLogMatches.length,
              severity: "LOW",
              description:
                "Code contains console.log statements instead of proper logging",
            });
          }

          return {
            result: {
              issuesFound: codeQualityIssues.length,
              todoCount: todoMatches.length,
              consoleLogCount: consoleLogMatches.length,
            },
            status: "code-analysis-complete",
          };
        },
      });

      manager.addKnowledgeHook(codeQualityHook);

      const event = {
        name: "code-analysis",
        payload: {
          codeContent: `
            // TODO: Implement proper dependency graph resolution
            function processData() {
              console.log('Processing data...');
              // FIXME: This is a temporary solution
              return data;
            }
          `,
        },
        context: {
          graph: (() => {
            const store = new Store();
            const { namedNode } = DataFactory;
            store.addQuad(
              namedNode("http://example.org/s"),
              namedNode("http://example.org/p"),
              namedNode("http://example.org/o"),
            );
            return store;
          })(),
          timestamp: new Date(),
        },
      };

      const results = await manager.executeAllKnowledgeHooks(event, {
        enableSandboxing: false,
      });

      // Should detect code quality issues
      expect(results[0].success).toBe(true);
      expect(results[0].runResult?.result?.issuesFound).toBe(2);
      expect(results[0].runResult?.result?.todoCount).toBe(2);
      expect(results[0].runResult?.result?.consoleLogCount).toBe(1);
      expect(codeQualityIssues.length).toBe(2);
      expect(codeQualityIssues[0].type).toBe("TODO_COMMENTS");
      expect(codeQualityIssues[1].type).toBe("CONSOLE_LOGS");
    });

    it("should detect code duplication and refactoring opportunities", async () => {
      const duplicationHook = defineHook({
        meta: {
          name: "maintenance:code-duplication",
          description: "Detect code duplication and refactoring opportunities",
        },
        when: {
          kind: "sparql-ask",
          ref: {
            uri: "file://test-conditions/always-true.ask.rq",
            sha256:
              "720ca56979ad91cd84a42aa3e84c21128090126acea855e8c928d42dcc7aea01",
          },
        },
        run: async (event) => {
          // Simulate code duplication analysis
          const files = event.payload.files || [];
          let duplicateBlocks = 0;
          let refactoringOpportunities = 0;

          files.forEach((file) => {
            // Simulate finding duplicate code blocks
            if (file.content.includes("duplicate logic")) {
              duplicateBlocks++;
              refactoringOpportunities++;
            }
          });

          if (duplicateBlocks > 0) {
            codeQualityIssues.push({
              type: "CODE_DUPLICATION",
              count: duplicateBlocks,
              severity: "HIGH",
              description:
                "Code contains duplicated logic that should be refactored",
            });
          }

          return {
            result: {
              duplicateBlocks,
              refactoringOpportunities,
              filesAnalyzed: files.length,
            },
            status: "duplication-analysis-complete",
          };
        },
      });

      manager.addKnowledgeHook(duplicationHook);

      const event = {
        name: "duplication-analysis",
        payload: {
          files: [
            { name: "file1.js", content: "duplicate logic here" },
            { name: "file2.js", content: "duplicate logic here" },
            { name: "file3.js", content: "unique logic" },
          ],
        },
        context: {
          graph: (() => {
            const store = new Store();
            const { namedNode } = DataFactory;
            store.addQuad(
              namedNode("http://example.org/s"),
              namedNode("http://example.org/p"),
              namedNode("http://example.org/o"),
            );
            return store;
          })(),
          timestamp: new Date(),
        },
      };

      const results = await manager.executeAllKnowledgeHooks(event, {
        enableSandboxing: false,
      });

      // Should detect code duplication
      expect(results[0].success).toBe(true);
      expect(results[0].runResult?.result?.duplicateBlocks).toBe(2);
      expect(results[0].runResult?.result?.refactoringOpportunities).toBe(2);
      expect(codeQualityIssues.length).toBe(1);
      expect(codeQualityIssues[0].type).toBe("CODE_DUPLICATION");
    });
  });

  describe("Performance Optimization Needs", () => {
    it("should detect performance bottlenecks and optimization opportunities", async () => {
      const performanceHook = defineHook({
        meta: {
          name: "maintenance:performance-optimization",
          description:
            "Detect performance bottlenecks and optimization opportunities",
        },
        when: {
          kind: "sparql-ask",
          ref: {
            uri: "file://test-conditions/always-true.ask.rq",
            sha256:
              "720ca56979ad91cd84a42aa3e84c21128090126acea855e8c928d42dcc7aea01",
          },
        },
        run: async (event) => {
          // Simulate performance analysis
          const executionTime = event.payload.executionTime || 0;
          const memoryUsage = event.payload.memoryUsage || 0;
          const cpuUsage = event.payload.cpuUsage || 0;

          let optimizationNeeded = false;
          let optimizationType = "";

          if (executionTime > 1000) {
            // > 1 second
            optimizationNeeded = true;
            optimizationType = "EXECUTION_TIME";
            performanceMetrics.push({
              type: "SLOW_EXECUTION",
              value: executionTime,
              threshold: 1000,
              severity: "HIGH",
              description: "Execution time exceeds acceptable threshold",
            });
          }

          if (memoryUsage > 100 * 1024 * 1024) {
            // > 100MB
            optimizationNeeded = true;
            optimizationType = "MEMORY_USAGE";
            performanceMetrics.push({
              type: "HIGH_MEMORY",
              value: memoryUsage,
              threshold: 100 * 1024 * 1024,
              severity: "MEDIUM",
              description: "Memory usage exceeds acceptable threshold",
            });
          }

          if (cpuUsage > 80) {
            // > 80%
            optimizationNeeded = true;
            optimizationType = "CPU_USAGE";
            performanceMetrics.push({
              type: "HIGH_CPU",
              value: cpuUsage,
              threshold: 80,
              severity: "HIGH",
              description: "CPU usage exceeds acceptable threshold",
            });
          }

          return {
            result: {
              optimizationNeeded,
              optimizationType,
              executionTime,
              memoryUsage,
              cpuUsage,
            },
            status: "performance-analysis-complete",
          };
        },
      });

      manager.addKnowledgeHook(performanceHook);

      const event = {
        name: "performance-analysis",
        payload: {
          executionTime: 1500, // 1.5 seconds
          memoryUsage: 150 * 1024 * 1024, // 150MB
          cpuUsage: 85, // 85%
        },
        context: {
          graph: (() => {
            const store = new Store();
            const { namedNode } = DataFactory;
            store.addQuad(
              namedNode("http://example.org/s"),
              namedNode("http://example.org/p"),
              namedNode("http://example.org/o"),
            );
            return store;
          })(),
          timestamp: new Date(),
        },
      };

      const results = await manager.executeAllKnowledgeHooks(event, {
        enableSandboxing: false,
      });

      // Should detect performance issues
      expect(results[0].success).toBe(true);
      expect(results[0].runResult?.result?.optimizationNeeded).toBe(true);
      expect(performanceMetrics.length).toBe(3);
      expect(performanceMetrics[0].type).toBe("SLOW_EXECUTION");
      expect(performanceMetrics[1].type).toBe("HIGH_MEMORY");
      expect(performanceMetrics[2].type).toBe("HIGH_CPU");
    });
  });

  describe("Security Vulnerability Patches", () => {
    it("should detect security vulnerabilities and recommend patches", async () => {
      const securityHook = defineHook({
        meta: {
          name: "maintenance:security-vulnerabilities",
          description: "Detect security vulnerabilities and recommend patches",
        },
        when: {
          kind: "sparql-ask",
          ref: {
            uri: "file://test-conditions/always-true.ask.rq",
            sha256:
              "720ca56979ad91cd84a42aa3e84c21128090126acea855e8c928d42dcc7aea01",
          },
        },
        run: async (event) => {
          // Simulate security vulnerability scanning
          const dependencies = event.payload.dependencies || [];
          const codeContent = event.payload.codeContent || "";

          let vulnerabilitiesFound = 0;
          let criticalVulnerabilities = 0;

          // Check for known vulnerable dependencies
          dependencies.forEach((dep) => {
            if (dep.version === "1.0.0" && dep.name === "vulnerable-package") {
              vulnerabilitiesFound++;
              criticalVulnerabilities++;
              securityVulnerabilities.push({
                type: "VULNERABLE_DEPENDENCY",
                package: dep.name,
                version: dep.version,
                severity: "CRITICAL",
                description: "Known security vulnerability in dependency",
              });
            }
          });

          // Check for insecure code patterns
          if (codeContent.includes("eval(")) {
            vulnerabilitiesFound++;
            securityVulnerabilities.push({
              type: "INSECURE_CODE",
              pattern: "eval()",
              severity: "HIGH",
              description: "Use of eval() creates security risk",
            });
          }

          if (codeContent.includes("innerHTML")) {
            vulnerabilitiesFound++;
            securityVulnerabilities.push({
              type: "XSS_RISK",
              pattern: "innerHTML",
              severity: "MEDIUM",
              description: "Potential XSS vulnerability with innerHTML",
            });
          }

          return {
            result: {
              vulnerabilitiesFound,
              criticalVulnerabilities,
              dependenciesScanned: dependencies.length,
            },
            status: "security-scan-complete",
          };
        },
      });

      manager.addKnowledgeHook(securityHook);

      const event = {
        name: "security-scan",
        payload: {
          dependencies: [
            { name: "vulnerable-package", version: "1.0.0" },
            { name: "safe-package", version: "2.1.0" },
          ],
          codeContent: `
            function processUserInput(input) {
              eval(input); // Security risk!
              document.getElementById('output').innerHTML = input;
            }
          `,
        },
        context: {
          graph: (() => {
            const store = new Store();
            const { namedNode } = DataFactory;
            store.addQuad(
              namedNode("http://example.org/s"),
              namedNode("http://example.org/p"),
              namedNode("http://example.org/o"),
            );
            return store;
          })(),
          timestamp: new Date(),
        },
      };

      const results = await manager.executeAllKnowledgeHooks(event, {
        enableSandboxing: false,
      });

      // Should detect security vulnerabilities
      expect(results[0].success).toBe(true);
      expect(results[0].runResult?.result?.vulnerabilitiesFound).toBe(3);
      expect(results[0].runResult?.result?.criticalVulnerabilities).toBe(1);
      expect(securityVulnerabilities.length).toBe(3);
      expect(securityVulnerabilities[0].type).toBe("VULNERABLE_DEPENDENCY");
      expect(securityVulnerabilities[1].type).toBe("INSECURE_CODE");
      expect(securityVulnerabilities[2].type).toBe("XSS_RISK");
    });
  });

  describe("Dependency Updates", () => {
    it("should detect outdated dependencies and recommend updates", async () => {
      const dependencyHook = defineHook({
        meta: {
          name: "maintenance:dependency-updates",
          description: "Detect outdated dependencies and recommend updates",
        },
        when: {
          kind: "sparql-ask",
          ref: {
            uri: "file://test-conditions/always-true.ask.rq",
            sha256:
              "720ca56979ad91cd84a42aa3e84c21128090126acea855e8c928d42dcc7aea01",
          },
        },
        run: async (event) => {
          // Simulate dependency analysis
          const dependencies = event.payload.dependencies || [];
          let outdatedDependencies = 0;
          let majorUpdatesAvailable = 0;

          dependencies.forEach((dep) => {
            const currentVersion = dep.version;
            const latestVersion = dep.latestVersion;

            if (currentVersion !== latestVersion) {
              outdatedDependencies++;
              dependencyIssues.push({
                type: "OUTDATED_DEPENDENCY",
                package: dep.name,
                currentVersion,
                latestVersion,
                severity: "MEDIUM",
                description: `Dependency ${dep.name} is outdated`,
              });

              // Check for major version updates
              const currentMajor = parseInt(currentVersion.split(".")[0]);
              const latestMajor = parseInt(latestVersion.split(".")[0]);

              if (latestMajor > currentMajor) {
                majorUpdatesAvailable++;
                dependencyIssues.push({
                  type: "MAJOR_UPDATE_AVAILABLE",
                  package: dep.name,
                  currentVersion,
                  latestVersion,
                  severity: "HIGH",
                  description: `Major version update available for ${dep.name}`,
                });
              }
            }
          });

          return {
            result: {
              outdatedDependencies,
              majorUpdatesAvailable,
              totalDependencies: dependencies.length,
            },
            status: "dependency-analysis-complete",
          };
        },
      });

      manager.addKnowledgeHook(dependencyHook);

      const event = {
        name: "dependency-analysis",
        payload: {
          dependencies: [
            { name: "lodash", version: "4.17.15", latestVersion: "4.17.21" },
            {
              name: "knowledge-engine",
              version: "1.0.0",
              latestVersion: "2.0.0",
            },
            { name: "express", version: "4.18.0", latestVersion: "4.18.0" },
          ],
        },
        context: {
          graph: (() => {
            const store = new Store();
            const { namedNode } = DataFactory;
            store.addQuad(
              namedNode("http://example.org/s"),
              namedNode("http://example.org/p"),
              namedNode("http://example.org/o"),
            );
            return store;
          })(),
          timestamp: new Date(),
        },
      };

      const results = await manager.executeAllKnowledgeHooks(event, {
        enableSandboxing: false,
      });

      // Should detect outdated dependencies
      expect(results[0].success).toBe(true);
      expect(results[0].runResult?.result?.outdatedDependencies).toBe(2);
      expect(results[0].runResult?.result?.majorUpdatesAvailable).toBe(1);
      expect(dependencyIssues.length).toBe(3); // 2 outdated + 1 major update
      expect(dependencyIssues[0].type).toBe("OUTDATED_DEPENDENCY");
      expect(dependencyIssues[2].type).toBe("MAJOR_UPDATE_AVAILABLE");
    });
  });

  describe("Legacy System Integration", () => {
    it("should detect legacy system integration issues", async () => {
      const legacyHook = defineHook({
        meta: {
          name: "maintenance:legacy-integration",
          description: "Detect legacy system integration issues",
        },
        when: {
          kind: "sparql-ask",
          ref: {
            uri: "file://test-conditions/always-true.ask.rq",
            sha256:
              "720ca56979ad91cd84a42aa3e84c21128090126acea855e8c928d42dcc7aea01",
          },
        },
        run: async (event) => {
          // Simulate legacy system analysis
          const integrations = event.payload.integrations || [];
          let legacyIssues = 0;
          let compatibilityIssues = 0;

          integrations.forEach((integration) => {
            if (integration.type === "legacy") {
              legacyIssues++;

              // Check for compatibility issues
              if (
                integration.protocol === "SOAP" &&
                integration.version === "1.1"
              ) {
                compatibilityIssues++;
                codeQualityIssues.push({
                  type: "LEGACY_PROTOCOL",
                  system: integration.name,
                  protocol: integration.protocol,
                  version: integration.version,
                  severity: "HIGH",
                  description: "Using outdated SOAP 1.1 protocol",
                });
              }

              if (integration.encoding === "ISO-8859-1") {
                compatibilityIssues++;
                codeQualityIssues.push({
                  type: "LEGACY_ENCODING",
                  system: integration.name,
                  encoding: integration.encoding,
                  severity: "MEDIUM",
                  description: "Using legacy character encoding",
                });
              }
            }
          });

          return {
            result: {
              legacyIssues,
              compatibilityIssues,
              totalIntegrations: integrations.length,
            },
            status: "legacy-analysis-complete",
          };
        },
      });

      manager.addKnowledgeHook(legacyHook);

      const event = {
        name: "legacy-analysis",
        payload: {
          integrations: [
            {
              name: "LegacyERP",
              type: "legacy",
              protocol: "SOAP",
              version: "1.1",
              encoding: "ISO-8859-1",
            },
            {
              name: "ModernAPI",
              type: "modern",
              protocol: "REST",
              version: "2.0",
              encoding: "UTF-8",
            },
          ],
        },
        context: {
          graph: (() => {
            const store = new Store();
            const { namedNode } = DataFactory;
            store.addQuad(
              namedNode("http://example.org/s"),
              namedNode("http://example.org/p"),
              namedNode("http://example.org/o"),
            );
            return store;
          })(),
          timestamp: new Date(),
        },
      };

      const results = await manager.executeAllKnowledgeHooks(event, {
        enableSandboxing: false,
      });

      // Should detect legacy integration issues
      expect(results[0].success).toBe(true);
      expect(results[0].runResult?.result?.legacyIssues).toBe(1);
      expect(results[0].runResult?.result?.compatibilityIssues).toBe(2);
      expect(codeQualityIssues.length).toBe(2);
      expect(codeQualityIssues[0].type).toBe("LEGACY_PROTOCOL");
      expect(codeQualityIssues[1].type).toBe("LEGACY_ENCODING");
    });
  });
});
