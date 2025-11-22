/**
 * @file MAPEK Full Integration Tests - tests all 10 innovations orchestrated together
 * @vitest-environment node
 */

import { describe, it, expect, beforeEach } from "vitest"
import { Store, DataFactory } from "n3"
import {
  runFullMapekWithAllInnovations,
  runInnovationsParallel,
  aggregateInnovationFindings,
  ALL_INNOVATIONS,
} from "../../src/project-engine/mapek-orchestration.mjs"

const { namedNode, literal } = DataFactory

const NS = {
  rdf: "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
  fs: "http://example.org/unrdf/filesystem#",
  proj: "http://example.org/unrdf/project#",
  dom: "http://example.org/unrdf/domain#",
}

/**
 * Create a test project store with sample files
 */
function createTestProjectStore() {
  const store = new Store()

  const files = [
    { path: "src/index.mjs", role: "Api", size: 1200 },
    { path: "src/users/user.service.mjs", role: "Service", size: 2400 },
    { path: "src/users/user.schema.mjs", role: "Schema", size: 800 },
    { path: "src/posts/post.service.mjs", role: "Service", size: 3200 },
    { path: "test/users/user.test.mjs", role: "Test", size: 1600 },
    { path: "docs/README.md", role: "Doc", size: 400 },
  ]

  for (const file of files) {
    const fileIri = namedNode(NS.fs + encodeURIComponent(file.path))
    store.addQuad(fileIri, namedNode(NS.rdf + "type"), namedNode(NS.fs + "File"))
    store.addQuad(fileIri, namedNode(NS.fs + "relativePath"), literal(file.path))
    store.addQuad(fileIri, namedNode(NS.fs + "byteSize"), literal(String(file.size)))
    store.addQuad(fileIri, namedNode(NS.proj + "roleString"), literal(file.role))
  }

  return store
}

/**
 * Create a test domain store with sample entities
 */
function createTestDomainStore() {
  const store = new Store()

  const entities = ["User", "Post", "Comment"]

  for (const entity of entities) {
    const entityIri = namedNode(NS.dom + entity)
    store.addQuad(entityIri, namedNode(NS.rdf + "type"), namedNode(NS.dom + "Entity"))
  }

  return store
}

describe("MAPEK Full Integration", () => {
  let projectStore
  let domainStore
  let stackProfile

  beforeEach(() => {
    projectStore = createTestProjectStore()
    domainStore = createTestDomainStore()
    stackProfile = {
      webFramework: "express",
      testFramework: "vitest",
      hasTypescript: false,
    }
  })

  describe("runInnovationsParallel", () => {
    it("should run all innovations in parallel", async () => {
      const result = await runInnovationsParallel({
        projectStore,
        domainStore,
        stackProfile,
        projectRoot: "/test/project",
      })

      expect(result).toHaveProperty("results")
      expect(result).toHaveProperty("errors")
      expect(result.innovationsRun).toBe(ALL_INNOVATIONS.length)
    })

    it("should run selected innovations only", async () => {
      const result = await runInnovationsParallel({
        projectStore,
        domainStore,
        stackProfile,
        innovations: ["gap-finder", "hotspot-analyzer"],
      })

      expect(result.innovationsRun).toBe(2)
      expect(result.results.gaps).toBeDefined()
      expect(result.results.hotspots).toBeDefined()
    })

    it("should handle errors gracefully", async () => {
      const result = await runInnovationsParallel({
        projectStore,
        domainStore,
        stackProfile,
        innovations: ["unknown-innovation"],
      })

      expect(result.errors.length).toBeGreaterThan(0)
      expect(result.errors[0].innovation).toBe("unknown-innovation")
    })
  })

  describe("aggregateInnovationFindings", () => {
    it("should aggregate findings from all innovations", async () => {
      const innovationResults = await runInnovationsParallel({
        projectStore,
        domainStore,
        stackProfile,
      })

      const aggregated = aggregateInnovationFindings(innovationResults)

      expect(aggregated).toHaveProperty("scores")
      expect(aggregated).toHaveProperty("overallHealth")
      expect(aggregated).toHaveProperty("totalIssues")
      expect(aggregated).toHaveProperty("prioritizedIssues")
      expect(aggregated).toHaveProperty("summaries")
    })

    it("should calculate health score between 0 and 100", async () => {
      const innovationResults = await runInnovationsParallel({
        projectStore,
        domainStore,
        stackProfile,
      })

      const aggregated = aggregateInnovationFindings(innovationResults)

      expect(aggregated.overallHealth).toBeGreaterThanOrEqual(0)
      expect(aggregated.overallHealth).toBeLessThanOrEqual(100)
    })

    it("should prioritize critical issues first", async () => {
      const innovationResults = await runInnovationsParallel({
        projectStore,
        domainStore,
        stackProfile,
      })

      const aggregated = aggregateInnovationFindings(innovationResults)

      if (aggregated.prioritizedIssues.length > 1) {
        const severityOrder = { critical: 0, high: 1, medium: 2, low: 3 }
        for (let i = 1; i < aggregated.prioritizedIssues.length; i++) {
          const prev = severityOrder[aggregated.prioritizedIssues[i - 1].severity]
          const curr = severityOrder[aggregated.prioritizedIssues[i].severity]
          expect(prev).toBeLessThanOrEqual(curr)
        }
      }
    })
  })

  describe("runFullMapekWithAllInnovations", () => {
    it("should complete full MAPEK cycle", async () => {
      const result = await runFullMapekWithAllInnovations({
        projectStore,
        domainStore,
        stackProfile,
        projectRoot: "/test/project",
      })

      expect(result.phase).toBe("knowledge")
      expect(result).toHaveProperty("allFindings")
      expect(result).toHaveProperty("aggregated")
      expect(result).toHaveProperty("decisions")
      expect(result).toHaveProperty("actions")
      expect(result).toHaveProperty("learnings")
    })

    it("should generate decisions based on findings", async () => {
      const result = await runFullMapekWithAllInnovations({
        projectStore,
        domainStore,
        stackProfile,
        projectRoot: "/test/project",
      })

      expect(Array.isArray(result.decisions)).toBe(true)
      for (const decision of result.decisions) {
        expect(decision).toHaveProperty("issue")
        expect(decision).toHaveProperty("severity")
        expect(decision).toHaveProperty("action")
        expect(decision).toHaveProperty("autoFixable")
      }
    })

    it("should plan actions for auto-fixable decisions", async () => {
      const result = await runFullMapekWithAllInnovations({
        projectStore,
        domainStore,
        stackProfile,
        projectRoot: "/test/project",
      })

      const autoFixableDecisions = result.decisions.filter(d => d.autoFixable)
      expect(result.actions.length).toBeLessThanOrEqual(autoFixableDecisions.length)
    })

    it("should extract learnings for knowledge phase", async () => {
      const result = await runFullMapekWithAllInnovations({
        projectStore,
        domainStore,
        stackProfile,
        projectRoot: "/test/project",
      })

      expect(result.learnings).toHaveProperty("timestamp")
      expect(result.learnings).toHaveProperty("patterns")
      expect(result.learnings).toHaveProperty("thresholds")
    })

    it("should indicate if another iteration is needed", async () => {
      const result = await runFullMapekWithAllInnovations({
        projectStore,
        domainStore,
        stackProfile,
        projectRoot: "/test/project",
      })

      expect(typeof result.shouldRepeat).toBe("boolean")
    })

    it("should measure execution duration", async () => {
      const result = await runFullMapekWithAllInnovations({
        projectStore,
        domainStore,
        stackProfile,
        projectRoot: "/test/project",
      })

      expect(result.duration).toBeGreaterThan(0)
    })
  })

  describe("ALL_INNOVATIONS constant", () => {
    it("should contain 10 innovations", () => {
      expect(ALL_INNOVATIONS.length).toBe(10)
    })

    it("should contain expected innovation names", () => {
      expect(ALL_INNOVATIONS).toContain("gap-finder")
      expect(ALL_INNOVATIONS).toContain("type-auditor")
      expect(ALL_INNOVATIONS).toContain("hotspot-analyzer")
      expect(ALL_INNOVATIONS).toContain("auto-test-generator")
      expect(ALL_INNOVATIONS).toContain("doc-drift-checker")
      expect(ALL_INNOVATIONS).toContain("dependency-graph")
      expect(ALL_INNOVATIONS).toContain("api-contract-validator")
      expect(ALL_INNOVATIONS).toContain("stack-linter")
      expect(ALL_INNOVATIONS).toContain("refactoring-guide")
      expect(ALL_INNOVATIONS).toContain("doc-generator")
    })
  })
})
