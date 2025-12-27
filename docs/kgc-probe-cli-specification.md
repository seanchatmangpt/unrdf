# KGC Probe CLI Specification

## SPARC Pseudocode Design - Agent-8 (CLI)

**Design Phase**: Pseudocode phase bridging specification to implementation
**Status**: Specification only - no implementation code yet
**Version**: 1.0.0
**Author**: Agent-8 (CLI Design)

---

## Executive Summary

This specification defines the KGC Probe CLI interface for deterministic codebase analysis. Five main commands provide:
- **Scanning**: Run swarm agents, capture shards, verify receipts
- **Merging**: Deterministic shard consolidation with conflict detection
- **Diffing**: Semantic changes between probe runs
- **Reporting**: Multi-format output (Markdown, JSON, TTL, PDF)
- **Verification**: Cryptographic proof validation and merkle chain integrity

The design separates concerns into:
- **Command Layer**: Citty-based argument parsing (noun/verb)
- **Orchestration Layer**: Swarm execution, shard collection, conflict resolution
- **Validation Layer**: Guard enforcement, receipt signing, merkle verification
- **Output Layer**: Format-specific generators (MD, JSON, TTL, PDF)

---

## Part 1: Command Definitions

### COMMAND GROUP: kgc probe

All commands follow pattern: `kgc probe <verb> [args] [--options]`

---

## 1.1 kgc probe scan

```
ALGORITHM: ProbeCommand.scan
PURPOSE: Run swarm agents, collect output shards, verify receipts, emit merged artifact

SIGNATURE:
    scan([options]) → {
        runId: string,
        shardCount: integer,
        mergedArtifact: Artifact,
        receipts: ReceiptChain,
        metrics: {startTime, endTime, agentCount, duration_ms}
    }

INPUT PARAMETERS:
    --config <path>         : Config file with allowlists, guard policies (default: .kgc-probe.json)
    --output <dir>          : Output directory for shards/merged/receipts (default: ./probe/out/<run-id>)
    --timeout <ms>          : Max execution time per agent (default: 30000)
    --parallel <count>      : Max concurrent agents (default: 10)
    --validate              : Validate each shard before merge (default: true)
    --format <fmt>          : Merged output format (default: all) - ttl|json|md
    --no-receipts           : Skip receipt generation (NOT RECOMMENDED)
    --merkle                : Include merkle tree in receipts (default: true)

OUTPUT STRUCTURE:
    probe/out/<run-id>/
    ├── shards/
    │   ├── agent-01-orchestrator.json       ← Raw shard from agent 1
    │   ├── agent-02-runtime.json            ← Raw shard from agent 2
    │   ├── ...
    │   └── agent-10-...</json>
    ├── merged/
    │   ├── world.ttl                        ← RDF graph (Turtle format)
    │   ├── index.json                       ← Machine-readable index
    │   └── report.md                        ← Human-readable report (Diataxis)
    ├── receipts/
    │   ├── chain.json                       ← Hash chain per agent (proof of provenance)
    │   ├── merkle.json                      ← Merkle root + membership proofs
    │   └── verification-log.txt             ← Signature/hash verification results
    ├── diff/ (optional, if --compare given)
    │   └── delta.json                       ← Changes from previous run
    └── meta/
        ├── config.json                      ← Run parameters + allowlists applied
        ├── manifest.json                    ← Agent versions, timestamps
        └── metrics.json                     ← Timing, shard sizes, coverage

BEGIN
    // Phase 0: Pre-flight checks
    config ← LoadConfig(options.config, options)
    ValidateConfig(config)                                    // Guard: Schema validation

    IF options.format NOT IN {ttl, json, md, all} THEN
        RETURN error("INVALID_FORMAT", "format must be one of: ttl|json|md|all")
    END IF

    // Phase 1: Initialize run
    runId ← GenerateRunId()                                   // e.g., "20250101-120000-abc123"
    outputDir ← ResolveOutputPath(options.output, runId)
    EnsureDir(outputDir + "/shards")
    EnsureDir(outputDir + "/receipts")
    EnsureDir(outputDir + "/meta")

    // Phase 2: Execute swarm
    agents ← LoadAgentRegistry()                              // 10 agents for KGC
    agentOrder ← TopologicalSort(agents, config.dependencies) // Deterministic order

    agentTasks ← []
    FOR EACH agent IN agentOrder DO
        IF agent.name NOT IN config.allowlist THEN
            LogWarning("Agent ${agent.name} not in allowlist, skipping")
            CONTINUE
        END IF

        task ← {
            agentId: agent.id,
            agentName: agent.name,
            timeout: options.timeout,
            config: config.guards[agent.name] || {}
        }
        agentTasks.append(task)
    END FOR

    // Phase 2a: Run agents in parallel (respecting maxParallel)
    startTime ← GetCurrentTime()
    results ← ExecuteParallel(agentTasks, options.parallel, timeout=options.timeout)
    endTime ← GetCurrentTime()

    // Phase 2b: Process results
    shards ← []
    failedAgents ← []

    FOR EACH result IN results DO
        IF result.status = "success" THEN
            shard ← {
                agentId: result.agentId,
                agentName: result.agentName,
                timestamp: result.timestamp,
                output: result.data,                          // Agent output (JSON or RDF)
                hash: SHA256(JSON.stringify(result.data)),
                duration_ms: result.duration
            }
            WriteShard(outputDir + "/shards/" + shard.agentName + ".json", shard)
            shards.append(shard)
        ELSE
            failedAgents.append({
                agentName: result.agentName,
                error: result.error,
                code: result.code
            })
        END IF
    END FOR

    // Phase 3: Generate receipts (hash chains)
    IF NOT options.no_receipts THEN
        hashChain ← []
        FOR EACH shard IN shards DO
            entry ← {
                agentName: shard.agentName,
                shardHash: shard.hash,
                previousHash: hashChain[-1].hash IF hashChain.length > 0 ELSE null,
                timestamp: shard.timestamp
            }
            entry.hash ← SHA256(JSON.stringify(entry))        // Chain hash
            hashChain.append(entry)
        END FOR

        WriteFile(outputDir + "/receipts/chain.json", JSON.stringify({
            chain: hashChain,
            root: hashChain[-1].hash,
            agentCount: shards.length
        }, null, 2))

        // Generate merkle tree if requested
        IF options.merkle THEN
            merkleTree ← BuildMerkleTree(shards)              // See: 2.1.2
            merkleProofs ← GenerateMembershipProofs(merkleTree)
            WriteFile(outputDir + "/receipts/merkle.json", JSON.stringify({
                root: merkleTree.root,
                tree: merkleTree.structure,
                proofs: merkleProofs
            }, null, 2))
        END IF
    END IF

    // Phase 4: Merge shards into unified artifact
    IF options.validate THEN
        FOR EACH shard IN shards DO
            ValidateShard(shard, config.schema)               // Zod schema validation
        END FOR
    END IF

    mergedArtifact ← MergeShardsDeterm(shards, config)        // See: 2.2

    // Phase 5: Generate reports in requested formats
    reports ← {}
    IF options.format IN {ttl, all} THEN
        ttlGraph ← ConvertToRDF(mergedArtifact, config.ontology)
        WriteFile(outputDir + "/merged/world.ttl", ttlGraph)
        reports.ttl ← outputDir + "/merged/world.ttl"
    END IF

    IF options.format IN {json, all} THEN
        WriteFile(outputDir + "/merged/index.json", JSON.stringify(mergedArtifact, null, 2))
        reports.json ← outputDir + "/merged/index.json"
    END IF

    IF options.format IN {md, all} THEN
        mdReport ← GenerateMarkdownReport(mergedArtifact, config)  // See: 2.3
        WriteFile(outputDir + "/merged/report.md", mdReport)
        reports.md ← outputDir + "/merged/report.md"
    END IF

    // Phase 6: Generate diff if comparing to previous run
    IF options.compare THEN
        previousArtifact ← LoadArtifact(options.compare)
        delta ← ComputeDelta(previousArtifact, mergedArtifact)
        WriteFile(outputDir + "/diff/delta.json", JSON.stringify(delta, null, 2))
    END IF

    // Phase 7: Write metadata and metrics
    WriteFile(outputDir + "/meta/config.json", JSON.stringify({
        runId: runId,
        timestamp: new Date().toISOString(),
        config: config,
        options: options,
        allowlist: config.allowlist
    }, null, 2))

    WriteFile(outputDir + "/meta/metrics.json", JSON.stringify({
        startTime: startTime,
        endTime: endTime,
        duration_ms: endTime - startTime,
        agentCount: shards.length,
        failedCount: failedAgents.length,
        shardSizes: shards.map(s => ({agent: s.agentName, bytes: SIZEOF(s)})),
        mergedSize: SIZEOF(mergedArtifact)
    }, null, 2))

    // Phase 8: Return result envelope
    IF failedAgents.length > 0 THEN
        LogWarning("${failedAgents.length} agents failed - see meta/metrics.json")
    END IF

    RETURN {
        success: failedAgents.length = 0,
        runId: runId,
        outputDir: outputDir,
        shardCount: shards.length,
        failedCount: failedAgents.length,
        failedAgents: failedAgents,
        mergedArtifact: mergedArtifact,
        receipts: {
            hashChainPath: outputDir + "/receipts/chain.json",
            merklePath: options.merkle ? outputDir + "/receipts/merkle.json" : null
        },
        reports: reports,
        metrics: {
            startTime: startTime,
            endTime: endTime,
            duration_ms: endTime - startTime,
            agentCount: shards.length
        }
    }
END
```

**Guard Violations**:
- Agent not in allowlist → Skip with warning, continue
- Shard validation fails → Log error, exclude from merge, list in metrics
- Timeout exceeded → Capture timeout error, mark agent failed
- Failed agent count = totalAgents → Return error with code SWARM_COMPLETE_FAILURE

**Receipt Guarantee**:
- Each shard signed with SHA256(shard_data)
- Hash chain links each shard to previous (immutable record)
- Merkle tree enables O(log n) membership proofs
- All receipts written before merge to ensure provenance

---

## 1.2 kgc probe merge

```
ALGORITHM: ProbeCommand.merge
PURPOSE: Re-merge existing shards deterministically (no re-running swarm)

SIGNATURE:
    merge(shardDir: string, [options]) → {
        mergedArtifact: Artifact,
        shardCount: integer,
        duplicates: Array<{agentName, conflict}>,
        warnings: Array<string>
    }

INPUT PARAMETERS:
    <shard-dir>             : Directory containing *.json shards (e.g., probe/out/20250101-120000-abc123/shards/)
    --config <path>         : Config for merge rules (default: .kgc-probe.json)
    --output <dir>          : Output directory for merged artifact (default: ./probe/out/<new-run-id>)
    --format <fmt>          : Output format (default: all) - ttl|json|md
    --on-conflict           : Conflict strategy (default: list)
                              - list: list conflicts, continue
                              - fail: error on first conflict
                              - merge: auto-merge by timestamp (dangerous)

OUTPUT STRUCTURE (same as scan):
    probe/out/<new-run-id>/merged/
    └── world.ttl, index.json, report.md

BEGIN
    // Phase 1: Validate inputs
    IF NOT DirExists(shardDir) THEN
        RETURN error("SHARD_DIR_NOT_FOUND", "Directory does not exist: ${shardDir}")
    END IF

    shardFiles ← GlobFiles(shardDir + "/*.json")
    IF shardFiles.length = 0 THEN
        RETURN error("NO_SHARDS", "No shard files found in ${shardDir}")
    END IF

    // Phase 2: Load and parse shards
    config ← LoadConfig(options.config, options)
    shards ← []
    parseErrors ← []

    FOR EACH file IN shardFiles DO
        TRY
            content ← ReadFile(file)
            shard ← JSON.parse(content)

            // Validate shard structure
            ValidateShard(shard, config.schema)
            shards.append(shard)
        CATCH error
            parseErrors.append({
                file: file,
                error: error.message
            })
        END TRY
    END FOR

    IF parseErrors.length > 0 THEN
        LogError("Failed to parse ${parseErrors.length} shards:")
        FOR EACH pe IN parseErrors DO
            LogError("  ${pe.file}: ${pe.error}")
        END FOR
        IF options.on_conflict = "fail" THEN
            RETURN error("SHARD_PARSE_ERROR", "See logs for details")
        END IF
    END IF

    // Phase 3: Detect conflicts
    conflicts ← DetectConflicts(shards)                       // See: 2.1.1

    IF conflicts.length > 0 THEN
        LogWarning("Detected ${conflicts.length} merge conflicts")

        IF options.on_conflict = "fail" THEN
            RETURN error("MERGE_CONFLICT", JSON.stringify(conflicts))
        ELSE IF options.on_conflict = "merge" THEN
            // Auto-merge by most recent timestamp (dangerous but deterministic)
            LogWarning("Auto-merging conflicts - results may not be semantically valid")
            FOR EACH conflict IN conflicts DO
                winner ← SelectByTimestamp(conflict.candidates)  // Most recent
                LogWarning("  Keeping version from ${winner.agentName} (${winner.timestamp})")
            END FOR
        ELSE
            // Default: list conflicts and continue
            LogWarning("Conflicts will be listed in output")
        END IF
    END IF

    // Phase 4: Merge shards deterministically
    mergedArtifact ← MergeShardsDeterm(shards, config, conflicts)

    // Phase 5: Generate outputs
    newRunId ← GenerateRunId()
    outputDir ← ResolveOutputPath(options.output, newRunId)
    EnsureDir(outputDir + "/merged")

    IF options.format IN {ttl, all} THEN
        ttlGraph ← ConvertToRDF(mergedArtifact, config.ontology)
        WriteFile(outputDir + "/merged/world.ttl", ttlGraph)
    END IF

    IF options.format IN {json, all} THEN
        WriteFile(outputDir + "/merged/index.json", JSON.stringify(mergedArtifact, null, 2))
    END IF

    IF options.format IN {md, all} THEN
        mdReport ← GenerateMarkdownReport(mergedArtifact, config)
        WriteFile(outputDir + "/merged/report.md", mdReport)
    END IF

    // Phase 6: Return result
    RETURN {
        success: conflicts.length = 0 OR options.on_conflict ≠ "fail",
        mergedArtifact: mergedArtifact,
        shardCount: shards.length,
        outputDir: outputDir,
        conflicts: conflicts,
        warnings: parseErrors.map(e => e.error)
    }
END
```

**Use Cases**:
- Reprocess shards with updated merge rules
- Change output format without re-running swarm
- Investigate merge conflicts from previous run
- Generate additional reports

**Conflict Handling**:
- **list** (default): Continue merge, track conflicts, list in output
- **fail**: Stop on first conflict, require manual resolution
- **merge**: Auto-select newest by timestamp (NOT RECOMMENDED for semantic data)

---

## 1.3 kgc probe diff

```
ALGORITHM: ProbeCommand.diff
PURPOSE: Compare two probe runs, emit semantic delta

SIGNATURE:
    diff(oldArtifact: string, newArtifact: string, [options]) → {
        added: Array<{claim, agent, timestamp}>,
        removed: Array<{claim, agent, timestamp}>,
        modified: Array<{claim, oldValue, newValue, agent}>,
        summary: {addedCount, removedCount, modifiedCount, changesetSize}
    }

INPUT PARAMETERS:
    <artifact-old>          : Path to old merged artifact (world.ttl or index.json)
    <artifact-new>          : Path to new merged artifact
    --format <fmt>          : Output format (default: json) - json|md
    --output <path>         : Write diff to file (default: stdout)
    --ignore-timestamps     : Ignore timestamp-only changes (default: false)
    --semantic-only         : Ignore structure/metadata changes (default: false)

OUTPUT FORMATS:
    JSON:
    {
        "delta": {
            "added": [
                {claim: "...", agent: "orchestrator", timestamp: "...", source_line: 42},
                ...
            ],
            "removed": [...],
            "modified": [
                {claim: "...", oldValue: "...", newValue: "...", agent: "runtime"},
                ...
            ]
        },
        "summary": {
            "addedCount": 15,
            "removedCount": 3,
            "modifiedCount": 2,
            "changesetSize": 20,
            "timestamp": "2025-01-01T12:00:00Z"
        }
    }

    MARKDOWN:
    # Probe Diff Report

    Old: 2025-01-01T10:00:00Z (345 claims)
    New: 2025-01-01T12:00:00Z (357 claims)

    ## Summary
    - Added: 15 claims
    - Removed: 3 claims
    - Modified: 2 claims

    ## Added Claims

    ### Agent: orchestrator
    - Claim 1 (source: line 42)
    - Claim 2 (source: line 43)

    ### Agent: runtime
    ...

BEGIN
    // Phase 1: Load artifacts
    oldArtifact ← LoadArtifact(options.artifact_old)           // Supports .ttl or .json
    newArtifact ← LoadArtifact(options.artifact_new)

    IF oldArtifact.claims.length = 0 THEN
        RETURN error("EMPTY_OLD_ARTIFACT", "Old artifact contains no claims")
    END IF

    IF newArtifact.claims.length = 0 THEN
        RETURN error("EMPTY_NEW_ARTIFACT", "New artifact contains no claims")
    END IF

    // Phase 2: Build normalized indices
    oldIndex ← NormalizeClaims(oldArtifact.claims)             // Strip metadata, standardize format
    newIndex ← NormalizeClaims(newArtifact.claims)

    // Phase 3: Compute delta
    added ← []
    removed ← []
    modified ← []

    FOR EACH claim IN newIndex DO
        oldClaim ← oldIndex.find(c => c.id = claim.id)

        IF oldClaim IS null THEN
            added.append({
                claim: claim,
                agent: claim.agent,
                timestamp: claim.timestamp,
                source_line: claim.source_line
            })
        ELSE IF oldClaim.value ≠ claim.value THEN
            IF NOT (options.ignore_timestamps AND OnlyTimestampDiffers(oldClaim, claim)) THEN
                modified.append({
                    claim: claim,
                    oldValue: oldClaim.value,
                    newValue: claim.value,
                    agent: claim.agent
                })
            END IF
        END IF
    END FOR

    FOR EACH claim IN oldIndex DO
        IF NOT newIndex.find(c => c.id = claim.id) THEN
            removed.append({
                claim: claim,
                agent: claim.agent,
                timestamp: claim.timestamp
            })
        END IF
    END FOR

    // Phase 4: Generate output
    delta ← {
        added: added,
        removed: removed,
        modified: modified,
        summary: {
            addedCount: added.length,
            removedCount: removed.length,
            modifiedCount: modified.length,
            changesetSize: added.length + removed.length + modified.length,
            timestamp: new Date().toISOString(),
            oldSize: oldArtifact.claims.length,
            newSize: newArtifact.claims.length
        }
    }

    output ← ""
    IF options.format = "json" THEN
        output ← JSON.stringify(delta, null, 2)
    ELSE IF options.format = "md" THEN
        output ← GenerateMarkdownDiff(delta, oldArtifact, newArtifact)
    END IF

    IF options.output THEN
        WriteFile(options.output, output)
    ELSE
        PrintToStdout(output)
    END IF

    RETURN delta
END
```

**Change Categories**:
- **Added**: Claim present in new but not old (new capability/knowledge)
- **Removed**: Claim present in old but not new (deprecated/removed capability)
- **Modified**: Same claim ID but different value (updated capability details)

---

## 1.4 kgc probe report

```
ALGORITHM: ProbeCommand.report
PURPOSE: Generate human/machine-readable reports from merged artifact

SIGNATURE:
    report(artifactPath: string, [options]) → {
        format: string,
        outputPath: string,
        sections: Array<string>,
        stats: {claimCount, agentCount, coverage}
    }

INPUT PARAMETERS:
    <merged-artifact>       : Path to merged artifact (world.ttl or index.json)
    --format <fmt>          : Output format (default: md)
                              - md: Markdown (Diataxis - How-to, Reference, Explanation)
                              - json: Machine-readable structure
                              - ttl: RDF Turtle format
                              - pdf: PDF report (via Pandoc or headless Chrome)
    --output <path>         : Output file path (default: ./report.<ext>)
    --style <name>          : Report style (default: technical)
                              - technical: Detailed, capability index
                              - executive: High-level summary
                              - audit: Compliance + provenance focus
    --include-provenance    : Include agent/timestamp metadata (default: true for audit)
    --max-depth <n>         : Max nesting depth in reference section (default: 3)

OUTPUT FORMATS:

    MARKDOWN (Default - Diataxis Structure):
    ```markdown
    # KGC Probe Report

    Generated: 2025-01-01T12:00:00Z | Run ID: 20250101-120000-abc123

    ## Quick Start (Tutorial)

    How to use the discovered capabilities...

    ## How-To Guide

    Common patterns and usage examples for each agent...

    ## Reference

    Complete capability inventory:
    - orchestrator: [cap-1], [cap-2], ...
    - runtime: [cap-3], [cap-4], ...

    ## Explanation

    Design decisions, architecture insights...

    ## Statistics

    - Total claims: 345
    - Coverage: 95% (estimated)
    - Agents: 10
    - Last updated: 2025-01-01T12:00:00Z
    ```

    JSON (Machine-Readable):
    ```json
    {
        "metadata": {
            "runId": "20250101-120000-abc123",
            "timestamp": "2025-01-01T12:00:00Z",
            "format": "json"
        },
        "summary": {
            "claimCount": 345,
            "agentCount": 10,
            "coverage": 0.95
        },
        "capabilities": [
            {
                "id": "cap-1",
                "agent": "orchestrator",
                "name": "Snapshot Creation",
                "description": "...",
                "usageExample": "...",
                "related": ["cap-2", "cap-3"]
            },
            ...
        ],
        "agents": [
            {
                "id": "01",
                "name": "orchestrator",
                "capabilityCount": 15,
                "timestamp": "2025-01-01T12:00:00Z"
            },
            ...
        ]
    }
    ```

    TTL (RDF):
    ```turtle
    @prefix kgc: <https://unrdf.io/kgc/probe/> .
    @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

    kgc:report-20250101-120000
        a kgc:ProbeReport ;
        kgc:hasCapability kgc:cap-1, kgc:cap-2 ;
        kgc:generatedBy kgc:agent-orchestrator ;
        kgc:timestamp "2025-01-01T12:00:00Z"^^xsd:dateTime .

    kgc:cap-1
        a kgc:Capability ;
        kgc:name "Snapshot Creation" ;
        kgc:description "..." ;
        kgc:agent kgc:agent-01 .
    ```

BEGIN
    // Phase 1: Load artifact
    artifact ← LoadArtifact(options.artifact_path)

    IF artifact IS null THEN
        RETURN error("ARTIFACT_NOT_FOUND", "Cannot load artifact: ${options.artifact_path}")
    END IF

    // Phase 2: Build report structure
    report ← {
        metadata: {
            runId: artifact.runId,
            timestamp: artifact.timestamp,
            format: options.format,
            style: options.style
        },
        summary: BuildSummary(artifact),
        sections: []
    }

    // Phase 3: Generate sections based on format
    IF options.format = "md" THEN
        report.sections.append(GenerateTutorialSection(artifact))
        report.sections.append(GenerateHowToGuideSection(artifact))
        report.sections.append(GenerateReferenceSection(artifact, options.max_depth))
        report.sections.append(GenerateExplanationSection(artifact))
        report.sections.append(GenerateStatisticsSection(artifact))

        output ← RenderMarkdownReport(report, options.style)
    ELSE IF options.format = "json" THEN
        // Flatten sections into JSON structure
        report.capabilities ← artifact.claims.map(c => ({
            id: c.id,
            agent: c.agent,
            name: c.name,
            description: c.description,
            usageExample: c.example,
            related: FindRelatedClaims(c, artifact.claims)
        }))
        report.agents ← BuildAgentIndex(artifact)

        output ← JSON.stringify(report, null, 2)
    ELSE IF options.format = "ttl" THEN
        output ← ConvertArtifactToTTL(artifact, options.include_provenance)
    ELSE IF options.format = "pdf" THEN
        // Convert to PDF via pandoc or headless browser
        mdReport ← RenderMarkdownReport(report, options.style)
        output ← ConvertMarkdownToPDF(mdReport)
    END IF

    // Phase 4: Write output
    outputPath ← ResolveOutputPath(options.output, options.format)
    WriteFile(outputPath, output)

    RETURN {
        success: true,
        format: options.format,
        outputPath: outputPath,
        sections: report.sections.map(s => s.title),
        stats: {
            claimCount: artifact.claims.length,
            agentCount: artifact.agents.length,
            coverage: CalculateCoverage(artifact)
        }
    }
END
```

**Diataxis Structure** (for Markdown):
1. **Tutorial**: Step-by-step guide for new users
2. **How-To**: Problem-solution patterns (reference-like but practical)
3. **Reference**: Complete API/capability listing
4. **Explanation**: Rationale, design decisions, deep dives

---

## 1.5 kgc probe verify

```
ALGORITHM: ProbeCommand.verify
PURPOSE: Validate artifact integrity, verify receipts, check merkle chain

SIGNATURE:
    verify(artifactPath: string, [options]) → {
        valid: boolean,
        checks: {hashChainValid, merkleValid, schemaValid, cryptoValid},
        mismatches: Array<{check, expected, actual}>,
        details: Array<string>
    }

INPUT PARAMETERS:
    <artifact>              : Path to artifact (world.ttl, index.json, or entire probe/out/)
    --check-merkle          : Verify merkle tree proofs (default: true)
    --check-schema          : Validate against Zod schema (default: true)
    --check-crypto          : Verify hash signatures (requires keys.json) (default: false)
    --receipt-dir <path>    : Path to receipts/ directory (auto-detected if artifact is dir)
    --strict                : Fail on any mismatch (default: true)

OUTPUT:
    {
        "valid": true,
        "checks": {
            "hashChainValid": true,
            "merkleValid": true,
            "schemaValid": true,
            "cryptoValid": false  ← Not checked (no keys)
        },
        "mismatches": [],
        "details": [
            "Hash chain verified: 10 shards, root=0xabcd...",
            "Merkle tree verified: root=0x1234..., 10 membership proofs OK",
            "Schema validation: All 10 shards conform to schema",
            "Crypto verification: SKIPPED (keys.json not found)"
        ],
        "timestamp": "2025-01-01T12:00:00Z"
    }

BEGIN
    // Phase 1: Locate artifact and receipts
    artifactPath ← options.artifact
    receiptDir ← options.receipt_dir

    // If artifact is directory, look for receipts/ subdirectory
    IF IsDirectory(artifactPath) THEN
        receiptDir ← artifactPath + "/receipts"
        // Find merged artifact
        IF FileExists(artifactPath + "/merged/index.json") THEN
            artifactPath ← artifactPath + "/merged/index.json"
        ELSE IF FileExists(artifactPath + "/merged/world.ttl") THEN
            artifactPath ← artifactPath + "/merged/world.ttl"
        END IF
    END IF

    IF NOT FileExists(artifactPath) THEN
        RETURN error("ARTIFACT_NOT_FOUND", "No artifact at ${artifactPath}")
    END IF

    IF NOT DirExists(receiptDir) THEN
        RETURN error("RECEIPT_DIR_NOT_FOUND", "No receipts at ${receiptDir}")
    END IF

    // Phase 2: Load artifact and receipts
    artifact ← LoadArtifact(artifactPath)
    hashChain ← LoadJSON(receiptDir + "/chain.json")
    merkleTree ← LoadJSON(receiptDir + "/merkle.json") IF FileExists(receiptDir + "/merkle.json")

    // Phase 3: Verify hash chain
    checks ← {
        hashChainValid: false,
        merkleValid: false,
        schemaValid: false,
        cryptoValid: false
    }
    mismatches ← []
    details ← []

    // Check hash chain integrity
    IF options.check_merkle OR hashChain THEN
        chainValid ← true
        previousHash ← null

        FOR EACH entry IN hashChain.chain DO
            // Verify link
            IF entry.previousHash ≠ previousHash THEN
                chainValid ← false
                mismatches.append({
                    check: "hash_chain_link",
                    entry: entry.agentName,
                    expected: previousHash,
                    actual: entry.previousHash
                })
            END IF

            // Verify entry hash (recompute)
            entryClone ← COPY(entry)
            DELETE entryClone.hash
            computedHash ← SHA256(JSON.stringify(entryClone))

            IF computedHash ≠ entry.hash THEN
                chainValid ← false
                mismatches.append({
                    check: "hash_chain_entry",
                    entry: entry.agentName,
                    expected: entry.hash,
                    actual: computedHash
                })
            END IF

            previousHash ← entry.hash
        END FOR

        checks.hashChainValid ← chainValid

        IF chainValid THEN
            details.append("Hash chain verified: ${hashChain.chain.length} entries, root=${hashChain.root}")
        ELSE
            details.append("Hash chain INVALID: ${mismatches.length} mismatches found")
        END IF
    END IF

    // Check merkle tree
    IF options.check_merkle AND merkleTree THEN
        merkleValid ← true

        // Verify merkle root
        // Verify membership proofs
        FOR EACH proof IN merkleTree.proofs DO
            isValid ← VerifyMembershipProof(proof, merkleTree.root)

            IF NOT isValid THEN
                merkleValid ← false
                mismatches.append({
                    check: "merkle_proof",
                    agent: proof.agentName,
                    root: merkleTree.root
                })
            END IF
        END FOR

        checks.merkleValid ← merkleValid

        IF merkleValid THEN
            details.append("Merkle tree verified: root=${merkleTree.root}, ${merkleTree.proofs.length} proofs OK")
        ELSE
            details.append("Merkle tree INVALID: ${merkleTree.proofs.length - validProofs} proof failures")
        END IF
    END IF

    // Check schema validation
    IF options.check_schema THEN
        schemaValid ← true
        config ← LoadConfig(".kgc-probe.json", {})

        FOR EACH entry IN hashChain.chain DO
            shardPath ← receiptDir + "/../shards/" + entry.agentName + ".json"

            IF FileExists(shardPath) THEN
                shard ← LoadJSON(shardPath)
                TRY
                    ValidateShard(shard, config.schema)
                CATCH error
                    schemaValid ← false
                    mismatches.append({
                        check: "schema_validation",
                        agent: entry.agentName,
                        error: error.message
                    })
                END TRY
            END IF
        END FOR

        checks.schemaValid ← schemaValid

        IF schemaValid THEN
            details.append("Schema validation: ${hashChain.chain.length} shards conform to schema")
        ELSE
            details.append("Schema validation FAILED: ${mismatches.filter(m => m.check = 'schema_validation').length} errors")
        END IF
    END IF

    // Check crypto signatures (if keys available)
    IF options.check_crypto THEN
        IF FileExists(receiptDir + "/keys.json") THEN
            keys ← LoadJSON(receiptDir + "/keys.json")
            cryptoValid ← true

            FOR EACH entry IN hashChain.chain DO
                signature ← entry.signature IF entry.signature

                IF signature THEN
                    publicKey ← keys[entry.agentName]
                    IF VerifySignature(entry.hash, signature, publicKey) THEN
                        // Valid
                    ELSE
                        cryptoValid ← false
                        mismatches.append({
                            check: "crypto_signature",
                            agent: entry.agentName
                        })
                    END IF
                END IF
            END FOR

            checks.cryptoValid ← cryptoValid
            details.append("Crypto verification: " + (cryptoValid ? "VALID" : "INVALID"))
        ELSE
            details.append("Crypto verification: SKIPPED (keys.json not found)")
        END IF
    END IF

    // Phase 4: Return result
    allValid ← mismatches.length = 0

    IF NOT allValid AND options.strict THEN
        RETURN error("VERIFICATION_FAILED", JSON.stringify(mismatches))
    END IF

    RETURN {
        valid: allValid,
        checks: checks,
        mismatches: mismatches,
        details: details,
        timestamp: new Date().toISOString()
    }
END
```

**Verification Layers**:
1. **Hash Chain**: Sequential hash linking (O(n) integrity check)
2. **Merkle Tree**: O(log n) membership proofs + root verification
3. **Schema**: Zod validation against config.schema
4. **Crypto** (optional): RSA/ECDSA signature verification

---

## Part 2: Data Structures & Algorithms

### 2.1 Merge Algorithms

#### 2.1.1 Conflict Detection

```
ALGORITHM: DetectConflicts
INPUT: shards (Array<Shard>)
OUTPUT: conflicts (Array<{claimId, candidates}>)

BEGIN
    claimMap ← Map<claimId, Array<{agentName, value, timestamp}>>

    FOR EACH shard IN shards DO
        FOR EACH claim IN shard.output.claims DO
            claimId ← claim.id

            IF NOT claimMap.has(claimId) THEN
                claimMap.set(claimId, [])
            END IF

            claimMap.get(claimId).append({
                agentName: shard.agentName,
                value: claim.value,
                timestamp: shard.timestamp,
                hash: shard.hash
            })
        END FOR
    END FOR

    // Identify conflicts (claim present in multiple shards with different values)
    conflicts ← []
    FOR EACH [claimId, candidates] IN claimMap DO
        IF candidates.length > 1 THEN
            uniqueValues ← Set()
            FOR EACH candidate IN candidates DO
                uniqueValues.add(JSON.stringify(candidate.value))  // Normalized comparison
            END FOR

            IF uniqueValues.size > 1 THEN
                conflicts.append({
                    claimId: claimId,
                    candidates: candidates,
                    valueCount: uniqueValues.size
                })
            END IF
        END IF
    END FOR

    RETURN conflicts
END
```

#### 2.1.2 Merkle Tree Construction

```
ALGORITHM: BuildMerkleTree
INPUT: shards (Array<Shard>)
OUTPUT: merkleTree (Object with root, structure, height)

PURPOSE: Enable O(log n) membership proofs for shard integrity

BEGIN
    // Step 1: Compute leaf hashes (one per shard)
    leaves ← []
    FOR EACH shard IN shards DO
        leafHash ← SHA256(shard.agentName + "||" + shard.hash)
        leaves.append({
            agentName: shard.agentName,
            hash: leafHash,
            shardHash: shard.hash
        })
    END FOR

    // Step 2: Build tree bottom-up (binary merkle tree)
    // If odd number of leaves, duplicate last leaf
    IF leaves.length MOD 2 = 1 THEN
        leaves.append(leaves[-1])  // Duplicate last
    END IF

    currentLevel ← leaves
    levels ← [leaves]

    WHILE currentLevel.length > 1 DO
        nextLevel ← []

        FOR i ← 0 TO currentLevel.length STEP 2 DO
            left ← currentLevel[i]
            right ← currentLevel[i+1]

            parentHash ← SHA256(left.hash + right.hash)
            nextLevel.append({
                hash: parentHash,
                left: left.hash,
                right: right.hash,
                leftNode: left,
                rightNode: right
            })
        END FOR

        levels.append(nextLevel)
        currentLevel ← nextLevel
    END WHILE

    // Step 3: Extract root
    root ← currentLevel[0].hash

    RETURN {
        root: root,
        leaves: leaves,
        levels: levels,
        height: levels.length,
        leafCount: leaves.length
    }
END
```

#### 2.1.3 Deterministic Merge Algorithm

```
ALGORITHM: MergeShardsDeterm
INPUT: shards (Array<Shard>), config (Config), conflicts (Array<Conflict>)
OUTPUT: mergedArtifact (Artifact)

PURPOSE: Deterministically combine shards into single RDF artifact

BEGIN
    // Step 1: Initialize merged artifact
    merged ← {
        metadata: {
            mergedAt: GetCurrentTime(),
            shardCount: shards.length,
            version: "1.0.0"
        },
        agents: [],
        claims: [],
        statistics: {}
    }

    // Step 2: Process each shard in topological order (deterministic)
    sortedShards ← Sort(shards, (a, b) => a.agentName.localeCompare(b.agentName))

    claimSet ← Set<string>()  // Track claim IDs to detect duplicates

    FOR EACH shard IN sortedShards DO
        merged.agents.append({
            id: shard.agentId,
            name: shard.agentName,
            timestamp: shard.timestamp,
            shardHash: shard.hash,
            claimCount: shard.output.claims.length
        })

        // Step 3: Merge claims with conflict resolution
        FOR EACH claim IN shard.output.claims DO
            claimId ← claim.id

            // Check if claim already exists
            IF claimSet.has(claimId) THEN
                // Conflict detected
                existingClaim ← merged.claims.find(c => c.id = claimId)

                // Get conflict resolution rule from config
                rule ← config.mergeRules[claimId] OR config.mergeRules.default OR "keep-first"

                IF rule = "keep-first" THEN
                    // Skip (already have claim from earlier shard)
                    CONTINUE
                ELSE IF rule = "keep-last" THEN
                    // Replace with newer version
                    mergedIdx ← merged.claims.findIndex(c => c.id = claimId)
                    merged.claims[mergedIdx] ← {
                        ...claim,
                        mergeNote: "Overwritten by " + shard.agentName,
                        sources: [existingClaim.sources[0], shard.agentName]
                    }
                ELSE IF rule = "merge-both" THEN
                    // Create composite claim
                    existingClaim.sources ← [existingClaim.sources[0], shard.agentName]
                    existingClaim.mergeNote ← "Merged from multiple sources"
                    CONTINUE
                ELSE IF rule = "fail" THEN
                    // Treat as error (should have been caught in validation)
                    LogError("Conflict in claim ${claimId} not resolved before merge")
                END IF
            ELSE
                // No conflict, add claim
                claimSet.add(claimId)
                merged.claims.append({
                    ...claim,
                    id: claimId,
                    source: shard.agentName,
                    timestamp: shard.timestamp,
                    shardHash: shard.hash
                })
            END IF
        END FOR
    END FOR

    // Step 4: Compute statistics
    merged.statistics ← {
        totalClaims: merged.claims.length,
        totalAgents: merged.agents.length,
        claimsByAgent: merged.agents.map(a => ({
            agent: a.name,
            count: merged.claims.filter(c => c.source = a.name).length
        })),
        lastUpdated: Sort(merged.agents, (a, b) => b.timestamp.localeCompare(a.timestamp))[0].timestamp
    }

    RETURN merged
END
```

### 2.2 Report Generation

#### 2.2.1 Markdown Report (Diataxis Format)

```
ALGORITHM: GenerateMarkdownReport
INPUT: artifact (Artifact), config (Config)
OUTPUT: markdownText (string)

BEGIN
    sections ← []

    // Title and metadata
    header ← "# KGC Probe Report\n\n"
    header ← header + "Generated: " + artifact.metadata.mergedAt + "\n"
    header ← header + "Run ID: " + artifact.metadata.runId + "\n"
    header ← header + artifact.statistics.totalClaims + " claims from " + artifact.statistics.totalAgents + " agents\n\n"
    sections.append(header)

    // Tutorial section
    sections.append("## Tutorial: Getting Started\n\n")
    sections.append(GenerateTutorialContent(artifact, config))
    sections.append("\n\n")

    // How-To section
    sections.append("## How-To Guide\n\n")
    sections.append(GenerateHowToContent(artifact, config))
    sections.append("\n\n")

    // Reference section (indexed by agent)
    sections.append("## Reference\n\n")
    sections.append("Complete capability inventory:\n\n")

    FOR EACH agent IN artifact.agents DO
        sections.append("### " + agent.name + "\n\n")
        agentClaims ← artifact.claims.filter(c => c.source = agent.name)

        FOR EACH claim IN agentClaims DO
            sections.append("- **" + claim.name + "**: " + claim.description + "\n")
            IF claim.example THEN
                sections.append("  Usage: `" + claim.example + "`\n")
            END IF
        END FOR

        sections.append("\n")
    END FOR

    // Explanation section
    sections.append("## Explanation\n\n")
    sections.append(GenerateExplanationContent(artifact, config))
    sections.append("\n\n")

    // Statistics
    sections.append("## Statistics\n\n")
    sections.append("| Metric | Value |\n")
    sections.append("| --- | --- |\n")
    sections.append("| Total Claims | " + artifact.statistics.totalClaims + " |\n")
    sections.append("| Total Agents | " + artifact.statistics.totalAgents + " |\n")
    sections.append("| Last Updated | " + artifact.statistics.lastUpdated + " |\n")
    sections.append("| Coverage | " + CalculateCoverage(artifact) * 100 + "% |\n")

    RETURN sections.join("")
END
```

---

## Part 3: Integration with KGC-CLI

### 3.1 Extension Registration

```
// file: packages/kgc-cli/src/extensions/kgc-probe.mjs

import { z } from 'zod';

// Argument schemas for each command
const ScanArgsSchema = z.object({
  config: z.string().optional().describe('Config file path'),
  output: z.string().optional().describe('Output directory'),
  timeout: z.number().int().positive().optional().default(30000),
  parallel: z.number().int().positive().optional().default(10),
  validate: z.boolean().optional().default(true),
  format: z.enum(['ttl', 'json', 'md', 'all']).optional().default('all'),
  'no-receipts': z.boolean().optional().default(false),
  merkle: z.boolean().optional().default(true)
});

const MergeArgsSchema = z.object({
  shardDir: z.string().describe('Shard directory path'),
  config: z.string().optional(),
  output: z.string().optional(),
  format: z.enum(['ttl', 'json', 'md', 'all']).optional().default('all'),
  'on-conflict': z.enum(['list', 'fail', 'merge']).optional().default('list')
});

const DiffArgsSchema = z.object({
  oldArtifact: z.string(),
  newArtifact: z.string(),
  format: z.enum(['json', 'md']).optional().default('json'),
  output: z.string().optional(),
  'ignore-timestamps': z.boolean().optional().default(false),
  'semantic-only': z.boolean().optional().default(false)
});

const ReportArgsSchema = z.object({
  artifactPath: z.string(),
  format: z.enum(['md', 'json', 'ttl', 'pdf']).optional().default('md'),
  output: z.string().optional(),
  style: z.enum(['technical', 'executive', 'audit']).optional().default('technical'),
  'include-provenance': z.boolean().optional().default(true),
  'max-depth': z.number().int().positive().optional().default(3)
});

const VerifyArgsSchema = z.object({
  artifactPath: z.string(),
  'check-merkle': z.boolean().optional().default(true),
  'check-schema': z.boolean().optional().default(true),
  'check-crypto': z.boolean().optional().default(false),
  'receipt-dir': z.string().optional(),
  strict: z.boolean().optional().default(true)
});

const extension = {
  id: '@unrdf/kgc-probe',
  description: 'KGC Probe - Deterministic codebase analysis with receipts & verification',

  nouns: {
    probe: {
      description: 'Manage KGC probe runs (scans, merges, diffs, reports, verification)',
      verbs: {
        scan: {
          description: 'Run swarm agents, capture shards, generate merged artifact',
          argsSchema: ScanArgsSchema,
          handler: async (args) => {
            // Implementation imports from @unrdf/kgc-probe
            const { runProbeScran } = await import('@unrdf/kgc-probe/scanner');
            return runProbeScan(args);
          },
          meta: {
            examples: [
              'kgc probe scan --output ./results',
              'kgc probe scan --config .kgc-probe.json --format md'
            ]
          }
        },

        merge: {
          description: 'Re-merge existing shards deterministically',
          argsSchema: MergeArgsSchema,
          handler: async (args) => {
            const { mergeShards } = await import('@unrdf/kgc-probe/merger');
            return mergeShards(args.shardDir, args);
          }
        },

        diff: {
          description: 'Compare two probe artifacts, emit delta',
          argsSchema: DiffArgsSchema,
          handler: async (args) => {
            const { diffArtifacts } = await import('@unrdf/kgc-probe/differ');
            return diffArtifacts(args.oldArtifact, args.newArtifact, args);
          }
        },

        report: {
          description: 'Generate human/machine-readable reports',
          argsSchema: ReportArgsSchema,
          handler: async (args) => {
            const { generateReport } = await import('@unrdf/kgc-probe/reporter');
            return generateReport(args.artifactPath, args);
          }
        },

        verify: {
          description: 'Validate artifact integrity and verify receipts',
          argsSchema: VerifyArgsSchema,
          handler: async (args) => {
            const { verifyArtifact } = await import('@unrdf/kgc-probe/verifier');
            return verifyArtifact(args.artifactPath, args);
          }
        }
      }
    }
  },

  priority: 20,

  guards: {
    refusals: ['destructive'],  // No destructive operations
    preconditions: async () => {
      // Check @unrdf/kgc-probe is available
      try {
        await import('@unrdf/kgc-probe');
        return true;
      } catch {
        throw new Error('@unrdf/kgc-probe package not found');
      }
    }
  },

  receipts: {
    success: {
      runId: 'string',
      outputDir: 'string',
      shardCount: 'integer',
      timestamp: 'string'
    },
    error: {
      code: 'string',
      message: 'string',
      details: 'any'
    }
  }
};

export default extension;
```

### 3.2 Configuration File Format

```json
// .kgc-probe.json (in project root)

{
  "version": "1.0.0",

  "allowlist": [
    "orchestrator",
    "runtime",
    "cache",
    "federation",
    "consensus",
    "hooks",
    "domain",
    "claude",
    "substrate",
    "knowledge-engine"
  ],

  "guards": {
    "orchestrator": {
      "timeLimit": 30000,
      "memoryLimit": "512MB",
      "refusals": ["destructive", "external-calls"]
    },
    "runtime": {
      "timeLimit": 30000,
      "memoryLimit": "256MB"
    },
    "*": {
      "timeLimit": 30000
    }
  },

  "mergeRules": {
    "default": "keep-first",
    "capability.version": "keep-last",
    "metadata.timestamp": "keep-last",
    "claims.#schema": "merge-both"
  },

  "schema": {
    "type": "object",
    "properties": {
      "agentId": {"type": "string"},
      "claims": {
        "type": "array",
        "items": {
          "type": "object",
          "properties": {
            "id": {"type": "string"},
            "name": {"type": "string"},
            "value": {}
          },
          "required": ["id"]
        }
      }
    },
    "required": ["agentId", "claims"]
  },

  "ontology": {
    "namespace": "https://unrdf.io/kgc/probe/",
    "prefixes": {
      "kgc": "https://unrdf.io/kgc/",
      "rdf": "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
      "rdfs": "http://www.w3.org/2000/01/rdf-schema#"
    }
  },

  "reports": {
    "diataxisStructure": true,
    "includeExamples": true,
    "maxDepth": 3
  }
}
```

---

## Part 4: Error Handling

### 4.1 Error Categories

```
ERROR HANDLING MATRIX:

Category            Code                    Recovery
─────────────────────────────────────────────────────────
Input Validation    INVALID_FORMAT          Suggest valid formats
                    INVALID_CONFIG          Show config schema
                    SHARD_DIR_NOT_FOUND     Check path
                    NO_SHARDS               Run scan first

Execution           SWARM_TIMEOUT           Increase --timeout
                    AGENT_FAILED            See meta/metrics.json
                    SWARM_COMPLETE_FAILURE  Check guard violations

Merge Conflicts     MERGE_CONFLICT          Use --on-conflict=list
                    DUPLICATE_CLAIMS        Review delta.json

Verification        VERIFICATION_FAILED     Check receipts/chain.json
                    HASH_MISMATCH           Shard corrupted
                    MERKLE_INVALID          Proof generation failed

File I/O            ARTIFACT_NOT_FOUND      Check path
                    PERMISSION_DENIED       Check file permissions
                    DISK_FULL               Free disk space
```

### 4.2 Error Response Envelope

```javascript
// Standard error format (all commands)

{
  "success": false,
  "code": "ERROR_CODE",
  "message": "Human-readable message",
  "details": {
    "context": "...",
    "suggestion": "How to fix"
  },
  "timestamp": "2025-01-01T12:00:00Z"
}

// Example: Merge conflict error
{
  "success": false,
  "code": "MERGE_CONFLICT",
  "message": "3 merge conflicts detected",
  "details": {
    "conflicts": [
      {
        "claimId": "cap-42",
        "candidates": [
          {"agent": "orchestrator", "value": "v1.0"},
          {"agent": "runtime", "value": "v2.0"}
        ]
      },
      ...
    ],
    "suggestion": "Use --on-conflict=merge to auto-resolve by timestamp"
  }
}
```

---

## Part 5: Pseudocode Quality Checklist

### Specification Completeness

- [x] All 5 commands defined with full pseudocode
- [x] Input parameters documented with types
- [x] Output structures specified (JSON/YAML examples)
- [x] All major algorithms pseudocoded (merge, diff, report, verify)
- [x] Data structures with clear ownership (shards, artifacts, receipts)
- [x] Guard enforcement (allowlists, timeouts, refusals)
- [x] Error handling with recovery paths
- [x] Integration points with kgc-cli (extension registration)
- [x] Configuration file format with examples
- [x] Cryptographic verification (hash chains, merkle trees)

### Algorithmic Properties

| Algorithm | Time | Space | Notes |
|-----------|------|-------|-------|
| DetectConflicts | O(n*m) | O(c) | n=shards, m=claims/shard, c=conflict count |
| BuildMerkleTree | O(n log n) | O(n) | Binary tree, O(log n) height |
| MergeShardsDeterm | O(n*m) | O(n*m) | Sorts by name for determinism |
| GenerateMarkdownReport | O(n*m) | O(n*m) | n=agents, m=claims/agent |
| VerifyHashChain | O(n) | O(1) | Sequential verification |

### Security Considerations

- **Hash Chain**: SHA256 links prevent tampering with shard order
- **Merkle Tree**: O(log n) proofs enable external verification
- **Schema Validation**: Zod prevents injection attacks
- **Crypto (Optional)**: RSA/ECDSA signatures for non-repudiation
- **Guard Enforcement**: Allowlists + timeout limits prevent resource exhaustion

---

## Part 6: Implementation Roadmap

### Phase 1: Core Scanner (Week 1-2)
- [x] Pseudocode specification
- [ ] Command: `kgc probe scan`
- [ ] Shard collection from 10 agents
- [ ] Hash chain generation
- [ ] Output directory structure

### Phase 2: Merge & Conflict Resolution (Week 2-3)
- [ ] Command: `kgc probe merge`
- [ ] Conflict detection algorithm
- [ ] Deterministic merge logic
- [ ] Conflict reporting

### Phase 3: Reports & Diffing (Week 3-4)
- [ ] Command: `kgc probe report` (MD, JSON, TTL formats)
- [ ] Diataxis structure generation
- [ ] Command: `kgc probe diff`
- [ ] Delta computation

### Phase 4: Verification & Receipts (Week 4-5)
- [ ] Command: `kgc probe verify`
- [ ] Merkle tree implementation
- [ ] Signature verification (crypto layer)
- [ ] Receipt chain validation

### Phase 5: Integration & Testing (Week 5-6)
- [ ] Register extension in kgc-cli
- [ ] End-to-end tests
- [ ] Error handling validation
- [ ] Documentation

---

## Questions for Adversarial Review

**Before implementation begins, address these questions**:

1. **Hash Chain Ordering**: Should hash chain order match agent execution order or alphabetical?
   - **Decision**: Alphabetical by agent name (deterministic, reproducible)

2. **Conflict Resolution**: When two agents report conflicting claims, which wins?
   - **Decision**: Config file specifies rule per claim ID (default: keep-first)
   - **Fallback**: User must run with --on-conflict=list to review

3. **Shard Size Limits**: Should we enforce max shard size?
   - **Decision**: Yes, warn if >100MB, error if >1GB
   - **Config**: shard.maxSize in .kgc-probe.json

4. **Merkle Tree Depth**: For 10 agents, merkle tree height is 4. Sufficient?
   - **Decision**: Yes, 2^4 = 16 capacity. Add future-proofing to 32 agents (5 levels)

5. **Report Format Defaults**: Should all formats be generated or just requested?
   - **Decision**: Only requested via --format. Default is --format=all (all three: MD, JSON, TTL)

6. **Verification Without Receipts**: Can we verify artifact without receipts/?
   - **Decision**: No. Receipts are essential. Error code: RECEIPT_DIR_NOT_FOUND

7. **Caching**: Should scan cache agent results?
   - **Decision**: No caching in scan (always fresh). User can manually re-merge with --merge for experiments.

---

## Conclusion

This specification provides a complete, executable blueprint for KGC Probe CLI implementation. Key design principles:

1. **Determinism**: Alphabetical ordering, fixed hash algorithms, reproducible merges
2. **Transparency**: Hash chains + merkle trees provide cryptographic proof
3. **Extensibility**: Config file + extension registry allow customization
4. **Usability**: Diataxis structure for reports, clear error messages
5. **Reliability**: Guard enforcement, timeout limits, shard validation

The pseudocode is language-agnostic and can be implemented in any language (Node.js recommended for integration with kgc-cli).

