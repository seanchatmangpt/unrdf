# YAWL Reference

This section is a precise specification. Use it when you need to know the exact shape of an API, the valid values for a field, or the semantics of a particular behaviour.

---

## Reference documents

| Document                                | Covers                                                                                            |
| --------------------------------------- | ------------------------------------------------------------------------------------------------- |
| [Workflow Schema](./workflow-schema.md) | `WorkflowSpecSchema`, `TaskDefSchema`, `FlowDefSchema`, `createWorkflow`, `WorkflowEngine` config |
| [Task API](./task-api.md)               | `TaskDefinitionSchema`, `TaskStatus`, `VALID_TRANSITIONS`, `ENGINE_EVENTS`, lifecycle methods     |
| [XOR Split/Join](./xor-split-join.md)   | `SPLIT_TYPE`, `JOIN_TYPE`, `PATTERNS`, all builder functions, validation rules                    |

---

## Package entry points

| Import path             | What it exposes                                                              |
| ----------------------- | ---------------------------------------------------------------------------- |
| `@unrdf/yawl`           | All public exports (main entry point)                                        |
| `@unrdf/yawl/api`       | High-level workflow API (`createWorkflow`, `createCase`, `enableTask`, etc.) |
| `@unrdf/yawl/ontology`  | RDF ontology: namespaces, class URIs, SPARQL utilities                       |
| `@unrdf/yawl/store`     | RDF store operations for cases and work items                                |
| `@unrdf/yawl/types`     | Type guards, status enums, transition maps                                   |
| `@unrdf/yawl/schemas`   | All Zod schemas as a flat export                                             |
| `@unrdf/yawl/hooks`     | YAWL-Hooks integration (`createYAWLPolicyPack`)                              |
| `@unrdf/yawl/resources` | Resource pool, allocation, role-based SPARQL eligibility                     |
| `@unrdf/yawl/receipt`   | Receipt generation and BLAKE3 chain verification                             |

---

## Namespace URIs

| Constant           | Value                             |
| ------------------ | --------------------------------- |
| `YAWL`             | `http://unrdf.org/yawl#`          |
| `YAWL_CASE`        | `http://unrdf.org/yawl/case#`     |
| `YAWL_TASK`        | `http://unrdf.org/yawl/task#`     |
| `YAWL_WORK`        | `http://unrdf.org/yawl/workitem#` |
| `YAWL_NS.BASE`     | `http://yawl.io/`                 |
| `YAWL_NS.WORKFLOW` | `http://yawl.io/workflow/`        |
| `YAWL_NS.TASK`     | `http://yawl.io/task/`            |
| `YAWL_NS.CASE`     | `http://yawl.io/case/`            |
