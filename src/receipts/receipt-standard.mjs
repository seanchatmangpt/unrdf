/**
 * @fileoverview Universal Receipt Standard - Main export file
 *
 * **Purpose**: Define a universal receipt format that works across all 42+ packages:
 * - Admission receipts (GOS gates)
 * - Test execution receipts
 * - Build/compilation receipts
 * - Deployment receipts
 * - Projection receipts (documentation generation)
 * - Query/operation receipts (SPARQL, workflow)
 *
 * **Design Principles**:
 * 1. Single canonical format with extensible type-specific fields
 * 2. Deterministic hashing via canonical JSON serialization
 * 3. Chain linkage via beforeHash for temporal ordering
 * 4. Merkle batching for efficient verification
 * 5. JSON-LD and TTL serialization for RDF compatibility
 *
 * @module receipts/receipt-standard
 */

// Re-export schemas
export {
  RECEIPT_TYPES,
  DECISION_OUTCOMES,
  SEVERITY_LEVELS,
  ProvenanceSchema,
  ToolchainSchema,
  InputSpecSchema,
  OutputSpecSchema,
  CheckResultSchema,
  MetricsSchema,
  ViolationSchema,
  AdmissionExtensionSchema,
  TestExtensionSchema,
  BuildExtensionSchema,
  DeploymentExtensionSchema,
  ProjectionExtensionSchema,
  QueryExtensionSchema,
  WorkflowExtensionSchema,
  ExtensionSchema,
  UniversalReceiptSchema,
} from './receipt-schemas.mjs';

// Re-export builder
export {
  generateEpoch,
  generateReceiptId,
  computeReceiptHash,
  ReceiptBuilder,
} from './receipt-builder.mjs';

// Re-export factories
export {
  createAdmissionReceipt,
  createTestReceipt,
  createBuildReceipt,
  createDeploymentReceipt,
  createProjectionReceipt,
  createQueryReceipt,
  createWorkflowReceipt,
} from './receipt-factories.mjs';

// Re-export serialization
export {
  receiptToJSONLD,
  receiptToTurtle,
  receiptToBinary,
  receiptFromBinary,
} from './receipt-serialization.mjs';

// Re-export verification
export {
  verifyReceiptHash,
  validateReceipt,
} from './receipt-verification.mjs';
