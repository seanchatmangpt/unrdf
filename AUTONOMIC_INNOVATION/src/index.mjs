/**
 * @fileoverview AUTONOMIC_INNOVATION - Public API
 * Single import path for all innovations from Agents 2-10
 * @module autonomic-innovation
 */

import { getAgentExports, validateIntegration, getIntegrationStatus, preloadAgents } from '../agent-1/index.mjs';

// Pre-load all agents on import
await preloadAgents();

// Agent 2: Capsules
const agent2 = await getAgentExports('agent-2');
export const planCapsule = agent2.exports.planCapsule;
export const applyCapsule = agent2.exports.applyCapsule;
export const verifyCapsule = agent2.exports.verifyCapsule;
export const canonicalize = agent2.exports.canonicalize;
export const hashCapsule = agent2.exports.hashCapsule;

// Agent 3: Lenses
const agent3 = await getAgentExports('agent-3');
export const defineLens = agent3.exports.defineLens;
export const compileLens = agent3.exports.compileLens;
export const executeLensToGraph = agent3.exports.executeLensToGraph;
export const executeLensFromGraph = agent3.exports.executeLensFromGraph;

// Agent 4: Impact Sets
const agent4 = await getAgentExports('agent-4');
export const computeImpactSet = agent4.exports.computeImpactSet;

// Agent 5: Commutativity
const agent5 = await getAgentExports('agent-5');
export const canReorder = agent5.exports.canReorder;
export const conflictCertificate = agent5.exports.conflictCertificate;

// Agent 6: Conventions
const agent6 = await getAgentExports('agent-6');
export const compileProfile = agent6.exports.compileProfile;
export const validateAgainstProfile = agent6.exports.validateAgainstProfile;
export const diagnosticReport = agent6.exports.diagnosticReport;

// Agent 7: Generator
const agent7 = await getAgentExports('agent-7');
export const generateFacade = agent7.exports.generateFacade;

// Agent 8: Store
const agent8 = await getAgentExports('agent-8');
export const atomicApply = agent8.exports.atomicApply;
export const replayFromReceipt = agent8.exports.replayFromReceipt;

// Agent 9: Shadow
const agent9 = await getAgentExports('agent-9');
export const shadowWrite = agent9.exports.shadowWrite;
export const shadowRead = agent9.exports.shadowRead;
export const partialServe = agent9.exports.partialServe;
export const mismatchReport = agent9.exports.mismatchReport;

// Agent 10: Quality
const agent10 = await getAgentExports('agent-10');
export const runQualityGates = agent10.exports.runQualityGates;
export const e2eValidate = agent10.exports.e2eValidate;

// Integration utilities
export { validateIntegration, getIntegrationStatus };
