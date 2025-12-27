/**
 * @file KGC Markdown Main Entry Point
 * @module kgc-docs
 *
 * KGC Markdown parser and renderer for dynamic documentation with proof anchoring
 */

export {
  parseKGCMarkdown,
  parseFrontmatter,
  parseFencedBlock,
  parseAllFencedBlocks,
  buildAST,
} from './parser.mjs';

export {
  renderDiataxisView,
  renderTutorial,
  renderHowTo,
  renderReference,
  renderExplanation,
  generateProofAppendix,
} from './renderer.mjs';

export {
  createMerkleTree,
  generateProofTree,
  verifyProof,
  linkReceiptHash,
} from './proof.mjs';
