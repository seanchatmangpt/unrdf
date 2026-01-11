/**
 * @file Groth16 Prover/Verifier Tests
 * @module @unrdf/zkp/test/groth16
 * @description Tests for Groth16 zk-SNARK prover and verifier
 */

import { describe, it, expect } from 'vitest';
import {
  Groth16Prover,
  createGroth16Prover,
} from '../src/groth16-prover.mjs';
import {
  Groth16Verifier,
  createGroth16Verifier,
  verifyProof,
} from '../src/groth16-verifier.mjs';

describe('Groth16Prover', () => {
  describe('Constructor', () => {
    it('should create prover with config', () => {
      const prover = new Groth16Prover({
        wasmPath: './test.wasm',
        zkeyPath: './test.zkey',
      });

      expect(prover).toBeDefined();
      expect(prover.config.wasmPath).toBe('./test.wasm');
      expect(prover.config.zkeyPath).toBe('./test.zkey');
    });

    it('should create prover without config', () => {
      const prover = new Groth16Prover();

      expect(prover).toBeDefined();
      expect(prover.config.wasmPath).toBeNull();
      expect(prover.config.zkeyPath).toBeNull();
    });
  });

  describe('Proof Size Estimation', () => {
    it('should return constant proof size', () => {
      const prover = new Groth16Prover();

      const size = prover.estimateProofSize();

      expect(size).toBe(192);
    });

    it('should return same size for different circuits', () => {
      const prover1 = new Groth16Prover();
      const prover2 = new Groth16Prover();

      expect(prover1.estimateProofSize()).toBe(prover2.estimateProofSize());
    });
  });

  describe('Factory Functions', () => {
    it('should create prover with createGroth16Prover', () => {
      const prover = createGroth16Prover({
        wasmPath: './circuit.wasm',
        zkeyPath: './circuit.zkey',
      });

      expect(prover).toBeInstanceOf(Groth16Prover);
    });
  });
});

describe('Groth16Verifier', () => {
  describe('Constructor', () => {
    it('should create verifier with vkey path', () => {
      const verifier = new Groth16Verifier({
        vkeyPath: './vkey.json',
      });

      expect(verifier).toBeDefined();
      expect(verifier.config.vkeyPath).toBe('./vkey.json');
    });

    it('should create verifier with vkey object', () => {
      const vkey = {
        protocol: 'groth16',
        curve: 'bn128',
        nPublic: 4,
        vk_alpha_1: ['1', '2', '1'],
        vk_beta_2: [
          ['1', '2'],
          ['3', '4'],
          ['1', '0'],
        ],
        vk_gamma_2: [
          ['1', '2'],
          ['3', '4'],
          ['1', '0'],
        ],
        vk_delta_2: [
          ['1', '2'],
          ['3', '4'],
          ['1', '0'],
        ],
        vk_alphabeta_12: [
          ['1', '2'],
          ['3', '4'],
        ],
        IC: [
          ['1', '2', '1'],
          ['3', '4', '1'],
        ],
      };

      const verifier = new Groth16Verifier({ vkey });

      expect(verifier).toBeDefined();
      expect(verifier.config.vkey).toEqual(vkey);
    });
  });

  describe('Verification Time Estimation', () => {
    it('should return constant verification time', () => {
      const verifier = new Groth16Verifier();

      const timeMs = verifier.estimateVerificationTime();

      expect(timeMs).toBe(2);
    });
  });

  describe('Factory Functions', () => {
    it('should create verifier with createGroth16Verifier', () => {
      const verifier = createGroth16Verifier({
        vkeyPath: './vkey.json',
      });

      expect(verifier).toBeInstanceOf(Groth16Verifier);
    });
  });
});

describe('Groth16 Integration', () => {
  it('should have matching proof and verifier interfaces', () => {
    const prover = new Groth16Prover();
    const verifier = new Groth16Verifier();

    expect(prover.estimateProofSize()).toBe(192);
    expect(verifier.estimateVerificationTime()).toBe(2);
  });

  it('should support proof size in bytes', () => {
    const prover = new Groth16Prover();

    const sizeBytes = prover.estimateProofSize();

    expect(sizeBytes).toBeGreaterThan(0);
    expect(sizeBytes).toBeLessThan(300);
  });
});
